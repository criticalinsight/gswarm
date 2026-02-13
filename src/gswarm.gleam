import gleam/io
import argv
import gleam/list
import gleam/dict
import gleam/erlang/process
import gswarm/fabric
import gswarm/node
import gswarm/reflex
import gswarm/supervisor
import gswarm/live_ticker
import gswarm/analyst
import gswarm/news_feed
import gswarm/paper_trader
import gswarm/correlator
import gswarm/result_fact
import gswarm/market_feed
import gswarm/resolution
import gswarm/cross_market
import gswarm/shard_manager
import gswarm/ingest_batcher
import gswarm/pruner
import gleam/int
import gleam/result
import gleam/option.{Some}
import gswarm/registry_actor
import gswarm/http
import gswarm/strategy_selector
import gleamdb

pub fn main() {
  io.println("🐝 Gswarm: Initializing Sharded Sovereign Fabric...")
  
  let arguments = argv.load().arguments
  let is_lean = list.contains(arguments, "--lean")
  
  let #(role, cluster_id) = case arguments {
    [r, c, ..] if r != "--lean" -> #(parse_role(r), c)
    [r] if r != "--lean" -> #(parse_role(r), "gswarm_mainnet")
    _ -> #(case is_lean { True -> node.Lean False -> node.Leader }, "gswarm_mainnet")
  }

  // Phase 41: Shard collapsing for Lean mode
  let shard_count = case is_lean {
    True -> 1
    False -> 4
  }
  
  case fabric.join_sharded_fabric(role, cluster_id, shard_count) {
    Ok(ctx) -> {
      io.println("👑 Sharded Fabric started with " <> role_to_string(role) <> " shards: " <> cluster_id)
      
      // Select Primary Shard (Shard 0) for global components
      let assert Ok(primary_db) = dict.get(ctx.db.shards, 0)

      // Start Supervision Tree on primary
      let assert Ok(_) = supervisor.start(primary_db)

      // News Feed (Global)
      news_feed.start_news_feed(primary_db)
      correlator.start_correlator(primary_db)
      result_fact.start_result_checker(primary_db)

      // Start Batchers and Pruners for all shards
      let batchers = list.map(dict.to_list(ctx.db.shards), fn(pair) {
        let #(id, db) = pair
        let assert Ok(batcher) = ingest_batcher.start(db, ctx.registry_actor)
        // Phase 45: Prune old ticks after 1 hour, check every 30 seconds
        let assert Ok(_) = pruner.start(db, 3_600_000, 30_000, "tick/timestamp")
        #(id, batcher)
      }) |> dict.from_list

      // Initalize BTC-USD on its respective shard
      let btc_market_id = "m_btc"
      let btc_shard_id = shard_manager.get_shard_id(btc_market_id, shard_count)
      let assert Ok(btc_db) = dict.get(ctx.db.shards, btc_shard_id)
      let assert Ok(btc_batcher) = dict.get(batchers, btc_shard_id)
      
      io.println("📡 Tracking " <> btc_market_id <> " on Shard " <> int.to_string(btc_shard_id))
      
      let assert Ok(btc_trader) = paper_trader.start_paper_trader(btc_db, btc_market_id, 10000.0)
      live_ticker.start_live_ticker(btc_batcher, btc_market_id, "BTC-USD", Some(btc_trader))
      reflex.spawn_market_watcher(btc_db, btc_market_id)
      analyst.start_analyst(btc_db, btc_market_id)

      // Prediction Markets
      let prediction_market_ids = ["claude-5-released-before-march-31", "will-the-us-strike-iran-by-the-end"]
      
      list.each(prediction_market_ids, fn(id) {
        let pm_id = "pm_" <> id
        let pm_shard_id = shard_manager.get_shard_id(pm_id, shard_count)
        let assert Ok(pm_db) = dict.get(ctx.db.shards, pm_shard_id)
        let assert Ok(pm_batcher) = dict.get(batchers, pm_shard_id)
        
        io.println("🎲 Tracking " <> pm_id <> " on Shard " <> int.to_string(pm_shard_id))
        
        // Note: paper_trader currently single instance per market-set or we shard it too
        // For now, prediction markets share the same trader if desired or get their own
        // Let's give them their own or reuse btc_trader if it's "the" trader
        market_feed.start_market_feed(pm_batcher, [id], Some(btc_trader))
        reflex.spawn_prediction_watcher(pm_db, pm_id)
        resolution.start_resolution_checker(pm_db, [id])
      })

      // Cross-Market Intel (Scatter-Gather across shards)
      let assert Ok(_) = cross_market.start_link(ctx)
      
      // Phase 46: Operability Dashboard
      io.println("📊 Starting Metrics Dashboard on port 8085...")
      http.start_server(8085, ctx)
      
      // Phase 44: Probabilistic Metrics Monitor
      process.spawn(fn() {
        let monitor_freq = 5000 // 5s
        let card_reply = process.new_subject()
        let freq_reply = process.new_subject()
        
        list.each(list.repeat(Nil, 1000), fn(_) {
          process.sleep(monitor_freq)
          process.send(ctx.registry_actor, registry_actor.GetCardinality(card_reply))
          process.send(ctx.registry_actor, registry_actor.GetFrequency(btc_market_id, freq_reply))
          
          let card = process.receive(card_reply, 100) |> result.unwrap(0)
          let freq = process.receive(freq_reply, 100) |> result.unwrap(0)
          
          io.println("🔮 Probabilistic Intel | Unique Markets (HLL): ~" <> int.to_string(card) <> " | BTC Activity (CMS): " <> int.to_string(freq))
        })
      })

      // Phase 47: Adaptive Strategy Selection Loop
      process.spawn(fn() {
        let loop_freq = 10_000 // 10s
        strategy_loop(primary_db, btc_trader, loop_freq)
      })
      
      process.sleep_forever()
    }
    Error(e) -> {
      io.println("❌ Failed to start sharded fabric: " <> e)
    }
  }
}

fn strategy_loop(db: gleamdb.Db, trader: process.Subject(paper_trader.Message), freq: Int) {
  process.sleep(freq)

  let #(best_id, best_strat) = strategy_selector.best_strategy(db)
  
  // Send update to trader (it handles deduplication if same strategy)
  process.send(trader, paper_trader.SetStrategy(best_strat, best_id))
  
  strategy_loop(db, trader, freq)
}

fn parse_role(s: String) -> node.NodeRole {
  case s {
    "follower" -> node.Follower
    "ephemeral" -> node.LeaderEphemeral
    "lean" -> node.Lean
    _ -> node.Leader
  }
}

fn role_to_string(r: node.NodeRole) -> String {
  case r {
    node.Leader -> "LEADER"
    node.Follower -> "FOLLOWER"
    node.LeaderEphemeral -> "EPHEMERAL LEADER"
    node.Lean -> "LEAN NODE (RESTRICTED)"
  }
}
