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

import gswarm/cross_market
import gswarm/shard_manager
import gswarm/ingest_batcher
import gswarm/pruner
import gleam/int
import gleam/option.{Some}
import gswarm/http
import gswarm/leaderboard

import gswarm/activity_feed
import gswarm/insider_store
import gswarm/graph_intel

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

      // Phase 30: Register market data constraints (v2.0.0)
      let _ = graph_intel.register_market_constraints(ctx)

      // News Feed (Global)
      news_feed.start_news_feed(primary_db)
      correlator.start_correlator(primary_db)
      result_fact.start_result_checker(primary_db)

      // 2. Start Insider Store (Phase 49)
      let assert Ok(insider_actor) = insider_store.start(primary_db)

      // Start Batchers and Pruners for all shards
      let batchers = list.map(dict.to_list(ctx.db.shards), fn(pair) {
        let #(id, db) = pair
        let assert Ok(batcher) = ingest_batcher.start(db, ctx.registry_actor, insider_actor)
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

      // --- PHASE 50: Advanced Amkabot Integration ---
      
      // 1. Start Leaderboard Actor (GleamDB Sovereign Fabric 🧙🏾‍♂️)
      let assert Ok(started) = leaderboard.start(primary_db)
      let lb_actor = started.data
      io.println("🏆 Leaderboard Active (GleamDB Sovereign Fabric 🧙🏾‍♂️)")

      // 3. PolyMarket Feed
      // Dynamically discover active markets to avoid 404s
      let polymarket_ids = case market_feed.fetch_active_tokens() {
        Ok(ids) if ids != [] -> ids
        _ -> {
          io.println("⚠️ PolyMarket: Discovery failed or empty, using fallback.")
          ["101676997363687199724245607342877036148401850938023978421879460310389391082353"] // Live token ID
        }
      }
      io.println("🔵 PolyMarket: Discovered " <> int.to_string(list.length(polymarket_ids)) <> " active tokens.")
      
      // We pick a shard for the feed (e.g. Shard 0)
      let assert Ok(pm_batcher) = dict.get(batchers, 0)
      
      market_feed.start_polymarket_feed(pm_batcher, polymarket_ids, Some(btc_trader))

      // 4. Activity Feed (Leaderboard)
      // Discover top traders to track
      let active_users = case activity_feed.fetch_leaderboard() {
        Ok(users) -> users
        Error(e) -> {
          io.println("⚠️ ActivityFeed: Discovery failed (" <> e <> "), using fallback.")
          ["0x4bFb41d5B3570DeFd03C39a9A4D8D6D04C96E631"]
        }
      }
      io.println("🕵️ ActivityFeed: Tracking " <> int.to_string(list.length(active_users)) <> " traders.")
      activity_feed.start_with_dedup(lb_actor, active_users)


      // Cross-Market Intel (Scatter-Gather across shards)
      let assert Ok(_) = cross_market.start_link(ctx)
      
      // Phase 46: Operability Dashboard
      io.println("📊 Starting Metrics Dashboard on port 8085...")
      http.start_server(8085, ctx, lb_actor, insider_actor)
      
      // Phase 44: Probabilistic Metrics Monitor
      process.spawn(fn() {
        loop_flush(lb_actor)
      })

      // Phase 32: Start periodic Graph Intelligence scans (v2.0.0)
      graph_intel.start_periodic_scan(ctx, 300_000)

      process.sleep_forever()
    }
    Error(e) -> {
      io.println("❌ Failed to join fabric: " <> e)
    }
  }
}

fn loop_flush(lb_actor) {
  process.sleep(60_000)
  process.send(lb_actor, leaderboard.Flush)
  loop_flush(lb_actor)
}

fn parse_role(r: String) -> node.NodeRole {
  case r {
    "leader" -> node.Leader
    "follower" -> node.Follower
    _ -> node.Leader
  }
}

fn role_to_string(r: node.NodeRole) -> String {
  case r {
    node.Leader -> "LEADER"
    node.Follower -> "FOLLOWER"
    node.Lean -> "LEAN NODE (RESTRICTED)"
    node.LeaderEphemeral -> "LEADER (EPHEMERAL)"
  }
}
