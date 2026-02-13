import gleam/io
import argv
import gleam/list
import gleam/erlang/process
import gleamdb
import gswarm/fabric
import gswarm/node
import gswarm/market
import gswarm/reflex
import gswarm/observer
import gswarm/context
import gswarm/supervisor
import gswarm/live_ticker
import gswarm/analyst
import gswarm/news_feed
import gswarm/paper_trader
import gswarm/correlator
import gswarm/result_fact
import gswarm/strategy_selector
import gswarm/market_feed
import gswarm/resolution
import gswarm/cross_market
import gleam/option.{None, Some}


pub fn main() {
  io.println("🐝 Gswarm: Initializing Sovereign Fabric...")
  
  // Arguments: ./gswarm [role] [cluster_id]
  // Default: Leader, gswarm_mainnet
  // Default: Leader, gswarm_mainnet
  let arguments = argv.load().arguments
  let #(role, cluster_id) = case arguments {
    [r, c, ..] -> #(parse_role(r), c)
    [r] -> #(parse_role(r), "gswarm_mainnet")
    [] -> #(node.Leader, "gswarm_mainnet")
  }
  
  case fabric.join_fabric(role, cluster_id) {
    Ok(ctx) -> {
      io.println("👑 Node started as " <> role_to_string(role) <> " of " <> cluster_id)
      
      // Simulate High-Frequency Ticker
      io.println("⚡️ Starting Silicon Saturation Ticker...")
      let market = market.Market("m_1", "Will GleamDB reach 1M ops/sec?", ["Yes", "No"],
        market.Binary, market.Open, 0, "internal")
      let market2 = market.Market("m_2", "Will Gswarm take over the world?", ["Yes", "No"],
        market.Binary, market.Open, 0, "internal")
      
      // Increased timeout resilience for first transaction
      case market.create_market(ctx.db, market) {
        Ok(_) -> {
          let assert Ok(_) = market.create_market(ctx.db, market2)
          
          // Index semantics for Vector Sovereignty
          let assert Ok(_) = context.index_market_semantics(ctx.db, "m_1", market.question)
          let assert Ok(_) = context.index_market_semantics(ctx.db, "m_2", market2.question)
          
          // Apply Memory Safety Retention Policies
          market.configure_tick_retention(ctx.db)
          
          // Start Supervision Tree 🛡️
          let assert Ok(_) = supervisor.start(ctx.db)

          // Live Feeds Only 📡
          io.println("⚡️ Starting Live Feed Swarm (BTC + Prediction Markets)...")
          
          // Paper Trader 📈
          let assert Ok(btc_trader) = paper_trader.start_paper_trader(ctx.db, "m_btc", 10000.0)
          
          live_ticker.start_live_ticker(ctx.db, "m_btc", "BTC-USD", Some(btc_trader))
          live_ticker.start_live_ticker(ctx.db, "m_btc", "BTC-USD", Some(btc_trader))

          // No synthetic tickers!
          
          reflex.spawn_market_watcher(ctx.db, "m_btc")
          reflex.spawn_market_watcher(ctx.db, "m_btc")
                    // AI Analyst 🧠 (now with reinforcement learning)
           analyst.start_analyst(ctx.db, "m_btc")

           // News Feed 📰
           news_feed.start_news_feed(ctx.db)

           // Signal Correlator 📊 (Phase 28)
           correlator.start_correlator(ctx.db)

           // Result Fact Checker 🎯 (Reinforcement Learning)
           result_fact.start_result_checker(ctx.db)

           // Adaptive Strategy Selector 🧬 (Phase 29)
           strategy_selector.start_selector(ctx.db, btc_trader)

           // === Prediction Markets Intelligence (Phases 34–38) ===
           io.println("🎲 Starting Prediction Markets Intelligence...")

           // Manifold Markets Feed 📊 (Phase 35)
           // Track real prediction markets — replace slugs with actual Manifold market IDs
           let prediction_market_ids = ["claude-5-released-before-march-31", "will-the-us-strike-iran-by-the-end"]
           // Enable full paper trading on prediction markets
           market_feed.start_market_feed(ctx.db, prediction_market_ids, Some(btc_trader))
           
           // Spawn Reflex watchers for rigor/visibility
           list.each(prediction_market_ids, fn(id) {
             reflex.spawn_prediction_watcher(ctx.db, "pm_" <> id)
           })

           // Probability Analyst 🔮 (Phase 36)
           // Analyst already handles prediction markets via pm_ prefix
           analyst.start_analyst(ctx.db, "pm_will-ai-pass-the-turing-test-by-2027")

           // Resolution Checker 🏁 (Phase 37)
           resolution.start_resolution_checker(ctx.db, prediction_market_ids)

           // Cross-Market Intelligence 🔗 (Phase 38)
           // Cross-Market Intelligence 🔗 (Phase 38)
           let assert Ok(_) = cross_market.start_link(ctx.db)
          
          process.spawn_unlinked(fn() {
            periodic_pulses(ctx.db)
          })
          
          process.sleep_forever()
        }
        Error(e) -> {
          io.println("❌ Failed to create market: " <> e)
        }
      }
    }
    Error(e) -> {
      io.println("❌ Failed to start node: " <> e)
    }
  }
}

fn periodic_pulses(db: gleamdb.Db) {
  process.sleep(5000)
  observer.pulse_market(db, "m_1")
  
  io.println("🧠 Context Pulse: Searching for AI contexts...")
  context.find_similar_markets(db, "AI and GleamDB performance")
  

}

fn parse_role(s: String) -> node.NodeRole {
  case s {
    "follower" -> node.Follower
    "ephemeral" -> node.LeaderEphemeral
    _ -> node.Leader
  }
}

fn role_to_string(r: node.NodeRole) -> String {
  case r {
    node.Leader -> "LEADER"
    node.Follower -> "FOLLOWER"
    node.LeaderEphemeral -> "EPHEMERAL LEADER"
  }
}
