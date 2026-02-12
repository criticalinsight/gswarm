import gleam/io
import gleam/erlang/process
import gleamdb
import gswarm/fabric
import gswarm/node
import gswarm/market
import gswarm/ticker
import gswarm/reflex
import gswarm/observer
import gswarm/analytics
import gswarm/context

pub fn main() {
  io.println("🐝 Gswarm: Initializing Sovereign Fabric...")
  
  // Default to Leader for now (Local-First Dev Mode)
  let cluster_id = "gswarm_mainnet"
  
  case fabric.join_fabric(node.Leader, cluster_id) {
    Ok(ctx) -> {
      io.println("👑 Node started as LEADER of " <> cluster_id)
      
      // Simulate High-Frequency Ticker
      io.println("⚡️ Starting Silicon Saturation Ticker...")
      let market = market.Market("m_1", "Will GleamDB reach 1M ops/sec?", ["Yes", "No"])
      let market2 = market.Market("m_2", "Will Gswarm take over the world?", ["Yes", "No"])
      
      // Increased timeout resilience for first transaction
      case market.create_market(ctx.db, market) {
        Ok(_) -> {
          let assert Ok(_) = market.create_market(ctx.db, market2)
          
          // Index semantics for Vector Sovereignty
          let assert Ok(_) = context.index_market_semantics(ctx.db, "m_1", market.question)
          let assert Ok(_) = context.index_market_semantics(ctx.db, "m_2", market2.question)
          
          // Apply Memory Safety Retention Policies
          market.configure_tick_retention(ctx.db)
          
          ticker.start_ticker(ctx.db, "m_1")
          ticker.start_ticker(ctx.db, "m_2")
          reflex.spawn_market_watcher(ctx.db, "m_1")
          reflex.spawn_market_watcher(ctx.db, "m_2")
          reflex.spawn_multi_market_watcher(ctx.db, "m_1", "m_2")
          
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
  analytics.audit_history(db, "m_1", 1)
  
  io.println("🧠 Context Pulse: Searching for AI contexts...")
  context.find_similar_markets(db, "AI and GleamDB performance")
  
  periodic_pulses(db)
}
