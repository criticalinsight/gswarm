import gleam/erlang/process
import gleam/string
import gleam/io
import gleam/list
import gleam/int
import gleamdb
import gleamdb/fact
import gleamdb/shared/types.{Delta, Val, Var}
import gswarm/node
import gswarm/fabric
import gswarm/market
import gswarm/ticker

pub fn main() -> Nil {
  high_concurrency_stress_test()
}

pub fn high_concurrency_stress_test() {
  io.println("🚀 Initiating DURABLE Baseline Benchmark (2500 events/sec)...")
  let cluster_id = "gswarm_durable_baseline"
  
  // Start Durable Leader
  let assert Ok(ctx) = node.start(node.Leader, cluster_id)
  
  // 5 tickers * 10 batches/sec * 50 ticks/batch = 2,500 events/sec
  list.range(1, 5)
  |> list.each(fn(i) {
    let m_id = "durable_m_" <> int.to_string(i)
    let m = market.Market(m_id, "Baseline?", ["Yes"])
    let assert Ok(_) = market.create_market(ctx.db, m)
    
    ticker.start_high_load_ticker(ctx.db, m_id, 50, 100)
  })
  
  io.println("  - Benchmark running. Monitoring for 20 seconds...")
  process.sleep(20000)
  
  io.println("✅ Durable benchmark completed.")
  node.stop(ctx)
}

pub fn validation_test() {
  let cluster_id = "gswarm_test_validation"
  let assert Ok(ctx) = node.start(node.Leader, cluster_id)
  let tick = market.Tick("m1", "Yes", -0.5, 100, 1)
  let res = market.ingest_tick(ctx.db, tick)
  case res {
    Error(e) -> {
      let assert True = string.contains(e, "Negative price")
    }
    _ -> panic as "Validation failed"
  }
  node.stop(ctx)
}

pub fn failover_promotion_test() {
  let cluster_id = "gswarm_test_failover"
  let assert Ok(leader_ctx) = node.start(node.Leader, cluster_id)
  process.sleep(200)
  let assert Ok(follower_ctx) = fabric.join_fabric(node.Follower, cluster_id)
  let assert Ok(leader_pid) = process.subject_owner(leader_ctx.db)
  process.kill(leader_pid)
  process.sleep(600)
  let m = market.Market("promoted_m", "Works?", ["Yes"])
  let assert Ok(_) = market.create_market(follower_ctx.db, m)
  node.stop(follower_ctx)
}
