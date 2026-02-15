import gleam/io
import gleam/int
import gleam/option.{None, Some}
import gleeunit/should
import gleamdb
import gleamdb/storage/mnesia
import gswarm/strategy_selector
import gleamdb/fact

import gleam/erlang/process

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

pub fn main() {
  switching_logic_test()
}

pub fn switching_logic_test() {
  io.println("🧪 StrategySwitchingTest: Initializing...")
  
  // 1. Setup DB with unique ID to avoid stale data from previous runs
  let cluster_id = "strategy_test_" <> int.to_string(erlang_system_time())
  let db = case gleamdb.start_distributed(cluster_id, Some(mnesia.adapter())) {
    Ok(d) -> d
    Error(_) -> {
      let assert Ok(d) = gleamdb.connect(cluster_id)
      d
    }
  }
  
  // 2. Define Schema to ensure indexing/querying works
  let unique_config = fact.AttributeConfig(unique: True, component: False, retention: fact.All, cardinality: fact.Many, check: None)
  let normal_config = fact.AttributeConfig(unique: False, component: False, retention: fact.All, cardinality: fact.Many, check: None)
  
  let _ = gleamdb.set_schema(db, "prediction/id", unique_config)
  let _ = gleamdb.set_schema(db, "prediction/strategy", normal_config)
  let _ = gleamdb.set_schema(db, "prediction/status", normal_config)
  let _ = gleamdb.set_schema(db, "prediction/result", normal_config)

  // 3. Seed Data
  io.println("🌱 StrategySwitchingTest: Seeding result facts...")
  seed_results(db, "trend_follower", 8, 2)
  seed_results(db, "mean_reversion", 2, 8)
  
  process.sleep(500) // Allow Mnesia to commit/index
  
  // 4. Verify Selector
  let #(best_id, _) = strategy_selector.best_strategy(db)
  
  io.println("🧠 StrategySwitchingTest: Selected " <> best_id)
  
  best_id |> should.equal("trend_follower")
  
  // 5. Shift Regime: Tank Trend Follower performance
  io.println("🌊 StrategySwitchingTest: Shifting regime (Trend Follower starts losing)...")
  // Add 100 losses to Trend Follower. 
  
  seed_results_offset(db, "trend_follower", 0, 100, 5000)
  
  process.sleep(200)

  let #(new_best_id, _strat) = strategy_selector.best_strategy(db)
  
  io.println("🧠 StrategySwitchingTest: Selected " <> new_best_id)
  
  new_best_id |> should.equal("mean_reversion")
}

fn seed_results(db: gleamdb.Db, strategy_id: String, wins: Int, losses: Int) {
  seed_results_offset(db, strategy_id, wins, losses, 0)
}

fn seed_results_offset(db: gleamdb.Db, strategy_id: String, wins: Int, losses: Int, start_offset: Int) {
  // We need to simulate the result_fact.record_prediction flow but manually writing the authorized fact state
  // Or utilize result_fact helper? 
  // result_fact.record_prediction records "pending".
  // Then the checker updates to "correct"/"incorrect".
  // We can shortcut and write the final state directly using gleamdb.transact if we know the schema.
  
  // Schema from result_fact.gleam:
  // prediction/id -> unique
  // prediction/strategy -> ID
  // prediction/status -> "verified"
  // prediction/result -> "correct" | "incorrect"
  
  // Wins
  int.range(from: 1, to: wins + 1, with: Nil, run: fn(_, i) {
    record_fact(db, strategy_id, "correct", start_offset + i)
    Nil
  })
  
  // Losses
  int.range(from: 1, to: losses + 1, with: Nil, run: fn(_, i) {
    record_fact(db, strategy_id, "incorrect", start_offset + 1000 + i)
    Nil
  })
}

fn record_fact(db: gleamdb.Db, strategy_id: String, result: String, i: Int) {
   let ts = 1234567890 + i
   let id = "test_pred_" <> strategy_id <> "_" <> int.to_string(ts) <> "_" <> result
   
   // Use explicit Entity ID to ensure write succeeds without index lookup dependency
   // Offset IDs based on strategy and timestamp to be unique
   let offset = case strategy_id {
      "trend_follower" -> 100000
      "mean_reversion" -> 200000
      _ -> 300000
   }
   let entity_id = offset + i
   let entity = fact.Uid(fact.EntityId(entity_id))
   
   let facts = [
     #(entity, "prediction/id", fact.Str(id)),
     #(entity, "prediction/strategy", fact.Str(strategy_id)),
     #(entity, "prediction/status", fact.Str("verified")),
     #(entity, "prediction/result", fact.Str(result))
   ]
   
   case gleamdb.transact(db, facts) {
      Ok(_) -> Nil
      Error(_e) -> io.println("❌ Failed to seed fact: " <> id)
   }
}
