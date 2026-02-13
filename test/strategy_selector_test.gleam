import gleeunit/should
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gleam/option.{None}
import gleam/int
import gswarm/strategy_selector

pub fn main() {
  selector_logic_test()
}

pub fn selector_logic_test() {
  let cluster_id = "selector_test_cluster"
  // Start ephemeral DB
  let assert Ok(db) = gleamdb.start_distributed(cluster_id, None)

  // Define Schema
  let unique_config = fact.AttributeConfig(unique: True, component: False, retention: fact.All)
  let normal_config = fact.AttributeConfig(unique: False, component: False, retention: fact.All)
  
  let _ = gleamdb.set_schema(db, "prediction/id", unique_config)
  let _ = gleamdb.set_schema(db, "prediction/strategy", normal_config)
  let _ = gleamdb.set_schema(db, "prediction/status", normal_config)
  let _ = gleamdb.set_schema(db, "prediction/result", normal_config)
  // Total 10 interactions
  seed_results(db, "mean_reversion", 4, 6)

  // 2. Seed "trend_follower" with 8 wins, 2 losses (80% win rate)
  // Total 10 interactions
  seed_results(db, "trend_follower", 8, 2)

  // 3. Seed "sentiment_momentum" with 1 win, 0 losses (100% win rate but low sample)
  // Total 1 interaction -> Should be ignored by min sample filter (>= 5)
  seed_results(db, "sentiment_momentum", 1, 0)

  // Test
  let best = strategy_selector.best_strategy(db)
  let best_name = best.0
  
  best_name |> should.equal("trend_follower")
  
  // Since we can't easily check function equality and to_string is a placeholder "unknown",
  // we actually need to fix strategy.to_string to return the real names for this test to pass!
  // BUT: strategy_selector Logged the winner.
  // Let's rely on the fact that if we fix to_string this works.
  
  // IMPORTANT: We need to fix strategy.to_string in src for this to be verifiable!
  
  process.sleep(100)
  let pid = process.subject_owner(db) |> result_unwrap_panic
  process.unlink(pid)
  process.kill(pid)
}

fn seed_results(db: gleamdb.Db, strat: String, wins: Int, losses: Int) {
  // Write wins
  int.range(from: 1, to: wins, with: Nil, run: fn(_, i) {
    let id = "mock_win_" <> strat <> "_" <> int.to_string(i)
    // Use large offset to avoid collisions between strategies if i resets
    // Or just hash the string if possible.
    // For test, just use sequential IDs with strategy offset.
    let strat_offset = case strat {
      "mean_reversion" -> 1000
      "trend_follower" -> 2000
      _ -> 3000
    }
    let entity_id = strat_offset + i
    let entity = fact.Uid(fact.EntityId(entity_id))
    
    let _ = gleamdb.transact(db, [
      #(entity, "prediction/id", fact.Str(id)),
      #(entity, "prediction/strategy", fact.Str(strat)),
      #(entity, "prediction/status", fact.Str("verified")),
      #(entity, "prediction/result", fact.Str("correct"))
    ])
    Nil
  })

  // Write losses
  int.range(from: 1, to: losses, with: Nil, run: fn(_, i) {
    let id = "mock_loss_" <> strat <> "_" <> int.to_string(i)
    let strat_offset = case strat {
      "mean_reversion" -> 1000
      "trend_follower" -> 2000
      _ -> 3000
    }
    let entity_id = strat_offset + 500 + i
    let entity = fact.Uid(fact.EntityId(entity_id))
    
    let _ = gleamdb.transact(db, [
      #(entity, "prediction/id", fact.Str(id)),
      #(entity, "prediction/strategy", fact.Str(strat)),
      #(entity, "prediction/status", fact.Str("verified")),
      #(entity, "prediction/result", fact.Str("incorrect"))
    ])
    Nil
  })
}

fn result_unwrap_panic(res) {
  case res {
    Ok(x) -> x
    Error(_) -> panic
  }
}
