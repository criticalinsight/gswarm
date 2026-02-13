import gleam/io
import gleam/list
import gleam/int
import gleam/float
import gleam/dict
import gleamdb
import gleamdb/fact
import gleamdb/shared/types
import gswarm/strategy.{type Strategy}

/// Selects the best strategy based on win rate from the last N verified predictions.
pub fn best_strategy(db: gleamdb.Db) -> #(String, Strategy) {
  // 1. Query verified prediction results
  let query = [
    types.Positive(#(types.Var("p"), "prediction/status", types.Val(fact.Str("verified")))),
    types.Positive(#(types.Var("p"), "prediction/strategy", types.Var("strat"))),
    types.Positive(#(types.Var("p"), "prediction/result", types.Var("res")))
  ]
  
  // Limiting to recent history would require sorting by timestamp, which we can add later.
  // For now, we aggregate all history.
  let results = gleamdb.query(db, query)
  io.println("StrategySelector: Found " <> int.to_string(list.length(results)) <> " verification records.")
  
  // 2. Group by strategy and calculate stats
  let stats = list.fold(results, dict.new(), fn(acc, row) {
    let strat_id = case dict.get(row, "strat") {
      Ok(fact.Str(s)) -> s
      _ -> "unknown"
    }
    
    let is_correct = case dict.get(row, "res") {
      Ok(fact.Str("correct")) -> True
      _ -> False
    }
    
    let current_stats = case dict.get(acc, strat_id) {
      Ok(#(wins, total)) -> #(wins, total)
      Error(_) -> #(0, 0)
    }
    
    let new_stats = case is_correct {
      True -> #(current_stats.0 + 1, current_stats.1 + 1)
      False -> #(current_stats.0, current_stats.1 + 1)
    }
    
    dict.insert(acc, strat_id, new_stats)
  })
  
  // 3. Find winner
  let best = dict.fold(stats, #("mean_reversion", -1.0), fn(current_best, strat_id, s) {
    let #(wins, total) = s
    let win_rate = int.to_float(wins) /. int.to_float(total)
    
    // Minimum 5 samples to qualify
    case total >= 5 {
      True -> {
        case win_rate >. current_best.1 {
          True -> #(strat_id, win_rate)
          False -> current_best
        }
      }
      False -> current_best
    }
  })
  
  io.println("🧠 StrategySelector: Winner is " <> best.0 <> " (Win Rate: " <> float.to_string(best.1) <> ")")
  
  #(best.0, strategy.from_string(best.0))
}
