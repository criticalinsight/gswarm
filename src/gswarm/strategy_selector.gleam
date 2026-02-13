import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/dict
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gleamdb/shared/types
import gswarm/strategy
import gswarm/paper_trader

/// Adaptive Strategy Selection.
/// Queries historical prediction accuracy over a rolling window,
/// selects the strategy with the highest win rate, and hot-swaps
/// the paper trader's active strategy.
///
/// Rich Hickey: "Information. Not process. The data tells you which strategy
/// to trust — you don't hard-code the answer."

pub type StrategyRecord {
  StrategyRecord(name: String, apply: strategy.Strategy)
}

/// All available strategies with their names (for scoring)
pub fn available_strategies() -> List(StrategyRecord) {
  [
    StrategyRecord("mean_reversion", strategy.mean_reversion),
    StrategyRecord("trend_follower", strategy.trend_follower),
    StrategyRecord("cross_signal", strategy.cross_signal),
    StrategyRecord("sentiment_momentum", strategy.sentiment_momentum),
  ]
}

/// Start the adaptive selector loop.
/// It evaluates strategy performance every 120s and hot-swaps the paper trader.
pub fn start_selector(
  db: gleamdb.Db,
  trader: process.Subject(paper_trader.Message)
) {
  process.spawn_unlinked(fn() {
    io.println("🧬 StrategySelector: Started. Evaluating every 120s...")
    selector_loop(db, trader, "mean_reversion")
  })
}

fn selector_loop(
  db: gleamdb.Db,
  trader: process.Subject(paper_trader.Message),
  current_name: String
) {
  process.sleep(120_000)

  // Query all result facts to compute per-strategy accuracy
  let query = [
    types.Positive(#(types.Var("r"), "prediction/direction", types.Var("dir"))),
    types.Positive(#(types.Var("r"), "prediction/status", types.Var("status"))),
    types.Positive(#(types.Var("r"), "prediction/strategy", types.Var("strat_name")))
  ]

  let results = gleamdb.query(db, query)

  case list.length(results) >= 10 {
    True -> {
      // Score each strategy
      let best = score_strategies(results)
      case best {
        Ok(#(name, accuracy)) -> {
          case name != current_name {
            True -> {
              io.println("🧬 StrategySelector: SWITCHING → " <> name
                <> " (accuracy: " <> float.to_string(accuracy) <> "%)")

              // Find the strategy function and hot-swap
              case find_strategy(name) {
                Ok(strat) -> {
                  process.send(trader, paper_trader.SetStrategy(strat.apply))
                  selector_loop(db, trader, name)
                }
                Error(_) -> selector_loop(db, trader, current_name)
              }
            }
            False -> {
              io.println("🧬 StrategySelector: Keeping " <> current_name
                <> " (accuracy: " <> float.to_string(accuracy) <> "%)")
              selector_loop(db, trader, current_name)
            }
          }
        }
        Error(_) -> {
          io.println("🧬 StrategySelector: Not enough scored data yet.")
          selector_loop(db, trader, current_name)
        }
      }
    }
    False -> {
      io.println("🧬 StrategySelector: Need ≥10 result facts ("
        <> int.to_string(list.length(results)) <> " found). Waiting...")
      selector_loop(db, trader, current_name)
    }
  }
}

/// Score strategies by grouping result facts and computing win rates.
/// Returns the name and accuracy of the best strategy.
fn score_strategies(results: types.QueryResult) -> Result(#(String, Float), Nil) {
  // Group by strategy name: Dict(name, #(correct, total))
  let scores = list.fold(results, dict.new(), fn(acc, row) {
    let name = case dict.get(row, "strat_name") {
      Ok(fact.Str(n)) -> n
      _ -> "unknown"
    }
    let is_correct = case dict.get(row, "status") {
      Ok(fact.Str("correct")) -> True
      _ -> False
    }

    let current = case dict.get(acc, name) {
      Ok(#(c, t)) -> #(c, t)
      _ -> #(0, 0)
    }

    let updated = case is_correct {
      True -> #(current.0 + 1, current.1 + 1)
      False -> #(current.0, current.1 + 1)
    }

    dict.insert(acc, name, updated)
  })

  // Find the strategy with highest accuracy (min 5 results to be eligible)
  let candidates = dict.to_list(scores)
    |> list.filter(fn(entry) { entry.1.1 >= 5 })
    |> list.map(fn(entry) {
      let #(name, #(correct, total)) = entry
      let accuracy = int.to_float(correct) /. int.to_float(total) *. 100.0
      #(name, accuracy)
    })
    |> list.sort(fn(a, b) { float.compare(b.1, a.1) })

  list.first(candidates)
}

/// Find a strategy record by name
fn find_strategy(name: String) -> Result(StrategyRecord, Nil) {
  available_strategies()
  |> list.find(fn(s) { s.name == name })
}
