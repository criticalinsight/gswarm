import gleam/io
import gleam/list
import gleam/int
import gleam/dict
import gleamdb
import gleamdb/fact
import gleamdb/shared/types
import gswarm/strategy.{Buy, Sell}

pub type BacktestResult {
  BacktestResult(
    total_trades: Int,
    starting_balance: Float,
    final_balance: Float,
    percent_return: Float,
    win_rate: Float
  )
}

pub fn run_backtest(
  db: gleamdb.Db,
  market_id: String,
  strat: strategy.Strategy,
  initial_balance: Float
) -> BacktestResult {
  // 1. Fetch historical Ticks and their Vectors via Query
  let query = [
    types.Positive(#(types.Val(fact.Str(market_id)), "market/id", types.Var("m"))),
    types.Positive(#(types.Var("m"), "tick/price/Yes", types.Var("price"))),
    types.Positive(#(types.Var("m"), "tick/vector", types.Var("vector")))
  ]
  
  let results = gleamdb.query(db, query)
  let timeline = process_query_results(results)
  
  case timeline {
    [] -> {
      io.println("⚠️ Backtest: No historical data found for " <> market_id)
      BacktestResult(0, initial_balance, initial_balance, 0.0, 0.0)
    }
    _ -> execute_replay(timeline, strat, initial_balance)
  }
}

fn execute_replay(
  timeline: List(#(Float, List(Float))),
  strat: strategy.Strategy,
  initial_balance: Float
) -> BacktestResult {
  let initial_state = #(initial_balance, 0.0, 0, 0) // #(Balance, Position, Trades, Wins)
  
  let #(final_balance, final_pos, total_trades, wins) = 
    list.fold(timeline, initial_state, fn(acc, step) {
      let #(balance, pos, trades, w) = acc
      let #(price, vector) = step
      
      let action = strat(vector)
      
      case action, pos == 0.0 {
        Buy, True -> {
          #(0.0, balance /. price, trades + 1, w)
        }
        Sell, False -> {
          let new_balance = pos *. price
          let _prev_total = balance +. {pos *. price} // Approx
          let is_win = case new_balance >. initial_balance { True -> 1 False -> 0 }
          #(new_balance, 0.0, trades + 1, w + is_win)
        }
        _, _ -> acc
      }
    })
  
  let actual_final_balance = case final_pos >. 0.0 {
    True -> {
      case list.last(timeline) {
        Ok(#(p, _)) -> final_pos *. p
        _ -> final_balance
      }
    }
    False -> final_balance
  }
  
  let win_rate = case total_trades > 0 {
    True -> int.to_float(wins) /. int.to_float(total_trades)
    False -> 0.0
  }
  
  BacktestResult(
    total_trades: total_trades,
    starting_balance: initial_balance,
    final_balance: actual_final_balance,
    percent_return: {actual_final_balance -. initial_balance} /. initial_balance *. 100.0,
    win_rate: win_rate
  )
}

fn process_query_results(results: types.QueryResult) -> List(#(Float, List(Float))) {
  list.filter_map(results, fn(row) {
    let price = case dict.get(row, "price") {
      Ok(fact.Float(p)) -> Ok(p)
      _ -> Error(Nil)
    }
    let vector = case dict.get(row, "vector") {
      Ok(fact.Vec(v)) -> Ok(v)
      _ -> Error(Nil)
    }
    
    case price, vector {
      Ok(p), Ok(v) -> Ok(#(p, v))
      _, _ -> Error(Nil)
    }
  })
}
