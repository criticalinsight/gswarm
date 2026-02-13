import gleam/io
import gleam/float
import gleam/result
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleamdb
import gswarm/strategy.{type Strategy, Buy, Sell}
import gswarm/risk

pub type State {
  State(
    db: gleamdb.Db,
    market_id: String,
    balance: Float,
    position: Float,
    peak_balance: Float,
    trades: Int,
    halted: Bool,
    active_strategy: Strategy,
    risk_config: risk.RiskConfig
  )
}

pub type Message {
  TickEvent(price: Float, vector: List(Float))
  SetStrategy(strategy: Strategy)
  GetStatus(reply_to: Subject(State))
  Shutdown
}

pub fn start_paper_trader(db: gleamdb.Db, market_id: String, initial_balance: Float) -> Result(Subject(Message), actor.StartError) {
  let state = State(
    db: db,
    market_id: market_id,
    balance: initial_balance,
    position: 0.0,
    peak_balance: initial_balance,
    trades: 0,
    halted: False,
    active_strategy: strategy.mean_reversion,
    risk_config: risk.default_config()
  )

  actor.new(state)
  |> actor.on_message(loop)
  |> actor.start
  |> result.map(fn(started) { started.data })
}

fn loop(state: State, msg: Message) -> actor.Next(State, Message) {
  case msg {
    Shutdown -> actor.stop()

    TickEvent(price, vector) -> {
      // Check drawdown before any action
      let effective_balance = state.balance +. state.position *. price
      let new_peak = float.max(state.peak_balance, effective_balance)
      let is_halted = risk.check_drawdown(effective_balance, new_peak, state.risk_config)

      case is_halted {
        True -> {
          case state.halted {
            False -> {
              io.println("🛑 [PaperTrade] HALTED — drawdown exceeded "
                <> float.to_string(state.risk_config.max_drawdown_pct *. 100.0) <> "%")
              risk.log_risk(effective_balance, new_peak, state.risk_config)
            }
            True -> Nil
          }
           actor.continue(State(..state, halted: True, peak_balance: new_peak))
        }
        False -> {
          // Execute the ACTIVE strategy (hot-swappable)
          let action = state.active_strategy(vector)

          let new_state = case action, state.position == 0.0 {
            Buy, True -> {
              // Risk-gated position sizing
              let pos = risk.size_position(state.balance, price, state.risk_config)
              let cost = pos *. price
              io.println("🚀 [PaperTrade] BUY " <> state.market_id
                <> " @ $" <> float.to_string(price)
                <> " | Size: " <> float.to_string(pos))
              State(..state,
                balance: state.balance -. cost,
                position: pos,
                peak_balance: new_peak,
                trades: state.trades + 1,
                halted: False
              )
            }
            Sell, False -> {
              let proceeds = state.position *. price
              io.println("💰 [PaperTrade] SELL " <> state.market_id
                <> " @ $" <> float.to_string(price)
                <> " | Proceeds: $" <> float.to_string(proceeds))
              State(..state,
                balance: state.balance +. proceeds,
                position: 0.0,
                peak_balance: new_peak,
                trades: state.trades + 1,
                halted: False
              )
            }
            _, _ -> State(..state, peak_balance: new_peak, halted: False)
          }
          actor.continue(new_state)
        }
      }
    }

    SetStrategy(new_strategy) -> {
      io.println("🧬 [PaperTrade] Strategy HOT-SWAPPED for " <> state.market_id)
      actor.continue(State(..state, active_strategy: new_strategy))
    }

    GetStatus(reply_to) -> {
      process.send(reply_to, state)
      actor.continue(state)
    }
  }
}

/// Bridge from live_ticker/market_feed to paper_trader
pub fn broadcast_tick(trader: Subject(Message), price: Float, vector: List(Float)) {
  process.send(trader, TickEvent(price, vector))
}
