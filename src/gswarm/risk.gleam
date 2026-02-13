import gleam/io
import gleam/float
import gleam/int

/// Portfolio Risk Engine (Phase 32).
/// Position sizing and drawdown protection.
/// Rich Hickey: "Constraints are not limitations — they are the shape of wisdom."

pub type RiskConfig {
  RiskConfig(
    max_position_pct: Float,    // Max % of balance per trade (e.g., 0.02 = 2%)
    max_drawdown_pct: Float,    // Halt threshold (e.g., 0.10 = 10% from peak)
    min_signal_score: Float     // Minimum correlator signal to act on
  )
}

/// Default config: 2% position size, 10% drawdown halt, 0.3 min signal
pub fn default_config() -> RiskConfig {
  RiskConfig(
    max_position_pct: 0.02,
    max_drawdown_pct: 0.10,
    min_signal_score: 0.3
  )
}

/// Calculate max position size in units given balance, price, and risk config.
/// Never risk more than max_position_pct of total balance on a single trade.
pub fn size_position(balance: Float, price: Float, config: RiskConfig) -> Float {
  case price >. 0.0 {
    True -> {
      let max_capital = balance *. config.max_position_pct
      max_capital /. price
    }
    False -> 0.0
  }
}

/// Check if drawdown has exceeded the halt threshold.
/// Returns True if trading should HALT (drawdown exceeded).
pub fn check_drawdown(current_balance: Float, peak_balance: Float, config: RiskConfig) -> Bool {
  case peak_balance >. 0.0 {
    True -> {
      let drawdown = { peak_balance -. current_balance } /. peak_balance
      drawdown >. config.max_drawdown_pct
    }
    False -> False
  }
}

/// Log risk status for observability.
pub fn log_risk(balance: Float, peak: Float, config: RiskConfig) {
  let drawdown_pct = case peak >. 0.0 {
    True -> { peak -. balance } /. peak *. 100.0
    False -> 0.0
  }
  let halted = check_drawdown(balance, peak, config)
  let status = case halted {
    True -> "🛑 HALTED"
    False -> "✅ ACTIVE"
  }
  io.println("⚖️ Risk: " <> status
    <> " | Balance: $" <> float.to_string(balance)
    <> " | Peak: $" <> float.to_string(peak)
    <> " | Drawdown: " <> float.to_string(drawdown_pct) <> "%"
    <> " | Max Position: " <> int.to_string(float.truncate(config.max_position_pct *. 100.0)) <> "%")
}
