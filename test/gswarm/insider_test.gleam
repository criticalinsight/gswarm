import gleam/option.{None, Some}
import gleeunit
import gleeunit/should
import gswarm/lead_time
import gswarm/amka_domain
import gswarm/market

pub fn main() {
  gleeunit.main()
}

// Helper to create a dummy trade
fn create_trade(timestamp: Int, trade_type: amka_domain.TradeType) -> amka_domain.TradeActivity {
  amka_domain.TradeActivity(
    user: "trader_1",
    market_title: "Will Gswarm Succeed?",
    market_slug: "gswarm-success",
    trade_type: trade_type,
    size: 100.0,
    price: 0.5,
    usdc_size: 50.0,
    timestamp: timestamp,
  )
}

// Helper to create a dummy tick
fn create_tick(price: Float, timestamp: Int) -> market.Tick {
  market.Tick(
    market_id: "gswarm-success",
    outcome: "Yes",
    price: price,
    volume: 1000,
    timestamp: timestamp,
  )
}

pub fn compute_lag_insider_detected_test() {
  // Trade entering BEFORE the pump
  let trade = create_trade(1000, amka_domain.Buy)
  
  // Ticks showing a 10% jump 5 minutes later (300s)
  let ticks = [
    create_tick(0.50, 1100), // Flat
    create_tick(0.51, 1200), // Slight up
    create_tick(0.60, 1300), // BOOM! +20% pump
    create_tick(0.62, 1400),
  ]

  // Expect lag of around -5 minutes (Trade at 1000, Inflection at 1300 = 300s difference)
  // Actually lag is defined as Inflection - Trade.
  // Wait, lead_time.gleam calculates `let lag = impact_time - trade.timestamp`.
  // If trade is at 1000 and inflection at 1300, lag is 300.
  // Wait, "Lead-Time Lag" usually implies how much *before* the event.
  // If result is positive 300s, it means trade was 5 mins *early*.
  // Let's check lead_time.gleam logic.
  
  let result = lead_time.compute_lag(trade, ticks)
  
  // We expect Some(lag)
  case result {
    Some(lag) -> {
      // Lag should be around 5.0 minutes
      // 300 seconds / 60 = 5.0
      lag.minutes |> should.equal(-5.0)
    }
    None -> should.fail()
  }
}

pub fn compute_lag_no_insider_test() {
  // Trade entering continuously flat market
  let trade = create_trade(1000, amka_domain.Buy)
  
  let ticks = [
    create_tick(0.50, 1100),
    create_tick(0.51, 1200),
    create_tick(0.52, 1300), // Small shift < 5%
    create_tick(0.49, 1400),
  ]

  lead_time.compute_lag(trade, ticks)
  |> should.equal(None)
}
