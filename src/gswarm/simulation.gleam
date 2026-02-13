import gleam/io
import gleam/int
import gleam/float
import gleam/string
import gleam/option.{None}
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gswarm/market
import gswarm/backtest

pub fn main() {
  io.println("🧪 Starting Gswarm Runtime Simulation...")
  
  // 1. Initialize DB
  let cluster_id = "gswarm_sim_" <> int.to_string(erlang_system_time())
  let assert Ok(db) = gleamdb.start_distributed(cluster_id, None)
  
  // 2. Configure Schema (Critical for queries)
  market.configure_tick_retention(db)
  
  // DEBUG IDs
  let d_uid = fact.deterministic_uid("sim_market_1")
  let p_hash = fact.phash2("sim_market_1")
  let e_id = fact.EntityId(p_hash)
  
  io.println("DEBUG: deterministic_uid(\"sim_market_1\"): " <> string.inspect(d_uid))
  io.println("DEBUG: phash2(\"sim_market_1\"): " <> string.inspect(p_hash))
  io.println("DEBUG: EntityId(phash2): " <> string.inspect(e_id))

  // 3. Create Market
  let m_id = "sim_market_1"
  let m = market.Market(
    id: m_id, 
    question: "Will Gswarm catch bugs?", 
    outcomes: ["Yes", "No"], 
    market_type: market.Binary, 
    status: market.Open, 
    close_time: 2000000000, 
    source: "simulation"
  )
  let assert Ok(_) = market.create_prediction_market(db, m)
  io.println("✅ Market created: " <> m_id)

  // 4. Seed Data (Simulating a trend)
  io.println("🌱 Seeding 1000 ticks...")
  // Generate a sine wave trend + noise
  int.range(from: 1, to: 1001, with: Nil, run: fn(_, i) {
    let f_i = int.to_float(i)
    let price = 0.5 +. 0.3 *. float_sin(f_i /. 100.0)
    
    // Simulate Indicators
    // RSI at index 6 (0-100 range)
    let rsi = 50.0 +. 40.0 *. float_sin(f_i /. 20.0)
    // MACD at index 7 (positive/negative)
    let macd = float_sin(f_i /. 50.0)
    
    let vector = [
      price, // 0
      0.0, 0.0, 0.0, 0.0, 0.0, // 1-5
      rsi,   // 6
      macd,  // 7
      0.0, 0.0, 0.0, 0.0, 0.0 // 8-12+
    ]
    
    let tick = market.PredictionTick(m_id, "Yes", price, 100, i)
    // Ingest with rich vector
    let assert Ok(_) = market.ingest_prediction_tick(db, tick, vector)
    Nil
  })
  
  io.println("⏳ Waiting for indexing...")
  process.sleep(2000)
  
  // 5. Run Adaptive Backtest
  io.println("🏃 Running Adaptive Backtest...")
  let result = backtest.run_adaptive_backtest(db, m_id, 10000.0, 50)
  
  io.println("📊 Backtest Results:")
  io.println("   Trades: " <> int.to_string(result.total_trades))
  io.println("   Final Balance: " <> float.to_string(result.final_balance))
  io.println("   Win Rate: " <> float.to_string(result.win_rate))
  
  // 6. Check for Runtime Bugs (Assertions)
  // If we crashed, we wouldn't be here.
  // Check logic: excessive drawdown?
  case result.drawdown_pct >. 0.5 {
    True -> io.println("⚠️  High Drawdown detected: " <> float.to_string(result.drawdown_pct))
    False -> io.println("✅ Drawdown within limits.")
  }
  
  io.println("✨ Simulation Complete.")
}

@external(erlang, "erlang", "system_time")
fn erlang_system_time() -> Int

@external(erlang, "math", "sin")
fn float_sin(x: Float) -> Float
