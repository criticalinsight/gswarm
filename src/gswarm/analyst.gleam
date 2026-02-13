import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gleamdb/shared/types
import gswarm/market

// The Analyst: Finds similar historical patterns and predicts probability movements.
// Now operates in probability space for prediction markets.
// Records predictions with predicted_probability for Brier scoring.

pub fn start_analyst(db: gleamdb.Db, market_id: String) {
  process.spawn_unlinked(fn() {
    loop(db, market_id)
  })
}

fn loop(db: gleamdb.Db, market_id: String) {
  process.sleep(10_000) // Analyze every 10s
  
  case market.get_latest_vector(db, market_id) {
    Ok(latest_vec) -> {
      // Find historical matches (k=5)
      let query = [
        types.Similarity("v", latest_vec, 0.85)
      ]
      
      let matches = gleamdb.query(db, query)
      let count = list.length(matches)
      
      case count > 1 {
        True -> {
          let fractals = count - 1
          
          // Derive prediction direction from fractal count
          let direction = case fractals > 3 {
            True -> "probability_up"
            False -> "probability_down"
          }
          
          // Extract current probability (first element of vector)
          let current_prob = case list.first(latest_vec) {
            Ok(p) -> p
            _ -> 0.5
          }

          // Predict probability magnitude based on fractal confidence
          let delta = int.to_float(fractals) *. 0.02  // 2% per fractal
          let predicted_prob = case direction {
            "probability_up" -> float.min(current_prob +. delta, 1.0)
            _ -> float.max(current_prob -. delta, 0.0)
          }
          
          io.println("🔮 Analyst [" <> market_id <> "]: " <> direction
            <> " | Current: " <> float.to_string(current_prob)
            <> " | Predicted: " <> float.to_string(predicted_prob)
            <> " | Confidence: " <> int.to_string(fractals) <> " fractals")
          
          // Record prediction for Brier scoring
          record_probability_prediction(db, market_id, direction, current_prob, predicted_prob)
        }
        False -> io.println("🧠 Analyst [" <> market_id <> "]: Uncharted territory (no similar patterns)")
      }
    }
    Error(_) -> Nil
  }
  
  loop(db, market_id)
}

/// Record a probability prediction for future Brier scoring.
/// Stores current and predicted probability so resolution.gleam can compare.
fn record_probability_prediction(
  db: gleamdb.Db,
  market_id: String,
  direction: String,
  current_probability: Float,
  predicted_probability: Float
) {
  let ts = erlang_system_time()
  let pred_id = "pred_" <> market_id <> "_" <> int.to_string(ts)
  let lookup = fact.Lookup(#("prediction/id", fact.Str(pred_id)))

  let facts = [
    #(lookup, "prediction/id", fact.Str(pred_id)),
    #(lookup, "prediction/market_id", fact.Str(market_id)),
    #(lookup, "prediction/direction", fact.Str(direction)),
    #(lookup, "prediction/current_probability", fact.Float(current_probability)),
    #(lookup, "prediction/predicted_probability", fact.Float(predicted_probability)),
    #(lookup, "prediction/timestamp", fact.Int(ts)),
    #(lookup, "prediction/status", fact.Str("pending"))
  ]
  let _ = gleamdb.transact(db, facts)
  Nil
}

@external(erlang, "erlang", "system_time")
fn do_system_time(unit: Int) -> Int

fn erlang_system_time() -> Int {
  do_system_time(1000)
}
