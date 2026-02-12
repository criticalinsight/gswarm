import gleam/io
import gleam/int
import gleam/list
import gleam/dict
import gleamdb
import gleamdb/shared/types.{Var}
import gleamdb/fact

pub fn audit_history(db: gleamdb.Db, _market_id: String, tx_id: Int) {
  // Use gleamdb.as_of to look back in time.
  // This demonstrates GleamDB's immutable temporal nature.
  let query = [
    gleamdb.p(#(Var("e"), "tick/price/Yes", Var("price")))
  ]
  
  let past_state_results = gleamdb.as_of(db, tx_id, query)
  
  case list.first(past_state_results) {
    Ok(binding) -> {
      case dict.get(binding, "price") {
        Ok(fact.Float(p)) -> {
          io.println("📜 Analytics Audit: Price at Tx[" <> int.to_string(tx_id) <> "] was " <> float_to_string(p))
        }
        _ -> Nil
      }
    }
    Error(_) -> io.println("📜 Analytics Audit: No data found at Tx[" <> int.to_string(tx_id) <> "]")
  }
}

// Simple helper to avoid dependency bloat in snippet
fn float_to_string(_f: Float) -> String {
  // dummy for demonstration
  "0.XX"
}
