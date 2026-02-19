import gleam/io
import gleam/int
import gleam/list
import gleam/string
import gleamdb
import gleamdb/fact
import gleamdb/engine
import gleamdb/shared/types.{Out, Attr, PullMap}

pub fn pulse_market(db: gleamdb.Db, market_id: String) {
  // Use gleamdb.pull to get a structured snapshot of the market identity.
  // This demonstrates structured data retrieval vs triple queries.
  let pattern = [
    Attr("market/id"),
    Attr("market/question")
  ]
  
  let result = gleamdb.pull(db, fact.Lookup(#("market/id", fact.Str(market_id))), pattern)
  
  case result {
    PullMap(data) -> {
      io.println("🧠 Observer Pulse: Market Snapshot")
      io.println("   ID: " <> string.inspect(data))
    }
    _ -> io.println("⚠️ Observer Pulse: Failed to capture market " <> market_id)
  }
}

/// Phase 60: Quick market relationship scan using the Graph Traversal DSL.
/// Follows market/influences edges to discover downstream impact chain.
pub fn scan_market_influence(db: gleamdb.Db, market_id: String) {
  let eid = fact.Lookup(#("market/id", fact.Str(market_id)))
  case gleamdb.traverse(db, eid, [Out("market/influences")], 3) {
    Ok(influenced) -> {
      io.println("🔗 Observer: " <> market_id <> " influences "
        <> int.to_string(list.length(influenced)) <> " markets")
    }
    Error(e) -> io.println("⚠️ Traverse: " <> e)
  }
}
