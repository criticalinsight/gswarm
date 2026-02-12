import gleam/io
import gleam/list
import gleam/dict
import gleamdb
import gleamdb/shared/types.{Var}
import gleamdb/fact
import gswarm/semantic

pub fn index_market_semantics(db: gleamdb.Db, market_id: String, question: String) {
  let embedding = semantic.generate_embedding(question)
  
  let facts = [
    #(fact.Lookup(#("market/id", fact.Str(market_id))), "market/embedding", fact.Vec(embedding))
  ]
  
  gleamdb.transact(db, facts)
}

pub fn find_similar_markets(db: gleamdb.Db, question: String) {
  let target_vector = semantic.generate_embedding(question)
  
  // Use GleamDB's Vector Sovereignty: Datalog + Similarity Join
  let query = [
    gleamdb.p(#(Var("e"), "market/id", Var("id"))),
    types.Similarity(variable: "e", vector: target_vector, threshold: 0.8)
  ]
  
  let results = gleamdb.query(db, query)
  
  case list.is_empty(results) {
    True -> io.println("🧠 Context: No similar markets found.")
    False -> {
      io.println("🧠 Context: Found " <> int_to_string(list.length(results)) <> " similar markets.")
      list.each(results, fn(r) {
        case dict.get(r, "id") {
          Ok(fact.Str(id)) -> io.println("   - " <> id)
          _ -> Nil
        }
      })
    }
  }
}

fn int_to_string(_i: Int) -> String { "many" }
