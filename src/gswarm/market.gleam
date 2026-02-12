import gleamdb
import gleamdb/fact

pub type Market {
  Market(
    id: String,
    question: String,
    outcomes: List(String)
  )
}

pub type Tick {
  Tick(
    market_id: String,
    outcome: String,
    price: Float,
    volume: Int,
    timestamp: Int
  )
}

import gleamdb/shared/types.{type DbState}

pub fn ingest_tick(db: gleamdb.Db, tick: Tick) -> Result(DbState, String) {
  // In a real high-frequency system, we might batch these.
  // For Gswarm, we transact directly to demonstrate Silicon Saturation speed.
  
  let facts = [
    #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/price/" <> tick.outcome, fact.Float(tick.price)),
    #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/volume/" <> tick.outcome, fact.Int(tick.volume)),
    #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/timestamp", fact.Int(tick.timestamp))
  ]
  
  gleamdb.transact(db, facts)
}

pub fn configure_tick_retention(db: gleamdb.Db) {
  let config = fact.AttributeConfig(unique: False, component: False, retention: fact.LatestOnly)
  let _ = gleamdb.set_schema(db, "tick/price/Yes", config)
  let _ = gleamdb.set_schema(db, "tick/volume/Yes", config)
  let _ = gleamdb.set_schema(db, "tick/timestamp", config)
  Nil
}

pub fn create_market(db: gleamdb.Db, market: Market) -> Result(DbState, String) {
  let facts = [
    // In a real app we'd use a unique ID generator, but for local-first
    // we can trust the transactor to assign a new ID for Uid(EntityId(0)) if treated as new,
    // or we manually assign a deterministic ID based on hash.
    // Here we use a fixed ID for simplicity of the reference.
    #(fact.Uid(fact.EntityId(100)), "market/id", fact.Str(market.id)),
    #(fact.Uid(fact.EntityId(100)), "market/question", fact.Str(market.question))
  ]
  
  gleamdb.transact_with_timeout(db, facts, 10000)
}
