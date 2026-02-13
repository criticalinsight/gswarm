import gleam/int
import gleam/float
import gleam/list
import gleam/dict
import gleam/result
import gleamdb
import gleamdb/fact
import gleamdb/shared/types
import gleamdb/q

// --- Core Market Types ---

@external(erlang, "erlang", "phash2")
fn phash2(x: a) -> Int

/// The kind of prediction market.
/// Binary = Yes/No (probability 0.0–1.0)
/// MultiOutcome = N outcomes, probabilities should sum to ~1.0
/// Scalar = Numeric range prediction (future)
pub type MarketType {
  Binary
  MultiOutcome
  Scalar
}

/// The lifecycle status of a prediction market.
/// Resolved carries the winning outcome for settlement.
pub type MarketStatus {
  Open
  Closed
  Resolved(winning_outcome: String)
}

/// A prediction market: a question with possible outcomes.
/// `source` tracks provenance (e.g. "manifold", "polymarket", "internal").
pub type Market {
  Market(
    id: String,
    question: String,
    outcomes: List(String),
    market_type: MarketType,
    status: MarketStatus,
    close_time: Int,
    source: String
  )
}

// --- Tick Types ---

/// Legacy tick: USD spot price for crypto feeds.
/// Preserved for backward compatibility with live_ticker, ticker, backtest.
pub type Tick {
  Tick(
    market_id: String,
    outcome: String,
    price: Float,
    volume: Int,
    timestamp: Int
  )
}

/// Prediction market tick: probability in [0.0, 1.0] for a specific outcome.
/// This is the native representation for event-outcome markets.
pub type PredictionTick {
  PredictionTick(
    market_id: String,
    outcome: String,
    probability: Float,
    volume: Int,
    timestamp: Int
  )
}

// --- Query Functions ---

/// Retrieve the latest 50D Alpha vector for a market from GleamDB.
/// Queries the `market/latest_vector` attribute written by `ingest_tick_with_vector`.
pub fn get_latest_vector(db: gleamdb.Db, market_id: String) -> Result(List(Float), Nil) {
  let query = [
    types.Positive(#(types.Val(fact.Str(market_id)), "market/id", types.Var("m"))),
    types.Positive(#(types.Var("m"), "market/latest_vector", types.Var("vec")))
  ]
  case gleamdb.query(db, query) {
    [row, ..] -> case dict.get(row, "vec") {
      Ok(fact.Vec(v)) -> Ok(v)
      _ -> Error(Nil)
    }
    _ -> Error(Nil)
  }
}

// --- Prediction Tick Functions ---

/// Validate a prediction tick: probability must be in [0.0, 1.0], volume >= 0.
pub fn validate_prediction_tick(tick: PredictionTick) -> Result(Nil, String) {
  case tick.probability <. 0.0 || tick.probability >. 1.0 {
    True -> Error("Probability out of bounds [0.0, 1.0]: " <> float.to_string(tick.probability))
    False -> {
      case tick.volume < 0 {
        True -> Error("Negative volume: " <> int.to_string(tick.volume))
        False -> Ok(Nil)
      }
    }
  }
}

/// Convert a PredictionTick to a basic vector: [probability, volume_normalized].
/// Full Alpha enrichment happens in market_feed via analytics.
pub fn prediction_tick_to_vector(tick: PredictionTick) -> List(Float) {
  [
    tick.probability,
    int.to_float(tick.volume) /. 10_000.0
  ]
}

/// Ingest a prediction tick with its computed Alpha vector.
/// Stores probability (not price) as the primary fact.
pub fn ingest_prediction_tick(
  db: gleamdb.Db,
  tick: PredictionTick,
  vector: List(Float)
) -> Result(types.DbState, String) {
  use _ <- result.try(validate_prediction_tick(tick))

  let market_id_hash = phash2(tick.market_id)
  let market_ref = fact.Ref(fact.EntityId(market_id_hash))
  
  // Deterministic Tick ID
  let tick_id_hash = phash2(#(tick.market_id, tick.timestamp, tick.outcome))
  let tick_entity = fact.Uid(fact.EntityId(tick_id_hash))

  let facts = [
    #(tick_entity, "tick/market", market_ref),
    #(tick_entity, "tick/outcome", fact.Str(tick.outcome)),
    #(tick_entity, "tick/probability", fact.Float(tick.probability)),
    #(tick_entity, "tick/volume", fact.Int(tick.volume)),
    #(tick_entity, "tick/timestamp", fact.Int(tick.timestamp)),
    #(tick_entity, "tick/vector", fact.Vec(vector))
    // We can also update market latest vector on the market entity itself if needed, 
    // but for now let's keep it simple.
  ]

  gleamdb.transact(db, facts)
}

// --- Legacy Tick Functions (Crypto Spot Feed) ---

pub fn tick_to_vector(tick: Tick) -> List(Float) {
  [
    tick.price /. 1000.0,
    0.5, 
    int.to_float(tick.volume) /. 10000.0
  ]
}

pub fn validate_tick(tick: Tick) -> Result(Nil, String) {
  case tick.price <. 0.0 {
    True -> Error("Negative price detected: " <> float.to_string(tick.price))
    False -> {
      case tick.volume < 0 {
        True -> Error("Negative volume detected: " <> int.to_string(tick.volume))
        False -> Ok(Nil)
      }
    }
  }
}

pub fn ingest_tick(db: gleamdb.Db, tick: Tick) -> Result(types.DbState, String) {
  ingest_tick_with_vector(db, tick, tick_to_vector(tick))
}

pub fn ingest_batch(db: gleamdb.Db, ticks: List(Tick)) -> Result(types.DbState, String) {
  use _ <- result.try(list.try_each(ticks, validate_tick))
  
  let facts = list.flat_map(ticks, fn(tick) {
    let vector = tick_to_vector(tick)
    [
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/price/" <> tick.outcome, fact.Float(tick.price)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/volume/" <> tick.outcome, fact.Int(tick.volume)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/timestamp", fact.Int(tick.timestamp)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/vector", fact.Vec(vector))
    ]
  })
  
  gleamdb.transact(db, facts)
}

pub fn ingest_tick_with_vector(db: gleamdb.Db, tick: Tick, vector: List(Float)) -> Result(types.DbState, String) {
   let facts = [
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/price/" <> tick.outcome, fact.Float(tick.price)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/volume/" <> tick.outcome, fact.Int(tick.volume)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/timestamp", fact.Int(tick.timestamp)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "tick/vector", fact.Vec(vector)),
      #(fact.Lookup(#("market/id", fact.Str(tick.market_id))), "market/latest_vector", fact.Vec(vector))
   ]
  
  gleamdb.transact(db, facts)
}

// --- Schema & Market Creation ---

pub fn configure_tick_retention(db: gleamdb.Db) {
  let config = fact.AttributeConfig(unique: False, component: False, retention: fact.All)
  let unique_config = fact.AttributeConfig(unique: True, component: False, retention: fact.All)
  
  // Market ID must be unique
  let _ = gleamdb.set_schema(db, "market/id", unique_config)
  // Internal UID for Lookup mechanism (workaround for indexing issue)
  let _ = gleamdb.set_schema(db, "market/uid", unique_config)
  
  let _ = gleamdb.set_schema(db, "tick/price/Yes", config)
  let _ = gleamdb.set_schema(db, "tick/probability/YES", config)
  let _ = gleamdb.set_schema(db, "tick/probability/NO", config)
  let _ = gleamdb.set_schema(db, "tick/volume/Yes", config)
  let _ = gleamdb.set_schema(db, "tick/timestamp", config)
  let _ = gleamdb.set_schema(db, "tick/vector", config)
  Nil
}

/// Create a prediction market in GleamDB.
/// Stores market metadata including type, status, close time, and source.
pub fn create_prediction_market(db: gleamdb.Db, m: Market) -> Result(types.DbState, String) {
  let type_str = case m.market_type {
    Binary -> "binary"
    MultiOutcome -> "multi"
    Scalar -> "scalar"
  }
  let status_str = case m.status {
    Open -> "open"
    Closed -> "closed"
    Resolved(w) -> "resolved:" <> w
  }
  // Use deterministic ID hash to bypass Lookup indexing bug (Phase 23 fix)
  let id_hash = phash2(m.id)
  let uid = fact.Uid(fact.EntityId(id_hash))
  
  let facts = [
    #(uid, "market/id", fact.Str(m.id)),
    #(uid, "market/question", fact.Str(m.question)),
    #(uid, "market/type", fact.Str(type_str)),
    #(uid, "market/status", fact.Str(status_str)),
    #(uid, "market/close_time", fact.Int(m.close_time)),
    #(uid, "market/source", fact.Str(m.source))
  ]

  gleamdb.transact(db, facts)
}

/// Legacy: create a simple market (backward compat for tests and crypto).
pub fn create_market(db: gleamdb.Db, market: Market) -> Result(types.DbState, String) {
  let facts = [
    #(fact.Uid(fact.EntityId(100)), "market/id", fact.Str(market.id)),
    #(fact.Uid(fact.EntityId(100)), "market/question", fact.Str(market.question))
  ]
  
  gleamdb.transact_with_timeout(db, facts, 10000)
}

// --- Cross-Market Analysis Helpers ---

/// Retrieve all active prediction markets (status = "open").
pub fn get_active_prediction_markets(db: gleamdb.Db) -> Result(List(String), Nil) {
  let query = [
    types.Positive(#(types.Var("m"), "market/status", types.Val(fact.Str("open")))),
    types.Positive(#(types.Var("m"), "market/id", types.Var("id")))
  ]
  
  case gleamdb.query(db, query) {
    [] -> Ok([])
    rows -> {
      let ids = list.filter_map(rows, fn(row) {
        case dict.get(row, "id") {
          Ok(fact.Str(id)) -> Ok(id)
          _ -> Error(Nil)
        }
      })
      Ok(ids)
    }
  } 
}

/// Retrieve the probability time series for a market's outcome.
/// Returns a list of #(timestamp, probability) sorted by time ascending.
/// Note: Inefficiently pulls all ticks and sorts in memory due to DB limitations.
pub fn get_probability_series(
  db: gleamdb.Db, 
  market_id: String, 
  outcome: String
) -> Result(List(#(Int, Float)), Nil) {
  // Resolve Market ID using phash2 to match ingest
  let market_id_hash = phash2(market_id)
  
  let query = 
    q.new()
    |> q.where(types.Var("t"), "tick/market", types.Val(fact.Ref(fact.EntityId(market_id_hash))))
    |> q.where(types.Var("t"), "tick/outcome", types.Val(fact.Str(outcome)))
    |> q.where(types.Var("t"), "tick/probability", types.Var("prob"))
    |> q.where(types.Var("t"), "tick/timestamp", types.Var("ts"))
    // Database-native sort (Phase 23)
    |> q.order_by("ts", types.Asc)
    |> q.to_clauses

  case gleamdb.query(db, query) {
    [] -> Ok([])
    rows -> {
      let series = list.filter_map(rows, fn(row) {
        case dict.get(row, "ts"), dict.get(row, "prob") {
          Ok(fact.Int(t)), Ok(fact.Float(p)) -> Ok(#(t, p))
          _, _ -> Error(Nil)
        }
      })
      // No manual sort needed!
      Ok(series)
    }
  }
}
