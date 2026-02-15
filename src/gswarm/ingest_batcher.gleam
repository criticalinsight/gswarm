import gleam/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/int
import gleam/result
import gleam/io
import gleamdb
import gswarm/market.{type PredictionTick}
import gswarm/registry_actor
import gswarm/lead_time
import gswarm/insider_store
import gswarm/amka_domain
import gleam/option.{None, Some}
import gleam/float

pub type Message {
  Ingest(tick: PredictionTick, vector: List(Float))
  MonitorTrade(trade: amka_domain.TradeActivity)
  Flush
}

pub type BatcherState {
  BatcherState(
    db: gleamdb.Db,
    registry_actor: Subject(registry_actor.Message),
    buffer: List(#(PredictionTick, List(Float))),
    last_flush: Int,
    batch_size_limit: Int,
    flush_interval_ms: Int,
    insider_store: Subject(insider_store.Message)
  )
}

pub fn start(
  db: gleamdb.Db, 
  registry_actor: Subject(registry_actor.Message),
  insider_store: Subject(insider_store.Message)
) -> Result(Subject(Message), actor.StartError) {
  let initial_state = BatcherState(
    db: db,
    registry_actor: registry_actor,
    buffer: [],
    last_flush: 0,
    batch_size_limit: 10,
    flush_interval_ms: 1000,
    insider_store: insider_store
  )

  actor.new(initial_state)
  |> actor.on_message(handle_message)
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

fn handle_message(state: BatcherState, msg: Message) -> actor.Next(BatcherState, Message) {
  case msg {
    Ingest(tick, vector) -> {
      let new_buffer = [#(tick, vector), ..state.buffer]
      process.send(state.registry_actor, registry_actor.RecordMarket(tick.market_id))
      
      // Adaptive Ticking Logic:
      // If load is high (large buffer), shrink interval and increase batch size.
      let buffer_len = list.length(new_buffer)
      let #(new_limit, new_interval) = case buffer_len > 50 {
        True -> #(100, 100) // High Load: 100 ticks or 100ms
        False -> #(10, 1000) // Low Load: 10 ticks or 1s
      }
      
      let next_state = BatcherState(..state, buffer: new_buffer, batch_size_limit: new_limit, flush_interval_ms: new_interval)
      
      case buffer_len >= next_state.batch_size_limit {
        True -> do_flush(next_state)
        False -> actor.continue(next_state)
      }
    }
    MonitorTrade(trade) -> {
      // Async: Wait for lookahead window, then check for lag
      // In a real system, we'd use a robust scheduler.
      // Here, we spawn a process that sleeps, then queries, then reports.
      
      let db = state.db
      let store = state.insider_store
      
      process.spawn(fn() {
        // Wait 5 seconds (simulated "future" check for instant feedback in demo)
        // Real logic: Wait N minutes or poll periodically.
        // For the *demo*, we assume ticks are flowing fast.
        process.sleep(5000)
        
        // Query recent ticks for this market from DB
        // We need a helper in market.gleam to get ticks *after* a timestamp
        // For now, we utilize the existing get_probability_series which returns sorted list
        
        // TODO: This is a heavy query for a task. 
        // Optimization: Rely on the `lead_time` module to do the heavy lifting lightly.
        
        case market.get_probability_series(db, trade.market_slug, "Yes") {
           Ok(series) -> {
             // Convert series to Ticks (Mocking volume/other fields for lead_time comp)
             let ticks = list.map(series, fn(pair) {
               let #(ts, p) = pair
               market.PredictionTick(trade.market_slug, "Yes", p, 0, ts)
             })
             
             // Convert generic PredictionTick to Tick (market.gleam unification required? 
             // market.gleam defines Tick (Legacy) and PredictionTick.
             // lead_time.gleam expects Tick (Legacy) because it imports `type Tick`.
             // Wait, looking at lead_time.gleam...
             // `import gswarm/market.{type Tick}`.
             
             // I need to map PredictionTick to Tick for the calculation.
             let leg_ticks = list.map(ticks, fn(pt) {
                market.Tick(pt.market_id, pt.outcome, pt.probability, pt.volume, pt.timestamp)
             })
             
             case lead_time.compute_lag(trade, leg_ticks) {
               Some(lag) -> {
                 io.println("🕵️‍♂️ Insider Signal: Lag " <> float.to_string(lag.minutes) <> "m")
                 process.send(store, insider_store.RecordTrade(trade.user, lag.minutes))
               }
               None -> Nil
             }
           }
           Error(_) -> Nil
        }
      })
      
      actor.continue(state)
    }
    Flush -> do_flush(state)
  }
}

fn do_flush(state: BatcherState) -> actor.Next(BatcherState, Message) {
  case state.buffer {
    [] -> actor.continue(state)
    ticks -> {
      io.println("🚀 Batching Ingest: " <> int.to_string(list.length(ticks)) <> " ticks")
      let _ = market.ingest_batch_with_vectors(state.db, list.reverse(ticks))
      actor.continue(BatcherState(..state, buffer: []))
    }
  }
}
