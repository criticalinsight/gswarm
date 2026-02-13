import gleam/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/int
import gleam/result
import gleam/io
import gleamdb
import gswarm/market.{type PredictionTick}
import gswarm/registry_actor

pub type Message {
  Ingest(tick: PredictionTick, vector: List(Float))
  Flush
}

pub type BatcherState {
  BatcherState(
    db: gleamdb.Db,
    registry_actor: Subject(registry_actor.Message),
    buffer: List(#(PredictionTick, List(Float))),
    last_flush: Int,
    batch_size_limit: Int,
    flush_interval_ms: Int
  )
}

pub fn start(db: gleamdb.Db, registry_actor: Subject(registry_actor.Message)) -> Result(Subject(Message), actor.StartError) {
  let initial_state = BatcherState(
    db: db,
    registry_actor: registry_actor,
    buffer: [],
    last_flush: 0,
    batch_size_limit: 10,
    flush_interval_ms: 1000
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
