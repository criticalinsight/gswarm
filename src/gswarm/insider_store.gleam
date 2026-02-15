import gleam/otp/actor
import gleam/erlang/process.{type Subject}
import gleam/dict.{type Dict}
import gleam/option.{type Option}
import gleam/result
import gleam/list
import gleam/int
import gleam/float
import gleamdb
import gleamdb/fact

pub type InsiderStats {
  InsiderStats(
    trader_id: String,
    total_trades: Int,
    successful_insides: Int,
    avg_lead_time: Float,
    confidence_score: Float,
    lags: List(Float) // Keep last N lags for calculation
  )
}

pub type Message {
  RecordTrade(
    trader_id: String,
    lag_minutes: Float
  )
  GetInsiderStats(
    trader_id: String,
    reply_to: Subject(Option(InsiderStats))
  )
  GetAllInsiders(
    reply_to: Subject(List(InsiderStats))
  )
}

pub type State {
  State(
    db: gleamdb.Db,
    insiders: Dict(String, InsiderStats)
  )
}

pub fn start(db: gleamdb.Db) -> Result(Subject(Message), actor.StartError) {
  actor.new(State(db, dict.new()))
  |> actor.on_message(loop)
  |> actor.start()
  |> result.map(fn(started) { started.data })
}

fn loop(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    RecordTrade(trader_id, lag) -> {
      let stats = dict.get(state.insiders, trader_id) |> result.unwrap(
        InsiderStats(trader_id, 0, 0, 0.0, 0.0, [])
      )
      
      let new_lags = list.take([lag, ..stats.lags], 20) // Keep last 20
      let new_total = stats.total_trades + 1
      let is_success = lag <. -5.0 // Trade happened >5 mins before move
      let new_success = case is_success {
        True -> stats.successful_insides + 1
        False -> stats.successful_insides
      }
      
      // Compute new average lead time
      let new_avg = list.fold(new_lags, 0.0, fn(acc, l) { acc +. l }) /. int.to_float(list.length(new_lags))
      
      // Compute new confidence score
      // Simple heuristic for now: (success_rate * log(total_trades))
      let success_rate = int.to_float(new_success) /. int.to_float(new_total)
      let weight = float.logarithm(int.to_float(new_total) +. 1.0) |> result.unwrap(0.0)
      let new_confidence = float.min(1.0, success_rate *. { weight /. 2.0 })
      
      let new_stats = InsiderStats(
        trader_id,
        new_total,
        new_success,
        new_avg,
        new_confidence,
        new_lags
      )
      
      // Persist to GleamDB (Fact: "insider/confidence")
      let tid = fact.deterministic_uid(trader_id)
      let _ = gleamdb.transact(state.db, [
        #(tid, "insider/confidence", fact.Float(new_confidence)),
        #(tid, "insider/avg_lag", fact.Float(new_avg))
      ])
      
      let new_insiders = dict.insert(state.insiders, trader_id, new_stats)
      actor.continue(State(state.db, new_insiders))
    }
    
    GetInsiderStats(tid, reply_to) -> {
      process.send(reply_to, dict.get(state.insiders, tid) |> option.from_result)
      actor.continue(state)
    }
    
    GetAllInsiders(reply_to) -> {
      process.send(reply_to, dict.values(state.insiders))
      actor.continue(state)
    }
  }
}
