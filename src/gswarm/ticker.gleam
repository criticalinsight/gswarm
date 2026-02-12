import gleam/io
import gleam/int
import gleam/erlang/process
import gleamdb
import gswarm/market

pub fn start_ticker(db: gleamdb.Db, market_id: String) {
  process.spawn_unlinked(fn() {
    loop(db, market_id, 1)
  })
}

fn loop(db: gleamdb.Db, market_id: String, count: Int) {
  // Oscillate price to trigger reflex
  let price = case count % 2 {
    0 -> 0.5
    _ -> 0.6
  }
  
  let tick = market.Tick(
    market_id: market_id,
    outcome: "Yes",
    price: price,
    volume: 100,
    timestamp: count
  )
  
  // We ignore errors to keep ticking high-frequency, 
  // but in production we'd log them.
  case market.ingest_tick(db, tick) {
    Ok(_) -> Nil
    Error(e) -> io.println("⚠️ Tick failed: " <> e)
  }
  
  case count % 1000 == 0 {
    True -> io.println("⚡️ Ingested " <> int.to_string(count) <> " ticks")
    False -> Nil
  }
  
  process.sleep(1) // 1ms throttle = ~1000 ops/sec
  loop(db, market_id, count + 1)
}
