import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/dynamic/decode
import gleam/result
import gleam/erlang/process
import gleamdb
import gswarm/market
import gswarm/analytics
import gswarm/paper_trader
import gleam/option.{type Option, None, Some}

/// A decoded Manifold Markets API response.
pub type ManifoldMarket {
  ManifoldMarket(
    id: String,
    question: String,
    probability: Float,
    volume: Float,
    close_time: Int,
    is_resolved: Bool,
    resolution: String
  )
}

/// Start prediction market feeds for a list of Manifold market slugs.
/// Each market gets its own polling loop (15s interval).
pub fn start_market_feed(
  db: gleamdb.Db,
  market_ids: List(String),
  trader: Option(process.Subject(paper_trader.Message))
) {
  list.each(market_ids, fn(mid) {
    process.spawn_unlinked(fn() {
      io.println("🎲 MarketFeed: Tracking prediction market [" <> mid <> "]")
      loop(db, mid, [], trader)
    })
  })
}

/// Main polling loop: fetch probability, compute Alpha, ingest, broadcast.
fn loop(
  db: gleamdb.Db,
  market_id: String,
  history: List(Float),
  trader: Option(process.Subject(paper_trader.Message))
) {
  let ts = erlang_system_time()

  case fetch_manifold_market(market_id) {
    Ok(mkt) -> {
      // 1. Update probability history (keep last 200)
      let new_history = [mkt.probability, ..list.take(history, 199)]

      //    Use std_dev and sma as basic features — volume_list from volume
      let volume_float_list = [mkt.volume, ..list.take(list.repeat(mkt.volume, 199), 199)]
      let volume_list = list.map(volume_float_list, float.truncate)
      let alpha_vector = analytics.calculate_all_metrics_with_time(
        new_history, volume_list, ts
      )

      // 3. Create and ingest prediction tick
      let tick = market.PredictionTick(
        market_id: "pm_" <> market_id,
        outcome: "YES",
        probability: mkt.probability,
        volume: float.truncate(mkt.volume),
        timestamp: ts
      )
      let _ = market.ingest_prediction_tick(db, tick, alpha_vector)

      // 4. Broadcast to paper trader (probability as "price" for strategy)
      case trader {
        Some(t) -> paper_trader.broadcast_tick(t, mkt.probability, alpha_vector)
        None -> Nil
      }

      // 5. Log
      let vol_short = analytics.std_dev(list.take(new_history, 10))
      io.println("🎲 Prediction [pm_" <> market_id <> "]: "
        <> mkt.question
        <> " | P=" <> float.to_string(mkt.probability)
        <> " | Vol: " <> float.to_string(mkt.volume)
        <> " | σ(10): " <> float.to_string(vol_short))

      // 6. Check resolution
      case mkt.is_resolved {
        True -> {
          io.println("🏁 Market RESOLVED: " <> mkt.question
            <> " → " <> mkt.resolution)
          // Don't loop — market is done
          Nil
        }
        False -> {
          process.sleep(15_000)
          loop(db, market_id, new_history, trader)
        }
      }
    }
    Error(e) -> {
      io.println("⚠️ MarketFeed Error [" <> market_id <> "]: " <> e)
      process.sleep(15_000)
      loop(db, market_id, history, trader)
    }
  }
}

/// Fetch a market from the Manifold Markets API.
/// GET https://api.manifold.markets/v0/market/{slug}
fn fetch_manifold_market(market_id: String) -> Result(ManifoldMarket, String) {
  let url = "https://api.manifold.markets/v0/slug/" <> market_id
  let req_result = request.to(url) |> result.map_error(fn(_) { "Invalid URL" })
  use req <- result.try(req_result)

  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> decode_manifold(resp.body)
    Ok(resp) -> Error("API returned status " <> int.to_string(resp.status))
    Error(_) -> Error("HTTP request failed")
  }
}

/// Decode Manifold API JSON response.
/// Handles both binary (probability) and multi-outcome markets.
fn decode_manifold(json_str: String) -> Result(ManifoldMarket, String) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use question <- decode.field("question", decode.string)
    use probability <- decode.optional_field("probability", 0.5, decode.float)
    use volume <- decode.optional_field("volume", 0.0, decode.float)
    use close_time <- decode.optional_field("closeTime", 0, decode.int)
    use is_resolved <- decode.optional_field("isResolved", False, decode.bool)
    use resolution <- decode.optional_field("resolution", "", decode.string)
    decode.success(ManifoldMarket(
      id: id,
      question: question,
      probability: probability,
      volume: volume,
      close_time: close_time,
      is_resolved: is_resolved,
      resolution: resolution
    ))
  }

  case json.parse(from: json_str, using: decoder) {
    Ok(m) -> Ok(m)
    Error(_) -> Error("Manifold JSON decode failed")
  }
}

@external(erlang, "erlang", "system_time")
fn do_system_time(unit: Int) -> Int

fn erlang_system_time() -> Int {
  do_system_time(1000)
}
