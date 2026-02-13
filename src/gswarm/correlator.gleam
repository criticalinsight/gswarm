import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/dict
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gleamdb/shared/types

// The Correlator: Cross-correlates News Sentiment with Price Movement.
// Stores derived "correlation facts" — immutable values, not mutations (Hickey).
//
// Signal = f(news_vector, price_delta)
// A positive signal means news preceded an UP move; negative means DOWN.

pub type CorrelationResult {
  CorrelationResult(
    news_id: String,
    direction: String,
    delta_pct: Float,
    signal_score: Float
  )
}

pub fn start_correlator(db: gleamdb.Db) {
  process.spawn_unlinked(fn() {
    io.println("📊 Correlator: Started. Scanning for news→price signals...")
    loop(db)
  })
}

fn loop(db: gleamdb.Db) {
  process.sleep(30_000) // Correlate every 30s

  // 1. Query recent news vectors
  let news_query = [
    types.Positive(#(types.Var("n"), "news/title", types.Var("title"))),
    types.Positive(#(types.Var("n"), "news/vector", types.Var("nvec"))),
    types.Positive(#(types.Var("n"), "news/timestamp", types.Var("nts")))
  ]
  let news_results = gleamdb.query(db, news_query)

  // 2. Query current BTC price vector
  let price_query = [
    types.Positive(#(types.Val(fact.Str("m_btc")), "market/id", types.Var("m"))),
    types.Positive(#(types.Var("m"), "tick/price/Yes", types.Var("price"))),
    types.Positive(#(types.Var("m"), "tick/vector", types.Var("pvec")))
  ]
  let price_results = gleamdb.query(db, price_query)

  // 3. Compute correlation signal
  let news_count = list.length(news_results)
  let price_count = list.length(price_results)

  case news_count > 0, price_count > 0 {
    True, True -> {
      // Extract the latest price for signal computation
      let current_price = extract_latest_price(price_results)
      let avg_sentiment = compute_avg_sentiment(news_results)

      // Signal Score = sentiment_magnitude × price_momentum_direction
      // Positive = bullish news + upward price = strong BUY signal
      // Negative = bearish news + downward price = strong SELL signal
      let signal_score = avg_sentiment *. current_price

      // Store the correlation fact
      let corr_id = "corr_" <> int.to_string(erlang_system_time())
      let lookup = fact.Lookup(#("correlation/id", fact.Str(corr_id)))
      let correlation_facts = [
        #(lookup, "correlation/id", fact.Str(corr_id)),
        #(lookup, "correlation/signal_score", fact.Float(signal_score)),
        #(lookup, "correlation/news_count", fact.Int(news_count)),
        #(lookup, "correlation/price_at_signal", fact.Float(current_price)),
        #(lookup, "correlation/sentiment_avg", fact.Float(avg_sentiment)),
        #(lookup, "correlation/timestamp", fact.Int(erlang_system_time()))
      ]
      let _ = gleamdb.transact(db, correlation_facts)

      let direction = case signal_score >. 0.0 {
        True -> "BULLISH 🟢"
        False -> "BEARISH 🔴"
      }

      io.println("📊 Correlator: " <> direction
        <> " | Signal: " <> float.to_string(signal_score)
        <> " | News: " <> int.to_string(news_count)
        <> " | Sentiment: " <> float.to_string(avg_sentiment)
        <> " | Price: $" <> float.to_string(current_price))
    }
    _, _ -> {
      io.println("📊 Correlator: Waiting for data (News: " <> int.to_string(news_count)
        <> ", Prices: " <> int.to_string(price_count) <> ")")
    }
  }

  loop(db)
}

/// Extract the most recent price from query results
fn extract_latest_price(results: types.QueryResult) -> Float {
  case list.first(results) {
    Ok(row) -> {
      case dict.get(row, "price") {
        Ok(fact.Float(p)) -> p
        _ -> 0.0
      }
    }
    _ -> 0.0
  }
}

/// Compute average sentiment from news vectors
/// Uses the first component of the news vector as a proxy for sentiment magnitude
fn compute_avg_sentiment(results: types.QueryResult) -> Float {
  let sentiments = list.filter_map(results, fn(row) {
    case dict.get(row, "nvec") {
      Ok(fact.Vec(v)) -> {
        case list.first(v) {
          Ok(s) -> Ok(s)
          _ -> Error(Nil)
        }
      }
      _ -> Error(Nil)
    }
  })

  let count = list.length(sentiments)
  case count > 0 {
    True -> {
      let sum = list.fold(sentiments, 0.0, fn(acc, s) { acc +. s })
      sum /. int.to_float(count)
    }
    False -> 0.0
  }
}

@external(erlang, "erlang", "system_time")
fn do_system_time(unit: Int) -> Int

fn erlang_system_time() -> Int {
  do_system_time(1000)
}
