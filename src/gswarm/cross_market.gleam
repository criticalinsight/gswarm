import gleam/io
import gleam/int
import gleam/float
import gleam/list
import gleam/dict
import gleam/result
import gleam/erlang/process
import gleamdb
import gleamdb/fact
import gswarm/market

const scan_interval = 60_000 // Scan every minute

// Start the Cross-Market Intelligence scanner as a standalone process
pub fn start_link(db: gleamdb.Db) -> Result(process.Pid, String) {
  let pid = process.spawn_unlinked(fn() {
    loop(db)
  })
  Ok(pid)
}

fn loop(db: gleamdb.Db) {
  io.println("🔍 Cross-Market: Scanning for correlations...")
  let _ = scan_correlations(db)
  
  process.sleep(scan_interval)
  loop(db)
}

// --- Correlation Logic ---

fn scan_correlations(db: gleamdb.Db) -> Result(Nil, Nil) {
  // 1. Get active markets
  use markets <- result.try(market.get_active_prediction_markets(db))
  
  // 2. Generate unique pairs
  let pairs = unique_pairs(markets)
  
  // 3. Analyze each pair
  list.each(pairs, fn(pair) {
    let #(id_a, id_b) = pair
    analyze_pair(db, id_a, id_b)
  })
  
  Ok(Nil)
}

fn analyze_pair(db: gleamdb.Db, id_a: String, id_b: String) {
  // Fetch probability series for "YES" (or primary outcome)
  // TODO: Handle non-binary markets? defaulting to first outcome for now.
  let series_a_res = market.get_probability_series(db, id_a, "YES")
  let series_b_res = market.get_probability_series(db, id_b, "YES")
  
  case series_a_res, series_b_res {
    Ok(series_a), Ok(series_b) -> {
      let aligned = align_series(series_a, series_b)
      case list.length(aligned) > 30 { // Need sufficient data points
        True -> {
          let correlation = calculate_pearson(aligned)
          case float.absolute_value(correlation) >. 0.7 {
            True -> {
              io.println("🔗 Strong Correlation (" <> float.to_string(correlation) <> "): " <> id_a <> " <-> " <> id_b)
              store_correlation_fact(db, id_a, id_b, correlation)
            }
            False -> Nil
          }
        }
        False -> Nil
      }
    }
    _, _ -> Nil
  }
}

// Align two time series by bucketing timestamps to the nearest minute
fn align_series(a: List(#(Int, Float)), b: List(#(Int, Float))) -> List(#(Float, Float)) {
  let map_a = list.fold(a, dict.new(), fn(acc, item) {
    let bucket = item.0 / 60_000
    dict.insert(acc, bucket, item.1)
  })
  
  list.filter_map(b, fn(item) {
    let bucket = item.0 / 60_000
    case dict.get(map_a, bucket) {
      Ok(val_a) -> Ok(#(val_a, item.1))
      Error(_) -> Error(Nil)
    }
  })
}

fn calculate_pearson(data: List(#(Float, Float))) -> Float {
  let n = int.to_float(list.length(data))
  
  let #(sum_x, sum_y, sum_xy, sum_x2, sum_y2) = list.fold(data, #(0.0, 0.0, 0.0, 0.0, 0.0), fn(acc, p) {
    let #(x, y) = p
    let #(sx, sy, sxy, sx2, sy2) = acc
    #(
      sx +. x,
      sy +. y,
      sxy +. x *. y,
      sx2 +. x *. x,
      sy2 +. y *. y
    )
  })
  
  let numerator = n *. sum_xy -. sum_x *. sum_y
  let den_x = float.square_root(n *. sum_x2 -. sum_x *. sum_x) |> result.unwrap(0.0)
  let den_y = float.square_root(n *. sum_y2 -. sum_y *. sum_y) |> result.unwrap(0.0)
  
  case den_x *. den_y {
    0.0 -> 0.0
    denom -> numerator /. denom
  }
}

fn unique_pairs(items: List(String)) -> List(#(String, String)) {
  case items {
    [] -> []
    [head, ..tail] -> {
      let pairs = list.map(tail, fn(x) { #(head, x) })
      list.append(pairs, unique_pairs(tail))
    }
  }
}

fn store_correlation_fact(db: gleamdb.Db, id_a: String, id_b: String, corr: Float) {
  let lookup = fact.Lookup(#("market/id", fact.Str(id_a))) // Anchored to market A for now
  
  // Also store a global fact? Or just log?
  // Let's store it as a metric on Market A about Market B
  let facts = [
    #(lookup, "metric/correlation/" <> id_b, fact.Float(corr))
  ]
  
  let _ = gleamdb.transact(db, facts)
  Nil
}
