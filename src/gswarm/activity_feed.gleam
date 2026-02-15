import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/result
import gleam/list
import gleam/io
import gleam/string
import gleam/dynamic/decode

const data_url = "https://data-api.polymarket.com"

/// Fetch top traders from PolyMarket leaderboard.
pub fn fetch_leaderboard() -> Result(List(String), String) {
  let url = data_url <> "/v1/leaderboard?limit=20&window=all"
  let req_result = request.to(url) |> result.map_error(fn(_) { "Invalid URL" })
  use req <- result.try(req_result)
  
  // Anti-bot mitigation: PolyMarket requires User-Agent
  let req = request.set_header(req, "User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  
  case httpc.send(req) {
    Ok(resp) if resp.status == 200 -> {
      let decoder = {
        use address <- decode.field("proxyWallet", decode.string)
        decode.success(address)
      }
      json.parse(resp.body, decode.list(decoder))
      |> result.map_error(fn(e) { "JSON Decode Error: " <> string.inspect(e) })
    }
    Ok(resp) -> {
      Error("API returned status " <> string.inspect(resp.status))
    }
    _ -> Error("Failed to send HTTP request")
  }
}

pub fn start_with_dedup(_lb_actor, users: List(String)) {
  list.each(users, fn(user) {
     io.println("🕵️ ActivityFeed (Dedup): Tracking user [" <> user <> "]")
     // For now we just track them. In a real system we'd poll their activity.
  })
}
