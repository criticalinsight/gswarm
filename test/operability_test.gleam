import gleeunit/should
import gleam/http/request
import gleam/httpc
import gleam/erlang/process
import gleam/dict
import gleamdb/sharded

import gswarm/http
import gswarm/node.{Leader}
import gswarm/shard_manager
import gswarm/registry_actor

pub fn main() {
  metrics_test()
}

pub fn metrics_test() {
  // 1. Setup Mock Context
  let shard_count = 1
  let registry = shard_manager.new_registry(shard_count)
  let assert Ok(actor) = registry_actor.start(shard_count)
  
  // We need a dummy DB for the context but it won't be used by http
  // Actually NodeContext needs shards dict.
  // We can just create a dummy ShardedContext.
  
  let db = sharded.ShardedDb(
    shards: dict.new(),
    shard_count: shard_count,
    cluster_id: "test_metrics"
  )
  
  let ctx = node.ShardedContext(
    role: Leader,
    db: db,
    cluster_id: "test_metrics",
    registry: registry,
    registry_actor: actor
  )
  
  // 2. Start Server
  // We need to run it on a distinct port to avoid collision with main app if running?
  // Use 8081 for test.
  http.start_server(8081, ctx)
  process.sleep(100)
  
  // 3. Request Metrics
  let assert Ok(req) = request.to("http://localhost:8081/metrics")
  let assert Ok(resp) = httpc.send(req)
  
  resp.status |> should.equal(200)
  
  // 4. Verify JSON content
  // Since we have no shards or HLL data inserted, expect zeros.
  // But we want to ensure basic structure.
  let assert Ok(json_string) = valid_body(resp.body)
  // Simple substring check or full decode
  // Let's decode to dynamic or just check key presence.
  
  // Checking for keys
  let contains_hll = string_contains(json_string, "hll_cardinality")
  let contains_shard = string_contains(json_string, "shard_count")
  
  contains_hll |> should.be_true()
  contains_shard |> should.be_true()
}

fn valid_body(body: String) -> Result(String, Nil) {
  Ok(body)
}

import gleam/string

fn string_contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
