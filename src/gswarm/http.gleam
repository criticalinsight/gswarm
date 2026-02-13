import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/bytes_tree
import gleam/json

import mist
import gswarm/node.{type ShardedContext, Leader, Follower, Lean, LeaderEphemeral}
import gswarm/registry_actor

pub fn start_server(port: Int, ctx: ShardedContext) {
  let assert Ok(_) =
    mist.new(fn(req) { handle_request(req, ctx) })
    |> mist.port(port)
    |> mist.start

  Nil
}

fn handle_request(req: Request(mist.Connection), ctx: ShardedContext) -> Response(mist.ResponseData) {
  case request.path_segments(req) {
    ["metrics"] -> handle_metrics(ctx)
    _ -> 
      response.new(404)
      |> response.set_body(mist.Bytes(bytes_tree.new()))
  }
}

fn handle_metrics(ctx: ShardedContext) -> Response(mist.ResponseData) {
  // Query registry for metrics
  let subject = ctx.registry_actor
  let reply_subject = process.new_subject()
  
  process.send(subject, registry_actor.GetMetrics(reply_subject))
  
  // Wait for reply (default 1000ms)
  let metrics = case process.receive(reply_subject, 1000) {
    Ok(m) -> m
    Error(_) -> registry_actor.Metrics(0, 0, 0) // Fallback or 500
  }
  
  let role_str = case ctx.role {
       Leader -> "Leader"
       Follower -> "Follower" 
       Lean -> "Lean"
       LeaderEphemeral -> "LeaderEphemeral"
  }

  let json_body = json.object([
    #("hll_cardinality", json.int(metrics.hll_cardinality)),
    #("shard_count", json.int(metrics.shard_count)),
    #("bloom_size_bits", json.int(metrics.bloom_size_bits)),
    #("node_role", json.string(role_str)),
    #("cluster_id", json.string(ctx.cluster_id))
  ])
  
  response.new(200)
  |> response.set_header("content-type", "application/json")
  |> response.set_body(mist.Bytes(bytes_tree.from_string(json.to_string(json_body))))
}
