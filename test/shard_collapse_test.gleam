import gleam/erlang/process
import gleam/dict
import gleeunit/should
import gswarm/shard_manager
import gswarm/registry_actor
import gswarm/node
import gleam/int

pub fn shard_collapse_test() {
  // 1. Start sharded context with 2 shards
  let assert Ok(ctx) = node.start_sharded(node.Lean, "collapse_test", 2)
  
  // 2. Collapse virtual shard 1 into physical shard 0
  process.send(ctx.registry_actor, registry_actor.CollapseShard(1, 0))
  
  // 3. Verify routing via registry actor
  let reply = process.new_subject()
  process.send(ctx.registry_actor, registry_actor.GetRegistry(reply))
  let registry = process.receive(reply, 1000) |> should.be_ok()
  
  // Virtual shard 1 should map to physical 0
  dict.get(registry.shard_map, 1) |> should.equal(Ok(0))
  
  // 4. Record a market that would normally go to shard 1
  let market_id = find_market_for_shard(1, 2)
  process.send(ctx.registry_actor, registry_actor.RecordMarket(market_id))
  
  // 5. Verify Bloom filter on shard 0 contains the market
  let reply_reg = process.new_subject()
  process.send(ctx.registry_actor, registry_actor.GetRegistry(reply_reg))
  let registry_after = process.receive(reply_reg, 1000) |> should.be_ok()
  
  shard_manager.market_might_exist(registry_after, market_id) |> should.be_true()
  
  // Verify physical routing
  let physical = shard_manager.resolve_physical_shard(registry_after, 1)
  physical |> should.equal(0)
  
  node.stop_sharded(ctx)
}

fn find_market_for_shard(target_shard: Int, shard_count: Int) -> String {
  find_market_loop(0, target_shard, shard_count)
}

fn find_market_loop(attempt: Int, target: Int, count: Int) -> String {
  let id = "m_" <> int.to_string(attempt)
  case shard_manager.get_shard_id(id, count) == target {
    True -> id
    False -> find_market_loop(attempt + 1, target, count)
  }
}
