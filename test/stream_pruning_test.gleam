import gleam/erlang/process
import gleam/list
import gleam/option.{None}
import gleeunit/should
import gleamdb
import gleamdb/fact
import gswarm/pruner

pub fn stream_pruning_test() {
  let db = gleamdb.new()
  
  // 1. Configure schema
  let config = fact.AttributeConfig(unique: False, component: False, retention: fact.All, cardinality: fact.Many, check: None)
  let _ = gleamdb.set_schema(db, "test/ts", config)
  let _ = gleamdb.set_schema(db, "test/val", config)
  
  // 2. Start pruner with 100ms retention and 50ms interval
  let assert Ok(p) = pruner.start(db, 100, 50, "test/ts")
  
  // 3. Ingest some facts
  let now = os_now_ms()
  let entity_1 = fact.Uid(fact.EntityId(1))
  let _ = gleamdb.transact(db, [
    #(entity_1, "test/ts", fact.Int(now)),
    #(entity_1, "test/val", fact.Int(42))
  ])
  
  // Verify it exists
  let res = gleamdb.query(db, [gleamdb.p(#(types.Var("e"), "test/val", types.Val(fact.Int(42))))])
  list.length(res) |> should.equal(1)
  
  // 4. Wait for it to expire
  process.sleep(200)
  
  // 5. Verify it's gone
  let res_after = gleamdb.query(db, [gleamdb.p(#(types.Var("e"), "test/val", types.Val(fact.Int(42))))])
  list.length(res_after) |> should.equal(0)
  
  process.send(p, pruner.Stop)
}

@external(erlang, "os", "system_time")
fn os_now_ms_native() -> Int

fn os_now_ms() -> Int {
  os_now_ms_native() / 1_000_000
}

import gleamdb/shared/types
