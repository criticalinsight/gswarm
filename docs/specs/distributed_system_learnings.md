# Learnings: Distributed System Engineering (Phase 39–45)

> "Reliability is not an outcome; it is a discipline."

## 1. Test Runner & Process Linking (`Exit(Killed)`)

**Problem**: Distributed integration tests (`gleeunit`) were crashing efficiently but silently with `Exit(Killed)`.
**Context**: In Gleam/OTP, `actor.start` links the new process to the caller (the test runner).
**Discovery**: When we manually stopped a node with `process.kill(db_pid)`, the exit signal propagated to the linked test runner, causing the entire test suite to abort.
**Solution**: **Unlink before Kill**.
```gleam
let assert Ok(pid) = process.subject_owner(ctx.db)
process.unlink(pid) // Break the link to the test runner
process.kill(pid)   // Now safe to kill
```
**Takeaway**: In testing harnesses suitable for distributed systems, manual lifecycle management (`start`/`stop`) requires careful handling of OTP links.

## 2. HyperLogLog & Small Set Bias

**Problem**: The `hll.estimate` function returned `763` for a known cardinality of `50`.
**Context**: Standard HyperLogLog algorithms have a large bias for small cardinalities (Linear Counting range).
**Discovery**: Our initial implementation lacked the "LinearCounting" correction step recommended by the original Flajolet et al. paper for when $E < \frac{5}{2}m$.
**Solution**: Implement LinearCounting hybrid approach.
```gleam
case raw_estimate <=. 2.5 *. m {
  True -> linear_counting(m, v) // 50 (Exact)
  False -> raw_estimate
}
```
**Takeaway**: Probabilistic data structures require hybrid implementations to be useful across the full range of cardinalities.

## 3. Global Registry & Race Conditions

**Problem**: `global` registry collisions during rapid shard startup in tests.
**Context**: Starting multiple nodes (shards) in the same BEAM VM within milliseconds.
**Discovery**: The Erlang `global` module is eventually consistent and can be chatty. Rapidly registering/unregistering the same names (like `gleamdb_leader` in tests) can lead to message queues backing up or "name stealing."
**Solution**:
1.  **Ordered Shutdown**: Unregister explicitly *before* killing the process.
2.  **Unique Test Names**: Use unique cluster IDs for every test run (e.g., `test_bloom_shards` vs `test_shards_1`).

## 4. Build Stability vs. Deprecations ✅ RESOLVED

**Problem**: `list.range` is deprecated in favor of `int.range`.
**Context**: Bulk replacing logic caused build failures due to arity mismatches and missing imports.
**Resolution**: All calls migrated to `int.range` with exclusive upper-bound adjustment.

## 5. GleamDB v2.0 Dogfooding Friction (Phase 32)

**Problem**: The `Db` vs `DbState` dichotomy.
**Context**: Integrating `with_facts` for speculative trading.
**Discovery**: `with_facts` returns a `DbState` (pure value). The public `gleamdb.query` API requires a `Db` (actor handle). This creates a leakage where Speculative Soul queries MUST use the internal `engine.run(state, ...)` rather than the standard `query` API.
**Takeaway**: Future versions should unify querying for both persistent handles and speculative values.

## 6. Graph Predicate Type Safety

**Problem**: "Silent Empty Results" in cycle detection.
**Discovery**: `graph_intel.gleam` produced zero cycles because trade edges were initially stored as `String` market IDs. Graph algorithms strictly require `Ref(EntityId)`.
**Solution**: Standardize on `fact.Ref(fact.EntityId(shard_key(market_id)))` for all graph edges.
