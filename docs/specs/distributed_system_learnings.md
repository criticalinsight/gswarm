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

## 4. Build Stability vs. Deprecations

**Problem**: `list.range` is deprecated in favor of `int.range`.
**Context**: Bulk replacing logic caused build failures due to arity mismatches and missing imports in the current environment.
**Decision**: We prioritized a **passing build** and **green tests** over zero warnings.
**Takeaway**: Refactoring core standard library usage should be treated as a major migration, not a quick cleanup, especially when it touches every module in the system.
