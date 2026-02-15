# Learnings: GleamDB v2.0.0 Dogfood in Gswarm (Phases 27-32)

> "Simplicity is not about making things easy. It is about untangling complexity." — Rich Hickey

## Context

Dogfooded GleamDB v2.0.0 (all features since v1.7.1) in Gswarm by creating `graph_intel.gleam` — a 270-line module exercising every new API. Build: clean. Tests: 15 pass, 0 failures.

## Feature-by-Feature Learnings

### 1. Speculative Soul (`with_facts`) — Phase 27

**What works well:** The fundamental capability is powerful — `gleamdb.with_facts(state, facts)` returns a forked dataframe that respects all consistent database semantics.

**DX Friction:** **The `Db` vs `DbState` impedance mismatch.**
- `gleamdb.query(db, ...)` takes an actor handle (`Db`).
- `gleamdb.with_facts(state, ...)` takes/returns a pure value (`DbState`).
- There is **no** `gleamdb.query_state(state, ...)` in the public API.
- Users must manually import `gleamdb/engine` and call `engine.run(state, ...)` to query the speculative fork. This leaks internal modules into application code.

**Recommendation:** Expose `gleamdb.query_state(state, clauses)` or `gleamdb.with_spec(db, facts, fn(spec_db_handle) -> a)`.

### 2. Navigator (`explain`) — Phase 28

**What works well:** "Invisible upgrade." Existing Gswarm queries automatically benefited from cost-based join reordering. `explain` confirms the planner is making intelligent choices (e.g., pushing most selective filters first).

**DX Friction:** Zero.

**Learning:** Transparent optimizations are the highest-leverage features.

### 3. Chronos (`transact_at`, `as_of_valid`) — Phase 29

**What works well:** `transact_at` allows backfilling historical data with correct valid-time semantics, essential for Gswarm's market replay.

**DX Friction:** **Identify Crisis.** `as_of_valid` returns a standard `List(Dict(String, Value))`. There is no metadata indicating *which* valid-time slice generated this result. When debugging a replay loop, it is easy to lose track of the temporal context.

**Recommendation:** Return a `TemporalQueryResult` wrapper or include metadata.

### 4. Completeness (`register_composite`) — Phase 30

**What works well:** Prevents data corruption from duplicate market ticks.

**DX Friction:** **Ephemeral by Design.** Unlike `store_rule` (which persists the rule to disk as a `_rule/content` fact), `register_composite` (and `register_predicate`) only updates the in-memory actor state.
- **Consequence:** Composites must be re-registered on every node restart.
- **Risk:** If a node restarts and accepts writes *before* registration code runs, data integrity is compromised.

**Recommendation:** Persist composite definitions to the log, similar to `store_rule` or schema definitions.

### 5. Sovereign Intelligence (Aggregates) — Phase 31

**What works well:** `q.avg`, `q.sum` etc. are expressive and compose well with the builder.

**DX Friction:** **Filter Verbosity.** Aggregates accept `List(BodyClause)` for filtering. The `q` builder returns a `QueryBuilder`. To use the builder for an aggregate filter, you must do this:

```gleam
q.avg("price", "p", 
  q.new() 
  |> q.where(q.v("p"), "tick/market", q.s("BTC"))
  |> q.to_clauses()
)
```
This is verbose compared to a hypothetical `q.clause(...)`.

**Recommendation:** Add `q.clause(e, a, v) -> BodyClause` or allow `QueryBuilder` directly in aggregates.

### 6. Graph Algorithm Suite (9 Predicates) — Phase 32

**What works well:** The DSL is consistent and powerful. Topological sort (`q.topological_sort`) and Cycle Detection (`q.cycle_detect`) replace hundreds of lines of application logic.

**DX Friction:** **Strict Typing.** Graph predicates require edges to be `Ref(EntityId)`. In many ETL pipelines (like Gswarm's JSON ingestion), edges essentially start as `String` identifiers. If you query `q.cycle_detect` on a string-based edge attribute, you get **empty results with no warning**.

**Recommendation:** `engine` should log a warning or return an error if a graph predicate scans an attribute that contains non-Ref values.

## Summary Table

| Phase | Feature | DX Score | Integration Effort | Gswarm Value |
|-------|---------|----------|-------------------|--------------|
| 27 | Speculative Soul | 6/10 | Medium (requires internal API) | High |
| 28 | Navigator | 10/10 | Zero | Medium |
| 29 | Chronos | 8/10 | Low | High |
| 30 | Completeness | 5/10 | Medium (requires init boilerplate) | High |
| 31 | Aggregates | 7/10 | Low | High |
| 32 | Graph Suite | 8/10 | Low | Ultra |

## Strategic Recommendations

1. **Unify the `Db` / `DbState` Experience:** The split is logical (Actor vs Value) but ergonomic poison. Create a specific "Speculative Handle" abstraction that looks like `Db` but wraps a pure state, so users can use the same `query` API for both real and speculative worlds.

2. **Persistence for All Schema constraints:** `register_composite` and `register_predicate` should not be second-class citizens compared to `store_rule`. Persist them.

3. **Type-Safe Graph Edges:** Add a "Graph Mode" to schema attributes (e.g., `one_ref`, `many_ref`) that enforces `Ref` type, preventing the "silent empty result" failure mode for graph queries.

---
*Generated from dogfood session: 2026-02-15*
