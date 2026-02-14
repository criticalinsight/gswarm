# Learnings: GleamDB v2.0.0 Dogfood in Gswarm (Phases 27-32)

> "Simplicity is not about making things easy. It is about untangling complexity." — Rich Hickey

## Context

Dogfooded GleamDB v2.0.0 (all features since v1.7.1) in Gswarm by creating `graph_intel.gleam` — a 270-line module exercising every new API. Build: clean. Tests: 15 pass, 0 failures.

## Feature-by-Feature Learnings

### 1. Speculative Soul (`with_facts`) — Phase 27

**What works well:** The API is dead simple — `gleamdb.with_facts(state, facts)` returns a forked `DbState` that can be queried immediately. Perfect for paper trading simulations.

**DX Friction:** `with_facts` takes a `DbState`, not a `Db`. This means you need `gleamdb.get_state(db)` first, then query the *original* `db` handle (not the speculative state) because speculative state isn't an actor. This is correct by design (speculative = pure value, not a process) but the ergonomic gap between `Db` and `DbState` is a recurrent surprise.

**Recommendation:** Consider a `gleamdb.with_spec(db, facts, fn(spec_db) -> a)` wrapper that handles the state extraction and provides a temporary queryable handle.

### 2. Navigator (`explain`) — Phase 28

**What works well:** Completely automatic — existing Gswarm queries benefit from cost-based reordering without any code changes. The `explain` function provides visibility into what the planner decided.

**DX Friction:** None. This is the ideal "invisible upgrade" — zero effort, automatic benefit.

**Learning:** Transparent optimizations are the highest-leverage features. The user gets better performance by upgrading the dependency, nothing more.

### 3. Chronos (`transact_at`, `as_of_valid`) — Phase 29

**What works well:** `transact_at(db, facts, valid_time)` is the right API for event-sourced systems. Gswarm naturally has a "when did this tick happen" vs "when did we record it" distinction, and bitemporal support maps perfectly.

**DX Friction:** `as_of_valid` returns a flat `QueryResult`, same as `query`. This is correct but makes it easy to forget you're querying a temporal slice — results look identical. No metadata about the temporal bounds is returned.

**Recommendation:** Consider returning a `TemporalQueryResult` or attaching metadata (e.g., the valid_time bound used) so debugging temporal queries is easier.

### 4. Completeness (`register_composite`) — Phase 30

**What works well:** One-liner constraint registration: `register_composite(db, ["tick/market_id", "tick/timestamp"])`. Prevents silently duplicate market ticks from corrupting the data.

**DX Friction:** Constraints are registered imperatively, not declaratively in the schema. This means you have to call `register_composite` at startup every time — it's not persisted. If a shard restarts, composites are lost.

**Recommendation:** Persist composite definitions in the schema alongside `AttributeConfig`, or offer `set_schema_composite`.

### 5. Sovereign Intelligence (Aggregates) — Phase 31

**What works well:** `q.avg`, `q.count`, `q.sum` compose naturally with the query builder. Writing `q.avg("avg_price", "price", filter_clauses)` reads exactly like the intent.

**DX Friction:** The filter clauses inside aggregates use raw `types.Positive(#(...))` tuples, not the `q.where()` builder, because aggregates take `List(BodyClause)` directly. This creates a style mismatch:
```gleam
// Outside aggregate: fluent builder
q.where(q.v("t"), "tick/price", q.v("price"))

// Inside aggregate: raw tuples
types.Positive(#(types.Var("t"), "tick/price", types.Var("price")))
```

**Recommendation:** Add `q.clause(entity, attr, value) -> BodyClause` to unify the inner/outer filter syntax.

### 6. Graph Algorithm Suite (9 Predicates) — Phase 32

**What works well:** Every predicate has the same pattern — add a `q.predicate_name(...)` call to the builder, done. The DSL is remarkably consistent. Composing graph predicates with `order_by` and `limit` for top-K queries is natural:
```gleam
q.betweenness_centrality("edge", "node", "score")
|> q.order_by("score", Desc)
|> q.limit(5)
```

**DX Friction:** Graph predicates require edges to be stored as `Ref(EntityId)` values. If your edges are stored as `Str` identifiers (common in JSON-sourced data), you must convert to `Ref` first. No helpful error message when this happens — you just get empty results.

**Recommendation:** Add a compile-time or query-time warning when a graph predicate's edge attribute contains non-Ref values: "⚠ attribute 'trades_with' contains 0 Ref values — graph predicates require Ref edges."

## Summary Table

| Phase | Feature | DX Score | Integration Effort | Gswarm Value |
|-------|---------|----------|-------------------|--------------|
| 27 | Speculative Soul | 7/10 | Low | High (paper trading) |
| 28 | Navigator | 10/10 | Zero | Medium (auto-optimize) |
| 29 | Chronos | 8/10 | Low | High (market replay) |
| 30 | Completeness | 6/10 | Low | High (data integrity) |
| 31 | Aggregates | 7/10 | Low | High (cross-market stats) |
| 32 | Graph Suite | 8/10 | Low | Ultra (insider detection) |

## Architectural Observations

1. **The `Db` vs `DbState` split** is the single biggest source of API confusion. `Db` is an actor handle; `DbState` is a pure value. Most functions take `Db`, but `with_facts` takes `DbState`. Users trip on this boundary constantly.

2. **Graph predicates are the killer feature** for Gswarm. The ability to express `q.cycle_detect("trades_with", "cycle")` in a single line replaces what would be ~100 lines of manual BFS/DFS in the application layer.

3. **Aggregates + Graph = compound intelligence.** The composition `q.betweenness_centrality(...) |> q.order_by(...)` is the kind of expressiveness that Datomic charges $20K/year for. It works out of the box in GleamDB.

4. **Missing: `q.where` inside aggregate filters.** The style split between builder DSL and raw tuples is the #1 papercut. Fixing this would raise all aggregate DX scores by 2 points.

---
*Generated from dogfood session: 2026-02-15*
