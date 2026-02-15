# Learnings: GleamDB v2.0.0 Dogfooding in Gswarm

**Mission Status**: ✅ MISSION ACCOMPLISHED (Surgical Verification Complete)
**Dogfooding Context**: Sharded Gswarm Fabric (Lean Mode - 1 Shard)
**Features Exercised**: Phases 27, 28, 29, 30, 31, 32

---

## Executive Summary
GleamDB v2.0.0 has been successfully integrated into Gswarm and verified under live execution. All 6 major features added since v1.7.1 were exercised via the `graph_intel.gleam` module. After initial friction regarding attribute naming and ingestion timing, robust debug instrumentation proved that the engine correctly persists facts, calculates aggregates, and detects graph cycles in a sharded environment.

## Feature-by-Feature Learnings

### 1. Speculative Soul (`with_facts`)
- **Score**: 8/10
- **Learnings**: Extremely powerful for "What-if" market simulations. 
- **Friction**: The biggest DX trap is that `with_facts` returns a `DbState` (pure value), but `gleamdb.query` ONLY accepts `Db` (the actor handle). 
- **Verification**: Fixed in `graph_intel.simulate_trade` by using `engine.run` directly on the speculative state.

### 2. Navigator (Cost-Based Planner)
- **Score**: 10/10
- **Learnings**: An "invisible" win. `explain` confirms the planner automatically prioritized selectively small datasets (`insider/confidence`). Zero code changes required for massive efficiency gains.

### 3. Chronos (`as_of_valid`)
- **Score**: 8/10
- **Learnings**: `transact_at` allows backfilling historical data with correct valid-time semantics.
- **Friction**: Lack of temporal metadata in query results makes deep-replay debugging tedious.

### 4. Completeness (`register_composite`)
- **Score**: 6/10
- **Learnings**: Essential for preventing duplicate ticks.
- **Friction**: Ephemeral by design. Unlike rules, composites are not persisted and must be re-registered on node restart (verified in `gswarm.main`).

### 5. Sovereign Intelligence (Aggregates)
- **Score**: 9/10
- **Learnings**: `q.avg` and `q.count` provide high-level insights for paper trading stats with push-down performance.
- **Verification**: Successfully aggregated 130+ live probability ticks.
- **Note**: "Attribute Mismatch Silent Failure" — Ensure standardized attributes (like `tick/probability`) are used.

### 6. Graph Algorithmic Suite
- **Score**: 9.5/10
- **Learnings**: `CycleDetect` and `StronglyConnectedComponents` are game-changers for insider detection.
- **Verification**: `detect_wash_trades` successfully identified a seeded cyclic trading ring (A->B->C->A).
- **Friction**: Strictly requires `Ref(EntityId)` for edges. Providing string IDs leads to silent empty results.

---

## Summary Table

| Phase | Feature | DX Score | Integration Effort | Gswarm Value |
|-------|---------|----------|-------------------|--------------|
| 27 | Speculative Soul | 8/10 | Medium | High |
| 28 | Navigator | 10/10 | Zero | Medium |
| 29 | Chronos | 8/10 | Low | High |
| 30 | Completeness | 6/10 | Medium | High |
| 31 | Aggregates | 9/10 | Low | High |
| 32 | Graph Suite | 9.5/10 | Low | Ultra |

## Strategic Recommendations

1. **Unify `Db` / `DbState` Querying**: The public API needs a unified way to query both actor handles and pure state values without leaking `engine` internals.
2. **Persistent Constraints**: `register_composite` should be durable.
3. **Graph Type Safety**: `engine` should log warnings if graph predicates hit non-Ref attributes.

---
*Generated from dogfood session: 2026-02-15*
