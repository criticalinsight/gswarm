# Gswarm Capabilities 🐝🛡️

## 1. High-Frequency Market Ingestion
Demonstrates the **Silicon Saturation** pillar.
- **Goal**: Ingest 10,000+ events/second.
- **Mechanism**: Sharded batching (`ingest_batcher.gleam`) and parallel Mnesia writers.

## 2. Insider Intelligence
Demonstrates the **Insider Detection** pillar.
- **Goal**: Identify traders with systemic information advantage (Lead-Time Lag).
- **Mechanism**: Real-time correlation of trade timestamps against probability inflection points.

## 3. Micro-Execution Edge
- **Goal**:  Stable returns on tiny capital ($10).
- **Mechanism**: Spread-aware execution logic in `copytrader.gleam` that rejects negative-EV copies.

## 4. Adaptive Strategy Selection
- **Goal**: Hot-swap strategies based on real-time win-rates.
- **Mechanism**: `strategy_selector.gleam` identifies the best strategy for the current market regime.

## 5. Memory Safety & Active Paging
- **Goal**: Perpetual operation on restricted hardware (M2 Pro).
- **Mechanism**: `pruner.gleam` implements sliding-window history eviction.

## 6. Configurable Query Parallelism
- **Goal**: Tune concurrency to match hardware and workload.
- **Mechanism**: GleamDB's `Config(parallel_threshold, batch_size)` via `gleamdb.set_config`. Lower thresholds for tick storms, higher for analytical queries.

## 7. Graph-Theoretic Alpha Extraction
- **Goal**: Identify complex market structures (e.g., wash-trading, influence rings).
- **Mechanism**: GleamDB `Graph Suite` predicates like `cycle_detect` and `pagerank` running over "trades_with" edges.

## 8. Bitemporal Market Replay
- **Goal**: Deterministic analysis of past market regimes with historical precision.
- **Mechanism**: `Chronos` (v2.0) bitemporal queries using `as_of_valid` to separate tick ingestion time from market occurrence time.

## 9. Speculative Trade Simulation
- **Goal**: Risk-free "What-if" analysis of trade execution.
- **Mechanism**: `Speculative Soul` (`with_facts`) creates immediate, non-persistent state forks for querying hypothetical scenarios.

## 10. Invisible Query Optimization
- **Goal**: High-performance intelligence scans without manual query tuning.
- **Mechanism**: `Navigator` cost-based planner automatically optimizes join orders based on data statistics.
