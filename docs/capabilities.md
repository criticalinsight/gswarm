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
