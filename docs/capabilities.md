# Gswarm Capabilities 🐝🛡️

## 1. High-Frequency Market Ingestion
Demonstrates the **Silicon Saturation** pillar.
- **Goal**: Ingest 10,000+ events/second.
- **Mechanism**: Sharded batching (`ingest_batcher.gleam`) and parallel Mnesia writers.

## 2. Probabilistic Intelligence
Demonstrates the **Lean Optimization** pillar.
- **Goal**: Track global cardinality and frequency with O(1) space.
- **Mechanism**: HyperLogLog (`hll.gleam`) and Count-Min Sketch (`cms.gleam`) for signal detection.

## 3. Prediction Market Analytical Edge
- **Goal**: Calibrated probability forecasting (0.0–1.0).
- **Mechanism**: Brier scoring (`resolution.gleam`) and cross-market correlation (`cross_market.gleam`).

## 4. Adaptive Strategy Selection
- **Goal**: Hot-swap strategies based on real-time win-rates.
- **Mechanism**: `strategy_selector.gleam` identifies the best strategy for the current market regime.

## 5. Memory Safety & Active Paging
- **Goal**: Perpetual operation on restricted hardware (M2 Pro).
- **Mechanism**: `pruner.gleam` implements sliding-window history eviction.
