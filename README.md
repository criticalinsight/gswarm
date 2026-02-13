# Gswarm 🐝

> "Sovereignty is the ability to maintain context over time."

Gswarm is a distributed trading swarm simulation built on **GleamDB**. It demonstrates high-frequency ingestion, reactive triggers, and semantic similarity search in a resilient, local-first architecture.

## 🚀 Pillars of the Swarm
1.  **Sharded Sovereignty**: Horizontal scaling across 12-16 logical shards, optimized for Apple Silicon (M2 Pro).
2.  **Silicon Saturation**: High-throughput ingestion target of 10k+ events/sec via batching and Mnesia WAL.
3.  **Probabilistic Intel**: Real-time cardinality (HLL) and frequency (CMS) tracking with Bloom filter deduplication.
4.  **Prediction Market Edge**: Refocused from spot prices to event outcome probabilities (0.0-1.0) with Brier calibration.
5.  **Reactive Reflexes**: Autonomous cross-shard Datalog joins and autohealing failover loops.
6.  **Lean Optimization**: Adaptive shard-collapsing and lazy analytics for restricted local-first environments.

## 🛠️ Implementation Details
- **`gswarm.gleam`**: Orchestrator for leader boot and cluster heartbeat.
- **`market.gleam`**: Defines `Market` and `Tick` entities (Entity-per-Tick model) with deterministic IDs.
- **`ticker.gleam`**: High-frequency data generator (Silicon Saturation).
- **`reflex.gleam`**: Datalog subscription logic (Reactive Reflexes).
- **`context.gleam`**: Vector similarity search (Vector Sovereignty).

## 🧪 Running the Simulation
```bash
gleam run
```

---
*Built as a reference implementation for GleamDB 🧙🏾‍♂️*
