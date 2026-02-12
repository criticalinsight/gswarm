# Gswarm 🐝

> "Sovereignty is the ability to maintain context over time."

Gswarm is a distributed trading swarm simulation built on **GleamDB**. It demonstrates high-frequency ingestion, reactive triggers, and semantic similarity search in a resilient, local-first architecture.

## 🚀 Pillars of the Swarm
1.  **Silicon Saturation**: Throttled ingestion @ ~1000 ticks/sec into ETS-backed indices.
2.  **Reactive Reflexes**: Autonomous cross-market Datalog joins that trigger price-band alerts.
3.  **Vector Sovereignty**: Semantic market clustering using GleamDB's native vector similarity.
4.  **Logical Perfection**: Temporal auditing via `as_of` and structured entity snapshots via `pull`.
5.  **Memory Safety**: Retention policies prevent memory exhaustion during long-running simulations.

## 🛠️ Implementation Details
- **`gswarm.gleam`**: Orchestrator for leader boot and cluster heartbeat.
- **`market.gleam`**: EAVT schema definition and retention configuration.
- **`ticker.gleam`**: High-frequency data generator (Silicon Saturation).
- **`reflex.gleam`**: Datalog subscription logic (Reactive Reflexes).
- **`context.gleam`**: Vector similarity search (Vector Sovereignty).

## 🧪 Running the Simulation
```bash
gleam run
```

---
*Built as a reference implementation for GleamDB 🧙🏾‍♂️*
