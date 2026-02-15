# Gswarm 🐝

> "Sovereignty is the ability to detect signal before the market moves."

Gswarm is a **Sovereign Alpha Extraction Engine** built on GleamDB. Its sole purpose is to identify "Insider" traders—those who consistently trade before probability spikes—and copytrade them with **$10 Micro-Execution** stability.

## 🚀 Pillars of the Alpha Swarm
1.  **Insider Detection**: Real-time analysis of "Lead-Time Lag" to identify traders with systemic information advantage.
2.  **Competence Verification**: Brier-calibrated scoring to distinguishing true insiders from lucky noise.
3.  **Micro-Execution ($10)**: Optimized execution logic for tiny capital, minimizing spread impact and fees.
4.  **Silicon Saturation**: High-throughput ingestion (10k+ events/sec) to catch every tick and trade.
5.  **Sharded Sovereignty**: Horizontal scaling across 12-16 logical shards on Apple Silicon.
6.  **Prediction Market Edge**: Focusing on event outcome probabilities (0.0-1.0) rather than spot prices.
7.  **Sovereign Intelligence**: Autonomous self-correction via continuous Brier calibration.
8.  **Configurable Parallelism**: Tunable query parallelism via GleamDB's `Config` API — adjust thresholds per workload.

## 🛠️ Implementation Details
- **`gswarm.gleam`**: Orchestrator for leader boot and cluster heartbeat.
- **`market.gleam`**: Defines `Market` and `Tick` entities (Entity-per-Tick model) with deterministic IDs.
- **`ticker.gleam`**: High-frequency data generator (Silicon Saturation).
- **`reflex.gleam`**: Datalog subscription logic (Reactive Reflexes).
- **`context.gleam`**: Vector similarity search (Vector Sovereignty).

## ⚙️ GleamDB Configuration
Tune parallelism for high-throughput ingestion workloads:
```gleam
import gleamdb
import gleamdb/shared/types

// Lower threshold for faster parallel kickin during tick storms
gleamdb.set_config(db, types.Config(
  parallel_threshold: 200,
  batch_size: 50,
))
```

## 🧪 Running the Simulation
```bash
gleam run
```

---
*Built as a reference implementation for GleamDB 🧙🏾‍♂️*
