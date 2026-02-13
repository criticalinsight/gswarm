# Getting Started with Gswarm 🐝

Follow these steps to initialize the sovereign fabric and run the trading simulation.

## 🛠️ Prerequisites
- **Erlang/OTP 27**: Required for the Silicon Saturation (ETS) layer.
- **Gleam v1.6.3+**: Required for the Datalog engine and type safety.
- **GleamDB**: Ensure `gleamdb` is available at the relative path specified in `gleam.toml`.

## 🚀 Quick Start

1. **Clone & Setup**:
   ```bash
   git clone <gswarm-repo-url>
   cd gswarm
   gleam deps download
   ```

2. **Run the Sharded Fabric**:
   ```bash
   # Start as a Leader with 4 shards (Default)
   gleam run
   
   # Start in Lean Mode (1 Shard, M2 Pro optimized)
   gleam run -- --lean
   
   # Start as a Follower (Joins existing cluster)
   gleam run follower
   ```

## 🧪 What to Expect
Upon running, the swarm will:
1.  **Initialize**: Join the sharded fabric and boot the **Supervision Tree**.
2.  **Ingestion**: Start the `market_feed` (Manifold) and `live_ticker` (Coinbase) loops.
3.  **Probabilistic Tracking**: Logs will show unique market cardinality (HLL) and signal frequency (CMS).
4.  **Intelligence Dashboard**: Visit `http://localhost:8080/metrics` to see the live operability state.
5.  **Adaptive Trading**: The `strategy_selector` will hot-swap trader strategies based on rolling win-rates.

## 📖 Further Reading
- [Architecture](docs/architecture.md): Deep dive into the Rama Pattern and actor model.
- [Capabilities](docs/capabilities.md): Overview of Vector Sovereignty and Memory Safety.
