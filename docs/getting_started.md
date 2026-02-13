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

2. **Run the Simulation**:
   ```bash
   gleam run
   ```

## 🧪 What to Expect
Upon running, the swarm will:
1.  **Initialize**: Join the `gswarm_mainnet` fabric as a Leader.
2.  **Saturation**: Start two ticker loops (`m_1`, `m_2`) ingesting prices.
3.  **Reflexes**: Spawn watcher processes that alert on price detected via Datalog subscriptions.
4.  **Context**: Periodic pulses will audit terminal output with market similarity and history audits.

## 📖 Further Reading
- [Architecture](docs/architecture.md): Deep dive into the Rama Pattern and actor model.
- [Capabilities](docs/capabilities.md): Overview of Vector Sovereignty and Memory Safety.
