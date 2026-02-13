# PRD: Backtesting & Paper Trading Engine

**Status**: Draft
**Priority**: P0
**Owner**: Rich Hickey (Architect)

## Overview
Gswarm currenty ingests real-time "Alpha" vectors (Volatility, Trend). We need to close the loop by:
1. **Backtesting**: Replaying historical vectors through a strategy function to calculate theoretical PnL.
2. **Paper Trading**: Executing strategy decisions in-memory against live price feeds to validate real-time performance.
3. **High-Dimensional Analytics**: Expanding the Alpha vector from 3 to 50+ metrics to provide the AI Analyst with richer features for pattern matching.

## User Stories
- **As a Quant**, I want to replay the last 7 days of SOL market movements through my 'Momentum' strategy so that I can see its Sharpe ratio.
- **As a Developer**, I want to run a 'Paper Trader' agent that logs buy/sell signals without risking real capital.
- **As an Analyst**, I want the market state to include deep volume and momentum metrics to improve similarity searching.

## Acceptance Criteria
### Backtesting (Simulation)
- **Given** a historical range of Ticks in GleamDB.
- **When** the Backtest engine is run with a Strategy function.
- **Then** it must produce a `BacktestResult` containing total return, drawdown, and win rate.
- **Failure Path**: If data is missing or gapped, the engine must warn or interpolate rather than producing false stats.

### Paper Trading (Reality Simulation)
- **Given** a live stream of Ticks from `live_ticker`.
- **When** the `paper_trader` actor receives a tick.
- **Then** it must update its virtual wallet if the strategy triggers a trade.
- **Then** it must log the "Slippage" (difference between signaled price and simulated fill).

## Technical Implementation

### Database Schema
We will add an `Outcome` entity type to relate a `MarketState` to the future.
```gleam
#(fact.Lookup(#("market/id", fact.Str(id))), "tick/outcome/next_5m", fact.Float(price_change))
```

### Metrics Expansion (The "Alpha-50")
The `live_ticker` will be extended to calculate:
- **Trend**: SMA(10, 20, 50), EMA, MACD.
- **Momentum**: RSI, Stochastic, ROC.
- **Volatility**: ATR, Bollinger Bands, StdDev.
- **Volume**: OBV, CMF, VWAP.
- **Context**: News Sentiment (Magnitude/Freq), Fractal Similarity.

### Visual Architecture
```mermaid
sequenceDiagram
    participant LT as Live Ticker
    participant DB as GleamDB (Mnesia)
    participant PT as Paper Trader
    participant BT as Backtest Engine

    LT->>DB: Ingest Vec(50) + Price
    LT->>PT: Emit Signal
    PT->>DB: Record Simulated Trade
    BT->>DB: Fetch History
    BT->>BT: Replay Logic
    BT->>Developer: Display Results
```

## Security & Validation
- **Sandboxing**: Paper trading actors MUST NOT have access to real exchange keys.
- **Input Validation**: Strategy functions must be total (handling all vector inputs without crashing).

## Pre-Mortem: Why will this fail?
- **Look-ahead Bias**: The backtester might accidentally use "future" metrics to make past decisions.
- **Garbage In/Garbage Out**: If the 50 metrics are highly correlated, the AI Analyst will find "ghost patterns".
- **Mitigation**: Pure strategy functions that only accept the *current* vector.

---
PRD Drafted. Initiate the Autonomous Pipeline: /proceed docs/specs/backtesting_and_paper_trading.md -> /test -> /refactor -> /test
