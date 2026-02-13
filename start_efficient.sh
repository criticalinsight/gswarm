#!/bin/bash
# Start Gswarm with efficiency optimizations for M2/Apple Silicon
# +S 4:2  -> Limit to 4 schedulers with 2 dirty schedulers (Targeting E-cores)
# +sbwt none -> No busy wait (Prevents CPU spinning when idle, MAJOR heat reduction)
# +sbwtdcpu none -> No dirty CPU busy wait
# +sbwtdio none -> No dirty IO busy wait

echo "🐝 Starting Gswarm in Efficiency Mode (Cool & Quiet)..."
export ERL_FLAGS="+S 4:2 +sbwt none +sbwtdcpu none +sbwtdio none"
gleam run "$@"
