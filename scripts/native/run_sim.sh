#!/usr/bin/env bash
# run_sim.sh - Execute the simulator with optimized builds
set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$SCRIPT_DIR/../.."
PAXOS_DIR="$ROOT_DIR/paxos"

info() { printf "\033[1;34m[run]\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m[run]\033[0m %s\n" "$*"; }
err() { printf "\033[1;31m[run]\033[0m %s\n" "$*" >&2; }

# Detect cores for parallel builds
if [[ "$OSTYPE" == "darwin"* ]]; then
    CORES=$(sysctl -n hw.ncpu 2>/dev/null || echo "4")
else
    CORES=$(nproc 2>/dev/null || echo "4")
fi

# Validation
if [[ ! -d "$PAXOS_DIR" ]]; then
    err "Paxos directory not found: $PAXOS_DIR"
    exit 1
fi

info "Activating local opam environment"
cd "$PAXOS_DIR" || {
    err "Failed to change to paxos directory"
    exit 1
}

# Check for opam switch
if [[ ! -d "$PAXOS_DIR/_opam" ]]; then
    err "Local opam switch not found at $PAXOS_DIR/_opam"
    err "Please run with setup first (remove --no-setup flag)"
    exit 1
fi

eval "$(opam env --switch . --set-switch)" || {
    err "Failed to activate opam environment"
    exit 1
}

info "Building project with $CORES parallel jobs (incremental builds are fast)..."
if ! dune build -j "$CORES" 2>&1; then
    err "Dune build failed"
    err "Try cleaning and rebuilding: dune clean && dune build -j $CORES"
    exit 1
fi

EXE_NAME="simulation.exe"
BIN_PATH="_build/default/bin/$EXE_NAME"

if [[ ! -x "$BIN_PATH" ]]; then
    warn "Binary not found at $BIN_PATH after build"
    info "Attempting full install build..."
    if ! dune build @install -j "$CORES"; then
        err "Full build failed"
        exit 1
    fi
fi

if [[ ! -x "$BIN_PATH" ]]; then
    err "Binary still not found at $BIN_PATH after build"
    err "Check that bin/dune has correct executable configuration"
    exit 1
fi

info "Running simulator: $BIN_PATH run $*"
"$BIN_PATH" "run" "$@" || {
    EXIT_CODE=$?
    err "Simulator execution failed with exit code $EXIT_CODE"
    exit "$EXIT_CODE"
}
