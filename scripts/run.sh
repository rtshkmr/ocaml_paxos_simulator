#!/usr/bin/env bash
# run.sh - single entrypoint for native execution
# Style: UX-friendly (Style C)

set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$SCRIPT_DIR/.."
PAXOS_DIR="$ROOT_DIR/paxos"

# Defaults
INCLUDE_SETUP=true
CLEANUP_AFTER=false
PASSTHRU_ARGS=()

print_help() {
  cat <<'HELP'
Usage: ./scripts/run.sh [OPTIONS] -- [SIMULATOR ARGS]

Top-level options:
--no-setup         Skip environment setup step
--cleanup-after    Cleanup (NUCLEAR) after run (prompts confirmation)
--help, -h          Show this help

Simulator flags (passed through):
-allow-step BOOL
-max-log-level LOG_LEVEL
-scenario SCENARIO
-scenario-file PATH
-help, -?           Print simulator help

Examples:
./scripts/run.sh -- -scenario basic
./scripts/run.sh -- -scenario parliament -max-log-level debug
./scripts/run.sh --cleanup-after -- -scenario basic
HELP
}

# parse args until `--` then remaining args are simulator args
while [[ $# -gt 0 ]]; do
  case "$1" in
    --no-setup)
      INCLUDE_SETUP=false; shift;;
    --cleanup-after)
      CLEANUP_AFTER=true; shift;;
    --help|-h)
      print_help; exit 0;;
    --)
      shift; PASSTHRU_ARGS+=("$@"); break;;
    *)
      PASSTHRU_ARGS+=("$1"); shift;;
  esac
done

# helper for colored UX
info() { printf "\033[1;34m[info]\033[0m %s\n" "$1"; }
warn() { printf "\033[1;33m[warn]\033[0m %s\n" "$1"; }
err() { printf "\033[1;31m[error]\033[0m %s\n" "$1"; }

info "Running in native mode."

# Setup the environment if required
if $INCLUDE_SETUP; then
  info "Running setup script: $SCRIPT_DIR/native/setup_ocaml_env.sh"
  bash "$SCRIPT_DIR/native/setup_ocaml_env.sh"
else
  info "Skipping setup step (user requested)"
fi

# Handle cleanup-after flag
if $CLEANUP_AFTER; then
  warn "NUCLEAR cleanup requested after run. This will remove build artifacts."
  read -r -p "Proceed with cleanup after the run? (y/N): " reply
  reply=${reply,,}  # lowercase
  if [[ "$reply" != "y" && "$reply" != "yes" ]]; then
    info "Aborting because user did not confirm cleanup. Exiting."
    exit 0
  fi
fi

# Run the simulation
info "Running simulator with args:\n${PASSTHRU_ARGS[@]}"
bash "$SCRIPT_DIR/native/run_sim.sh" "${PASSTHRU_ARGS[@]}"

# Handle cleanup-after
if $CLEANUP_AFTER; then
  info "Running cleanup after simulation"
  bash "$SCRIPT_DIR/native/cleanup_after.sh"
fi

info "Done."
