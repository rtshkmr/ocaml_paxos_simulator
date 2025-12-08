#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'


SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT_DIR="$SCRIPT_DIR/.."
PAXOS_DIR="$ROOT_DIR/paxos"


info() { printf "\033[1;34m[cleanup]\033[0m %s\n" "$1"; }
warn() { printf "\033[1;33m[cleanup]\033[0m %s\n" "$1"; }


warn "Starting NUCLEAR cleanup (native mode). This will remove:"
printf " - %s/_opam\n - %s/_build\n - %s/_opam.locked (if any)\n" "$PAXOS_DIR" "$PAXOS_DIR" "$PAXOS_DIR"


read -r -p "Confirm final removal? (y/N): " confirm
confirm=${confirm,,}
if [[ "$confirm" != "y" && "$confirm" != "yes" ]]; then
info "Cleanup aborted by user."
exit 0
fi


info "Removing local opam switch directory: $PAXOS_DIR/_opam"
rm -rf "$PAXOS_DIR/_opam"
info "Removing build artifacts: $PAXOS_DIR/_build"
rm -rf "$PAXOS_DIR/_build"
info "Removing opam lock/metadata files if present"
rm -f "$PAXOS_DIR/.opam-switch" || true


info "Native NUCLEAR cleanup complete."
