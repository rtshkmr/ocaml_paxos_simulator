#!/usr/bin/env bash
# setup_ocaml_env.sh - Fast, reproducible OCaml environment setup
set -euo pipefail
IFS=$'\n\t'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$SCRIPT_DIR/../.."
PAXOS_DIR="$ROOT_DIR/paxos"

info() { printf "\033[1;34m[setup]\033[0m %s\n" "$*"; }
warn() { printf "\033[1;33m[setup]\033[0m %s\n" "$*"; }
err() { printf "\033[1;31m[setup]\033[0m %s\n" "$*" >&2; }

# Detect number of cores for parallel builds
if [[ "$OSTYPE" == "darwin"* ]]; then
    CORES=$(sysctl -n hw.ncpu 2>/dev/null || echo "4")
else
    CORES=$(nproc 2>/dev/null || echo "4")
fi
info "Detected $CORES CPU cores for parallel builds"

info "Setting up native OCaml environment"

if [[ ! -d "$PAXOS_DIR" ]]; then
    err "Paxos directory not found: $PAXOS_DIR"
    exit 1
fi

# Check for opam
if ! command -v opam >/dev/null 2>&1; then
    warn "opam not found in PATH. Installing opam via official installer (user-level)."

    # Create local bin directory
    mkdir -p "$HOME/.local/bin"

    # Install opam NON-INTERACTIVELY by providing answers via stdin
    # The installer asks: "Where should it be installed?" and we answer with the path
    info "Installing to $HOME/.local/bin (this may take a minute)..."

    # Download installer to temp file (sh-compatible approach)
    TEMP_INSTALLER=$(mktemp)
    if ! curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh -o "$TEMP_INSTALLER"; then
        err "Failed to download opam installer"
        rm -f "$TEMP_INSTALLER"
        exit 1
    fi

    # Run installer with path provided via stdin
    if ! echo "$HOME/.local/bin" | sh "$TEMP_INSTALLER"; then
        err "Failed to install opam"
        rm -f "$TEMP_INSTALLER"
        exit 1
    fi
    rm -f "$TEMP_INSTALLER"

    # Add to PATH for current session
    export PATH="$HOME/.local/bin:$PATH"

    # Verify installation
    if ! command -v opam >/dev/null 2>&1; then
        err "opam installation failed or not in PATH"
        err "Check that $HOME/.local/bin/opam exists and is executable"
        exit 1
    fi

    info "✓ opam installed successfully at $(which opam)"
else
    info "✓ opam already installed at $(which opam)"
fi

cd "$PAXOS_DIR" || {
    err "Failed to change to paxos directory"
    exit 1
}

# Initialize opam if needed (only check once)
if ! opam var root >/dev/null 2>&1; then
    info "Initializing opam (first-time setup)..."
    if ! opam init -y --disable-sandboxing; then
        err "opam initialization failed"
        exit 1
    fi
    info "✓ opam initialized"
else
    info "✓ opam already initialized"
fi

# Check for lockfile
LOCK_FLAG=""
if [[ -f "paxos.opam.locked" ]]; then
    info "Found lockfile - using exact dependency versions for reproducibility"
    LOCK_FLAG="--locked"
else
    warn "No lockfile found at paxos.opam.locked"
    warn "Builds may not be reproducible across different machines"
    warn "To create one: opam lock . && git add paxos.opam.locked"
fi

info "Installing dune build tool..."
if ! opam install dune -y --jobs="$CORES"; then
    err "Failed to install dune"
    exit 1
fi
info "✓ dune installed ($(dune --version))"

# Use local switch in repo
if [[ ! -d "$PAXOS_DIR/_opam" ]]; then
    info "Creating local opam switch in $PAXOS_DIR/_opam"
    info "This will take 3-5 minutes (installing OCaml compiler + dependencies)..."

    # Create switch and install dependencies with parallel builds
    if ! opam switch create . $LOCK_FLAG -y --jobs="$CORES"; then
        err "Failed to create local opam switch"
        err ""
        err "Common causes:"
        err "  - Missing system dependencies (try: brew install pkg-config on macOS)"
        err "  - Network issues downloading packages"
        err "  - Corrupted opam cache (try: rm -rf ~/.opam/repo/default)"
        exit 1
    fi

    info "✓ Switch created successfully"
else
    info "✓ Local switch already exists at $PAXOS_DIR/_opam"

    # Ensure dependencies are current (fast if already installed)
    info "Verifying dependencies are installed..."
    eval "$(opam env --switch . --set-switch)"

    # Install dependencies if not already present
    if ! opam install . $LOCK_FLAG --deps-only -y --jobs="$CORES"; then
        err "Failed to install dependencies"
        exit 1
    fi
fi

# Activate environment
eval "$(opam env --switch . --set-switch)" || {
    err "Failed to activate opam environment"
    exit 1
}

# Verify critical dependencies are available
info "Verifying core dependencies..."
MISSING_DEPS=""
for dep in base core yojson re ppx_deriving_yojson; do
    if ! ocamlfind query "$dep" >/dev/null 2>&1; then
        MISSING_DEPS="$MISSING_DEPS $dep"
    fi
done

if [[ -n "$MISSING_DEPS" ]]; then
    err "Missing dependencies:$MISSING_DEPS"
    err ""
    err "Installing missing dependencies..."
    if ! opam install $MISSING_DEPS -y; then
        err "Failed to install missing dependencies"
        err "Try manually: opam install base core yojson re ppx_deriving_yojson ppx_jane -y"
        exit 1
    fi
fi

info "✓ All dependencies verified"
info "✓ Native OCaml environment ready"
info ""

# Detect the user's shell config file
SHELL_NAME=$(basename "$SHELL")
case "$SHELL_NAME" in
bash)
    SHELL_CONFIG="~/.bashrc or ~/.bash_profile"
    SOURCE_CMD="source ~/.bashrc"
    ;;
zsh)
    SHELL_CONFIG="~/.zshrc"
    SOURCE_CMD="source ~/.zshrc"
    ;;
fish)
    SHELL_CONFIG="~/.config/fish/config.fish"
    SOURCE_CMD="source ~/.config/fish/config.fish"
    ;;
*)
    SHELL_CONFIG="your shell config file"
    SOURCE_CMD="source <your-config-file>"
    ;;
esac

# Check if PATH update is needed by looking at the actual config file
NEEDS_PATH_UPDATE=true
if [[ -f "$HOME/.zshrc" ]] && grep -q "\.local/bin" "$HOME/.zshrc" 2>/dev/null; then
    NEEDS_PATH_UPDATE=false
elif [[ -f "$HOME/.bashrc" ]] && grep -q "\.local/bin" "$HOME/.bashrc" 2>/dev/null; then
    NEEDS_PATH_UPDATE=false
elif [[ -f "$HOME/.bash_profile" ]] && grep -q "\.local/bin" "$HOME/.bash_profile" 2>/dev/null; then
    NEEDS_PATH_UPDATE=false
fi

# Final user instructions
if $NEEDS_PATH_UPDATE; then
    warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    warn "⚠️  IMPORTANT: One more step required!"
    warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    warn ""
    warn "To use opam, ocaml, and dune, you need to update your PATH."
    warn ""
    warn "Add this line to your $SHELL_CONFIG:"
    warn ""
    warn "    export PATH=\"\$HOME/.local/bin:\$PATH\""
    warn ""
    warn "This ensures the 'opam' command is available system-wide."
    warn ""
    warn "Then reload your shell:"
    warn ""
    warn "    $SOURCE_CMD"
    warn ""
else
    info "✓ PATH appears to already include \$HOME/.local/bin"
    info ""
    info "If this is a new terminal session, reload your shell:"
    info "    $SOURCE_CMD"
fi

echo ""
warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
warn "⚠️  ONE MORE REQUIRED STEP FOR OCAML & DUNE"
warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
warn ""
warn "To activate the local opam switch (which contains OCaml & dune), run:"
warn ""
warn "    eval \"\$(opam env --switch ./paxos --set-switch)\""
warn ""
warn "This updates your PATH so that:"
warn "  - ocaml"
warn "  - dune"
warn "  - all project-specific tools"
warn "are available in your current shell."
echo ""

warn "After performing BOTH steps above, try running:"
warn ""
warn "    make magic"
warn ""
warn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
