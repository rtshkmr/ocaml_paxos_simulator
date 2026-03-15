.PHONY: help setup magic cleanup quickstart dev exec check_install test fmt

# vars overridable via cli injection
scenario ?= camel_caravan_complex
override_log_level ?= info
allow_step ?= true
cleanup_after ?= false
autopromote ?= false

# Setup simulator flags conditionally
SIM_FLAGS := -scenario $(scenario) -log-level $(override_log_level) -allow-step $(allow_step)

# Add cleanup flag if requested (for run.sh, not simulator)
ifeq ($(cleanup_after),true)
CLEANUP_FLAG := --cleanup-after
else
CLEANUP_FLAG :=
endif

# Add autopromote flag for dune tests if requested
ifeq ($(autopromote),true)
AUTOPROMOTE_FLAG := --auto-promote
else
AUTOPROMOTE_FLAG :=
endif

# --- Help target ---
help:
	@echo "Available targets:"
	@echo " make setup        - Setup native environment (first time only)"
	@echo " make test         - Run tests using dune"
	@echo " make fmt          - Format code using dune fmt"
	@echo " make magic        - Run the simulator (after setup)"
	@echo " make quickstart   - Setup + run in one command (requires PATH update first)"
	@echo " make cleanup      - Run the cleanup script"
	@echo " make dev          - Start watch mode for development"
	@echo " make check_install - Check if environment is correctly set up"
	@echo ""
	@echo "First-time setup workflow:"
	@echo " 1. make setup"
	@echo " 2. Add '~/.local/bin' to your PATH (instructions will be shown)"
	@echo " 3. Restart your shell or run: source ~/.zshrc"
	@echo " 4. make magic"
	@echo ""
	@echo "Examples:"
	@echo " make magic scenario=parliament"
	@echo " make magic scenario=camel_caravan_complex"
	@echo " make magic scenario=camel_caravan allow_step=false"
	@echo " make test autopromote=true"

# --- Base simulator runner (assumes setup already done) ---
magic: check_install
	@echo "🎬 Running simulator with flags: $(SIM_FLAGS)"
	@echo ""
	cd paxos && eval "$$(opam env --switch . --set-switch)" && \
	sh ../scripts/run.sh --no-setup -- $(SIM_FLAGS)

# --- Cleanup (nuclear) ---
cleanup:
	@echo "☢️ Cleanup is nuclear. It will purge all things that are supposed to be auto-setup for you."
	@echo "Please run it directly via ./scripts/native/cleanup_after.sh"

# --- Quickstart (for users who already have PATH configured) ---
quickstart: setup magic
	@echo "✅ Quickstart complete!"

# --- Development mode with watch ---
dev:
	@echo "🤖 Starting dune build in watch mode... Changes will trigger rebuild."
	@echo "👀 Watching the project for changes in the paxos directory."
	@echo "💨 Using parallel builds for faster recompilation."
	@CORES=$$(if [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.ncpu 2>/dev/null || echo 4; else nproc 2>/dev/null || echo 4; fi); \
	echo "🔧 Building with $$CORES cores..."; \
	cd paxos && dune build -w -j $$CORES

# --- Format code ---
fmt:
	@echo "🎨 Formatting code with dune fmt..."
	cd paxos && dune fmt

# --- Run tests ---
test:
	@echo "🧪 Running tests..."
	@CORES=$$(if [ "$$(uname)" = "Darwin" ]; then sysctl -n hw.ncpu 2>/dev/null || echo 4; else nproc 2>/dev/null || echo 4; fi); \
	cd paxos && dune runtest $(AUTOPROMOTE_FLAG) -j $$CORES

# --- Direct execution (for advanced users) ---
exec:
	@echo "🐫 Running simulation with provided arguments..."
	cd paxos && dune exec bin/simulation.exe -- run $(filter-out $@,$(MAKECMDGOALS))

# --- Environment check ---
check_install:
	@echo "🔎 Checking if all required tools are installed..."
	cd paxos && eval "$$(opam env --switch . --set-switch)";
	@which opam > /dev/null || (echo "⛔️ Error: opam is not installed!" && exit 1)
	@which dune > /dev/null || (echo "⛔️ Error: dune is not installed!" && exit 1)
	@which ocaml > /dev/null || (echo "⛔️ Error: ocaml is not installed!" && exit 1)
	@echo "✅ All required tools are installed. You are ready to run the simulator."
	@echo ""

# --- Setup ---
setup:
	@echo "🔧 Setting up OCaml environment..."
	sh ./scripts/native/setup_ocaml_env.sh
