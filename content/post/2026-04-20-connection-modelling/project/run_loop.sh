#!/usr/bin/env bash
# run_loop.sh — host-side orchestration for the Claude iterative modelling loop
#
# Usage:
#   ./run_loop.sh [max_iterations]
#
# Requirements:
#   - ANTHROPIC_API_KEY must be set in the environment
#   - Docker daemon running, tcp-modelling:latest image built
#   - Run from the project/ directory
#
# What it does:
#   1. Bootstrap (once): Claude explores data, writes m1.stan, fits, documents
#   2. Iterate (up to max_iterations): Claude diagnoses, improves one thing,
#      re-fits, updates NOTES.md, checks convergence
#   Each iteration is git-committed and its full JSON log saved to work/loop_logs/

set -euo pipefail

MAX_ITER=${1:-10}
ITER=0
LOG_DIR="work/loop_logs"

# ── Preflight checks ────────────────────────────────────────────────────────

if [ -z "${ANTHROPIC_API_KEY:-}" ]; then
  echo "ERROR: ANTHROPIC_API_KEY is not set."
  exit 1
fi

if ! docker info >/dev/null 2>&1; then
  echo "ERROR: Docker daemon is not running."
  exit 1
fi

mkdir -p "$LOG_DIR" work/models work/scripts work/diagnostics work/plots
# Ensure the analyst user (uid 1001) inside the container can write to all
# work/ subdirectories. Only chmod directories (we own those); files created
# by a previous container run are owned by uid 1001 and can't be chmod'd here.
find work/ -type d | xargs chmod o+rwx 2>/dev/null || true

# ── Helper: run one Claude invocation inside the container ──────────────────
# Passes the prompt via environment variable to avoid shell quoting issues.
# All Claude output (JSON) goes to the log file AND to stdout via tee.

run_claude() {
  local label="$1"
  local prompt_file="$2"
  local log_file="$3"

  echo ""
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  echo "  $label"
  echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

  # Prompt and log path are passed as env vars; bash inside the container
  # expands them. tee runs inside the container so the analyst user (uid 1001)
  # always has write access to /work/loop_logs/, regardless of host ownership.
  docker compose run --rm \
    -e ANTHROPIC_API_KEY="$ANTHROPIC_API_KEY" \
    -e CLAUDE_PROMPT="$(cat "$prompt_file")" \
    -e CLAUDE_LOG="/work/loop_logs/$(basename "$log_file")" \
    r-stan \
    -c 'claude --dangerously-skip-permissions --model claude-opus-4-7 -p "${CLAUDE_PROMPT}" --output-format json < /dev/null | tee "${CLAUDE_LOG}"'
}

# ── Phase 1: Bootstrap ──────────────────────────────────────────────────────

if [ ! -f "work/models/m1.stan" ]; then
  echo "No model found — running bootstrap."
  run_claude "Bootstrap: data exploration + initial model" \
    "prompts/bootstrap.md" \
    "$LOG_DIR/bootstrap.json"

  git add -A && git commit -m "loop: bootstrap"
else
  echo "work/models/m1.stan exists — skipping bootstrap."
fi

# ── Phase 2: Iterative improvement ─────────────────────────────────────────

while [ "$ITER" -lt "$MAX_ITER" ]; do
  ITER=$(( ITER + 1 ))

  STATUS=$(cat work/diagnostics/status.txt 2>/dev/null || echo "CONTINUE")
  if [ "$STATUS" = "CONVERGED" ]; then
    echo ""
    echo "Status is CONVERGED — stopping before iteration $ITER."
    break
  fi

  run_claude "Iteration $ITER / $MAX_ITER" \
    "prompts/iterate.md" \
    "$LOG_DIR/iter_${ITER}.json"

  git add -A && git commit -m "loop: iter $ITER"

  STATUS=$(cat work/diagnostics/status.txt 2>/dev/null || echo "CONTINUE")
  echo ""
  echo "Status after iteration $ITER: $STATUS"

  if [ "$STATUS" = "CONVERGED" ]; then
    echo "Model converged. Review work/NOTES.md for the full report."
    break
  fi
done

# ── Summary ─────────────────────────────────────────────────────────────────

STATUS=$(cat work/diagnostics/status.txt 2>/dev/null || echo "CONTINUE")
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
if [ "$STATUS" = "CONVERGED" ]; then
  echo "  Loop complete: CONVERGED after $ITER iteration(s)."
else
  echo "  Loop complete: reached max iterations ($MAX_ITER), not yet converged."
  echo "  Re-run ./run_loop.sh to continue, or inspect work/NOTES.md."
fi
echo "  Notes:       work/NOTES.md"
echo "  Plots:       work/plots/"
echo "  Diagnostics: work/diagnostics/summary.txt"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
