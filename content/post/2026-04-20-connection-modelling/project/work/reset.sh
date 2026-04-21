#!/usr/bin/env bash
# reset.sh — restore /work to a clean pre-bootstrap state
#
# Run from inside the container:
#   bash /work/reset.sh
#
# Removes all generated files (models, scripts, diagnostics, plots, notes,
# loop logs) and recreates empty directories ready for a fresh run.
# Preserves /work/CONTEXT.md and this script.

set -euo pipefail

echo "Resetting /work to clean state..."

# Generated directories — remove contents and the directory itself
rm -rf \
  /work/models \
  /work/scripts \
  /work/diagnostics \
  /work/plots \
  /work/loop_logs

# Generated files at /work root
rm -f \
  /work/NOTES.md \
  /work/ELI5.md

echo "Done. Preserved:"
echo "  /work/CONTEXT.md"
echo "  /work/reset.sh"
echo ""
echo "Run ./run_loop.sh from the host — it will recreate the directories."
