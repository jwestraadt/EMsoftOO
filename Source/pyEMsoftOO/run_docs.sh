#!/usr/bin/env bash
# run_docs.sh — build the EMsoft Python Interface Sphinx documentation
#
# Usage:
#   ./Source/pyEMsoftOO/run_docs.sh [--clean] [--no-browser]
#
# Options:
#   --clean       Remove docs/_build before building (full rebuild)
#   --no-browser  Do not open the HTML output after building

set -euo pipefail

PY_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOCS_DIR="$PY_DIR/docs"
BUILD_DIR="$DOCS_DIR/_build/html"
CLEAN=0
NO_BROWSER=0

while [[ $# -gt 0 ]]; do
    case $1 in
        --clean)      CLEAN=1;      shift ;;
        --no-browser) NO_BROWSER=1; shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
done

step() { echo; echo "==> $*"; }
fail() { echo "ERROR: $*" >&2; exit 1; }

command -v uv &>/dev/null || fail "uv not found. Install: curl -Ls https://astral.sh/uv/install.sh | sh"

cd "$PY_DIR"

# ── Step 1: install doc dependencies ─────────────────────────────────────────
step "Installing documentation dependencies"
uv pip install -e ".[docs]" --quiet

# ── Step 2: optional clean ────────────────────────────────────────────────────
if [[ $CLEAN -eq 1 && -d "$BUILD_DIR" ]]; then
    step "Removing existing build"
    rm -rf "$BUILD_DIR"
fi

# ── Step 3: sphinx-build ──────────────────────────────────────────────────────
step "Running sphinx-build"
uv run sphinx-build -b html "$DOCS_DIR" "$BUILD_DIR"

echo
echo "Build succeeded. Output: $BUILD_DIR"

# ── Step 4: open browser ──────────────────────────────────────────────────────
if [[ $NO_BROWSER -eq 0 ]]; then
    INDEX="$BUILD_DIR/index.html"
    if [[ -f "$INDEX" ]]; then
        step "Opening browser"
        case "$(uname -s)" in
            Darwin) open "$INDEX" ;;
            Linux)  xdg-open "$INDEX" 2>/dev/null || true ;;
        esac
    fi
fi
