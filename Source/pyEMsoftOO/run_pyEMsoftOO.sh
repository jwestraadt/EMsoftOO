#!/usr/bin/env bash
# run_pyEMsoftOO.sh — build libEMsoftOO_c and set up the Python environment
#
# Usage:
#   ./Source/pyEMsoftOO/run_pyEMsoftOO.sh [--sdk-root /path/to/SDK] \
#                                          [--build-dir build-release] \
#                                          [--skip-build]
#
# Options:
#   --sdk-root    Path to the EMsoftOO SDK (required unless --skip-build)
#   --build-dir   Build output directory name (default: EMsoftOOBuild/Release)
#   --skip-build  Skip cmake configure+build (DLL/so already exists)

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PY_DIR="$REPO_ROOT/Source/pyEMsoftOO"
SDK_ROOT=""
BUILD_DIR="EMsoftOOBuild/Release"
SKIP_BUILD=0

# ── parse args ────────────────────────────────────────────────────────────────
while [[ $# -gt 0 ]]; do
    case $1 in
        --sdk-root)  SDK_ROOT="$2"; shift 2 ;;
        --build-dir) BUILD_DIR="$2"; shift 2 ;;
        --skip-build) SKIP_BUILD=1; shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
done

step() { echo; echo "==> $*"; }
fail() { echo "ERROR: $*" >&2; exit 1; }

# ── detect platform ───────────────────────────────────────────────────────────
OS="$(uname -s)"
case "$OS" in
    Darwin) LIBNAME="libEMsoftOO_c.dylib" ;;
    Linux)  LIBNAME="libEMsoftOO_c.so"    ;;
    *)      fail "Unsupported OS: $OS. Use run_pyEMsoftOO.ps1 on Windows." ;;
esac

# ── read EMsoftConfig.json for defaults ──────────────────────────────────────
CONFIG="$HOME/.config/EMsoft/EMsoftConfig.json"
if [[ -f "$CONFIG" ]]; then
    echo "Loaded $CONFIG"
    if [[ -z "$SDK_ROOT" ]] && command -v python3 &>/dev/null; then
        SDK_ROOT="$(python3 -c "import json; d=json.load(open('$CONFIG')); print(d.get('EMsoftOO_SDK',''))" 2>/dev/null || true)"
    fi
fi

# ── Step 1: cmake configure + build ──────────────────────────────────────────
if [[ $SKIP_BUILD -eq 0 ]]; then
    step "Configuring and building $LIBNAME"

    [[ -z "$SDK_ROOT" ]] && fail "Provide --sdk-root <path> or set EMsoftOO_SDK in EMsoftConfig.json"

    mkdir -p "$REPO_ROOT/$BUILD_DIR"
    cmake -S "$REPO_ROOT" -B "$REPO_ROOT/$BUILD_DIR" \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_SHARED_LIBS=ON \
        -DEMsoftOO_SDK="$SDK_ROOT" \
        -DEMsoftOO_ENABLE_TESTING=OFF
    cmake --build "$REPO_ROOT/$BUILD_DIR" --parallel "$(nproc 2>/dev/null || sysctl -n hw.ncpu)"
fi

# ── Step 2: locate the library ────────────────────────────────────────────────
step "Locating $LIBNAME"

LIB_PATH=""
# Try standard build output locations
for candidate in \
    "$REPO_ROOT/$BUILD_DIR/lib/$LIBNAME" \
    "$REPO_ROOT/$BUILD_DIR/Bin/$LIBNAME"; do
    if [[ -f "$candidate" ]]; then
        LIB_PATH="$candidate"
        break
    fi
done

# Fall back to config
if [[ -z "$LIB_PATH" && -f "$CONFIG" ]] && command -v python3 &>/dev/null; then
    LIB_DIR="$(python3 -c "import json; d=json.load(open('$CONFIG')); print(d.get('EMsoftLibraryLocation',''))" 2>/dev/null || true)"
    [[ -f "$LIB_DIR/$LIBNAME" ]] && LIB_PATH="$LIB_DIR/$LIBNAME"
fi

[[ -z "$LIB_PATH" ]] && fail "Cannot find $LIBNAME. Set EMSOFTOO_LIB or update EMsoftConfig.json."
echo "Found library: $LIB_PATH"

# ── Step 3: uv venv + install ─────────────────────────────────────────────────
step "Setting up Python environment with uv"

command -v uv &>/dev/null || fail "uv not found. Install: curl -Ls https://astral.sh/uv/install.sh | sh"

cd "$PY_DIR"
uv venv --quiet
uv pip install h5py jupyterlab --quiet
uv pip install -e . --quiet

# ── Step 4: verify import ─────────────────────────────────────────────────────
step "Verifying emsoft import"

export EMSOFTOO_LIB="$LIB_PATH"
result="$(uv run python -c "import emsoft; print('emsoft', emsoft.__version__, 'loaded OK')")"
echo "$result"

echo
echo "Done.  To start working:"
echo
echo "  cd $PY_DIR"
echo "  export EMSOFTOO_LIB='$LIB_PATH'"
echo "  uv run jupyter lab"
echo
