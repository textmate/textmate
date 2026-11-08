#!/bin/bash
# Builds OakSwiftUI dynamic library and copies to TextMate's build directory.
# Usage: ./build.sh [debug|release] [build-dir]
# Incremental: swift build handles caching, only rebuilds changed sources.

set -euo pipefail

CONFIG="${1:-debug}"
CALLER_DIR="$(pwd)"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Resolve BUILD_DIR relative to caller's CWD, not script location
if [ -n "${2:-}" ]; then
    case "$2" in
        /*) BUILD_DIR="$2" ;;
        *)  BUILD_DIR="${CALLER_DIR}/$2" ;;
    esac
else
    BUILD_DIR="${CALLER_DIR}/build-${CONFIG}"
fi
LIB_DIR="${BUILD_DIR}/lib"

cd "$SCRIPT_DIR"

case "$CONFIG" in
    debug)   SWIFT_CONFIG="debug" ;;
    release) SWIFT_CONFIG="release" ;;
    *)       echo "Usage: $0 [debug|release] [build-dir]"; exit 1 ;;
esac

echo "Building OakSwiftUI ($SWIFT_CONFIG)..."
# CMake consumes the native build system's generated Objective-C header layout.
swift build -c "$SWIFT_CONFIG" --build-system native --disable-sandbox --product OakSwiftUI 2>&1

DYLIB_PATH=".build/${SWIFT_CONFIG}/libOakSwiftUI.dylib"

if [ ! -f "$DYLIB_PATH" ]; then
    echo "Error: $DYLIB_PATH not found after build"
    exit 1
fi

mkdir -p "$LIB_DIR"
cp "$DYLIB_PATH" "$LIB_DIR/"

# Set install name for @rpath resolution at runtime
install_name_tool -id "@rpath/libOakSwiftUI.dylib" "$LIB_DIR/libOakSwiftUI.dylib"

echo "OakSwiftUI built: $LIB_DIR/libOakSwiftUI.dylib"
