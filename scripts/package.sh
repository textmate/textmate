#!/bin/bash
set -euo pipefail

# ── Configuration ────────────────────────────────────────────────────
APP_NAME="TextMate"
IDENTITY="Developer ID Application"
NOTARY_TEAM_ID="${NOTARY_TEAM_ID:?Set NOTARY_TEAM_ID env var to your Apple team ID}"
NOTARY_APPLE_ID="${NOTARY_APPLE_ID:?Set NOTARY_APPLE_ID env var to your Apple ID email}"
NOTARY_PASSWORD="${NOTARY_PASSWORD:?Set NOTARY_PASSWORD env var (app-specific password from appleid.apple.com)}"

GIT_TAG=$(git describe --tags --exact-match 2>/dev/null || true)
if [ -z "$GIT_TAG" ]; then
  echo "ERROR: HEAD is not tagged. Run make package [patch|minor] to prepare a release tag."
  exit 1
fi
VERSION="${GIT_TAG#v}"

BUILD_DIR="build-release"
APP_PATH="$BUILD_DIR/Applications/TextMate/TextMate.app"

# ── Build ────────────────────────────────────────────────────────────
echo "==> Building release (v$VERSION)"
make release CS_IDENTITY="$IDENTITY"

if [ ! -d "$APP_PATH" ]; then
  echo "ERROR: $APP_PATH not found after build"
  exit 1
fi

# ── Verify signature ────────────────────────────────────────────────
echo "==> Verifying code signature"
codesign --verify --deep --strict "$APP_PATH"
echo "    Signature OK"

# ── Notarize ─────────────────────────────────────────────────────────
echo "==> Creating zip for notarization"
NOTARIZE_ZIP="$BUILD_DIR/$APP_NAME-$VERSION-notarize.zip"
ditto -c -k --keepParent "$APP_PATH" "$NOTARIZE_ZIP"

echo "==> Submitting for notarization (this may take a few minutes)"
NOTARY_OUTPUT=$(xcrun notarytool submit "$NOTARIZE_ZIP" \
  --apple-id "$NOTARY_APPLE_ID" \
  --team-id "$NOTARY_TEAM_ID" \
  --password "$NOTARY_PASSWORD" \
  --wait 2>&1) || true
echo "$NOTARY_OUTPUT"

NOTARY_ID=$(echo "$NOTARY_OUTPUT" | grep "  id:" | head -1 | awk '{print $2}')

if ! echo "$NOTARY_OUTPUT" | grep -q "status: Accepted"; then
  echo ""
  echo "Notarization failed. Fetching log..."
  if [ -n "$NOTARY_ID" ]; then
    xcrun notarytool log "$NOTARY_ID" \
      --apple-id "$NOTARY_APPLE_ID" \
      --team-id "$NOTARY_TEAM_ID" \
      --password "$NOTARY_PASSWORD" 2>&1 || true
  fi
  exit 1
fi

rm -f "$NOTARIZE_ZIP"

# ── Staple ───────────────────────────────────────────────────────────
echo "==> Stapling notarization ticket"
xcrun stapler staple "$APP_PATH"

# ── Package as .tar.gz ───────────────────────────────────────────────
TGZ_PATH="$BUILD_DIR/$APP_NAME-$VERSION.tar.gz"
echo "==> Creating $TGZ_PATH"
tar -czf "$TGZ_PATH" -C "$(dirname "$APP_PATH")" "$APP_NAME.app"

TGZ_SHA=$(shasum -a 256 "$TGZ_PATH" | awk '{print $1}')

# ── Done ─────────────────────────────────────────────────────────────
echo ""
echo "Done! Signed & notarized: $TGZ_PATH"
echo "  Version: $VERSION"
echo "  SHA256:  $TGZ_SHA"
echo ""
echo "Next step: run make publish to push the tag and publish the release."
