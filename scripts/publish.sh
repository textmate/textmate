#!/bin/bash
set -euo pipefail

# ── Configuration ────────────────────────────────────────────────────
APP_NAME="TextMate"

GIT_TAG=$(git describe --tags --exact-match 2>/dev/null || true)
if [ -z "$GIT_TAG" ]; then
  echo "ERROR: HEAD is not tagged. Run make package [patch|minor] to prepare a release tag."
  exit 1
fi
VERSION="${GIT_TAG#v}"

BUILD_DIR="build-release"
TGZ_PATH="$BUILD_DIR/$APP_NAME-$VERSION.tar.gz"

if [ ! -f "$TGZ_PATH" ]; then
  echo "ERROR: $TGZ_PATH not found. Run make package to build, sign and notarize first."
  exit 1
fi

if ! command -v gh &>/dev/null; then
  echo "ERROR: gh CLI not found"
  echo "       $TGZ_PATH was created but the release was not published."
  exit 1
fi

TGZ_SHA=$(shasum -a 256 "$TGZ_PATH" | awk '{print $1}')

UPSTREAM=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}')
REMOTE="${UPSTREAM%%/*}"

# ── Push branch and tag ──────────────────────────────────────────────
echo "==> Pushing branch and tag"
git push "$REMOTE" HEAD
git push "$REMOTE" "$GIT_TAG"

# ── GitHub Release ───────────────────────────────────────────────────
echo "==> Publishing GitHub release"
if gh release view "$GIT_TAG" &>/dev/null; then
  gh release upload "$GIT_TAG" "$TGZ_PATH" --clobber
else
  gh release create "$GIT_TAG" "$TGZ_PATH" \
    --title "$APP_NAME $VERSION" \
    --generate-notes
fi

# ── Update Homebrew tap ──────────────────────────────────────────────
HOMEBREW_TAP="${HOMEBREW_TAP:-$HOME/code/homebrew-textmate}"
CASK_FILE="$HOMEBREW_TAP/Casks/textmate.rb"

if [ -f "$CASK_FILE" ]; then
  echo "==> Updating Homebrew cask"
  sed -i '' "s/version \".*\"/version \"$VERSION\"/" "$CASK_FILE"
  sed -i '' "s/sha256 \".*\"/sha256 \"$TGZ_SHA\"/" "$CASK_FILE"
  git -C "$HOMEBREW_TAP" add Casks/textmate.rb
  git -C "$HOMEBREW_TAP" commit -m "Update TextMate to $VERSION"
  git -C "$HOMEBREW_TAP" push
else
  echo "WARNING: cask not found at $CASK_FILE — skipping tap update"
fi

# ── Done ─────────────────────────────────────────────────────────────
echo ""
echo "Published: $TGZ_PATH"
echo "  Version: $VERSION"
echo "  SHA256:  $TGZ_SHA"
