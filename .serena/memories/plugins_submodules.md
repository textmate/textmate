# PlugIns and Git Submodules

## PlugIns
The PlugIns directory contains two subdirectories (formerly git submodules, now incorporated):
- `PlugIns/dialog/` → Dialog2.tmplugin + tm_dialog2 CLI tool
- `PlugIns/dialog-1.x/` → Dialog.tmplugin + tm_dialog CLI tool

Both have their own `CMakeLists.txt` files. Plugin bundles use `-bundle` linker flag and `.tmplugin` extension.

## Active Submodules
- `Applications/TextMate/icons` — app icon assets
- `bin/CxxTest` — test framework
- `vendor/Onigmo/vendor` — Onigmo regex engine
- `vendor/kvdb/vendor` — kvdb key-value store

Always run `git submodule update --init` when working with this repo.
