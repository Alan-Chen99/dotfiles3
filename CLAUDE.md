# CLAUDE.md

## Repository overview

Nix flake-based dotfiles repo. Manages a full development environment
including a custom Emacs build, Python/Rust/JS toolchains, and
home-manager config. Target system is x86_64-linux (Pop!\_OS/NixOS).

## Nix structure

- `flake.nix` — entry point, ~30 inputs including multiple nixpkgs
  versions, emacs-overlay, rust-overlay, language build tools
- `nix/flake-attrs.nix` — flake outputs, chains overlays and build
  variants
- `nix/default.nix` — module orchestration, custom `callpackage` +
  `reexport` pattern, `checkedjoin` prevents attr collisions
- `nix/deps.nix` — base nixpkgs config and 65+ core packages
- `nix/profile.nix` — user profile with core tools and symlinks
- `nix/home.nix` — home-manager user config

Module pattern: `mod.name = callpackage ./file.nix {} (reexport
(prev: { inherit (prev) exported-attr; }))`. Modules get all deps +
other modules via `self.*`.

## Emacs build

- `emacs/default.nix` — builds `emacs-git-pgtk` (v31.0.50) from
  pinned master commit via emacs-overlay
- `dontStrip = true` is set; uses `mcc-env` (mini-compile-commands)
  stdenv to preserve C sources alongside the build
- The installed binary is a Nix C wrapper
  (`bin/emacs-31.0.50`); the real binary is at
  `bin/.emacs-31.0.50-wrapped` (25MB, has full DWARF debug info)
- Use `addr2line -f -e .emacs-31.0.50-wrapped <addr>` to resolve
  crash backtraces (the wrapper's symbol table is nearly empty)
- Emacs source pin is in `flake.nix` under `emacs31` input
- Alternative builds available: `emacs29`, `emacs30`, `emacs-gtk`
  (X11/GTK3 instead of PGTK)

## Emacs config

- `emacs/` dir, 100+ elisp modules named `alan-*.el`
- Uses Evil mode (vim bindings), elpaca package manager
- Entry: `.emacs` → `early-init.el` → `alan.el`
- Lock file: `elpaca-lock.eld`

## Other language dirs

- `python/` — pyproject.toml + uv, integrated via uv2nix/poetry2nix
- `rust/` — Cargo project, built with crane in nix
- `js/` — yarn-based, nix integration in default.nix
- `scripts/` — misc Python/bash utilities

## Debugging Emacs crashes

The Emacs crash handler uses `backtrace_symbols()` which only reads
`.dynsym`. Since most Emacs C functions aren't dynamically exported,
crash backtraces show bare `emacs() [0xADDR]`. To get function names:

```sh
REAL=$(ls /nix/store/<hash>-emacs-git-pgtk-*/bin/.emacs-*-wrapped)
addr2line -f -e "$REAL" 0xADDR
```

The real binary has `.debug_info` etc. because `dontStrip = true`
preserves compiler-emitted debug sections.

## Build/CI

- `nix/ci.nix` defines CI build set
- `.github/` has CI workflows
- `emacs/ci.el` byte-compiles elisp for CI checks

## Interactive Emacs

Agents running debugging should ALWAYS use `emacs/agent_work_template.el` for interactive emacs.
Run Emacs on a virtual display so it does not appear on the user's screen:

```sh
nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs -l /tmp/agent-work.el 2>/dev/null
```

Agents MUST verify that they can run emacs BEFORE starting/thinking using the command above.
DO NOT use `--batch` — it skips normal config loading and `(require 'alan)` will fail.
DO NOT ask users to run your scripts for interactive emacs.
Never use code to find something that can be found by running emacs.

After Emacs exits, read the log filtered to work output (skip startup trace):

```sh
grep -A9999 -- '----start----' /tmp/debug.log
```
