# CLAUDE.md

## Repository overview

Nix flake-based dotfiles repo. Manages a full development environment
including a custom Emacs build, Python/Rust/JS toolchains, and
home-manager config. Target system is x86_64-linux (Pop!\_OS/NixOS).

## Nix store

NEVER search or `find` the entire `/nix/store` — it contains millions
of files and will hang indefinitely. Instead, use specific known store
paths, `nix path-info`, `nix eval`, or query Emacs
(`(locate-library ...)`) to find what you need.

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

Agents running debugging MUST use `emacs/agent_work_template.el` for interactive emacs.
Run Emacs on a virtual display so it does not appear on the user's screen:

```sh
agent-tools run --desc "emacs agent work" nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el
```

Run it under `agent-tools run`. span reports a failure of the log handler
on stderr — the one failure the log itself cannot carry — and agent-tools
captures stderr and passes it through, so no redirect is needed.

MUST verify that they can run emacs BEFORE exploring code or starting any related work.
SHOULD NOT use `--batch` — it skips normal config loading and `(require 'alan)` will fail.
MUST NOT ask users to run your scripts for interactive emacs.
MUST NOT use code to find something that can be found by running emacs.

After Emacs exits, read the log filtered to work output (skip startup trace):

```sh
grep -a -A9999 -- '----start----' /tmp/debug.log
```

`-a` is required: the log embeds raw subprocess output, so plain
`grep` can classify it as binary and print nothing at all. `message`
output appears in this log tagged `%%`, not in `*Messages*`.

Any file the work section writes itself MUST bind
`coding-system-for-write` to `utf-8-emacs-unix`. Emacs strings hold raw
bytes and characters above `#x10FFFF` — consult appends the latter to
every completion candidate — and no ordinary coding system encodes
them, so `write-region` stops on a coding-system prompt. Emacs then
hangs with an empty stdout and stderr and a log that simply stops.

The span log itself is already safe: the template installs
`span-file-log-handler`, which encodes in Lisp and keeps the write off
Tramp and off lock files. Do not hand-roll a log handler.

`span-msg` queues the entry; the log is written on a 0.5s timer. Use
`span-msg-now` for a checkpoint that must survive a segfault or an
external kill — it returns only once the entry is on disk.

The template clears `debug-ignored-errors`. Emacs skips the debugger for
the errors listed there, and span logs errors by way of the debugger, so
an unhandled `end-of-file` — an unbalanced paren in your work file — would
otherwise leave nothing in the log but a bare `! :load`.
