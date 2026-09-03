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

Agents debugging Emacs MUST use `emacs/agent_work_template.el`, on a
virtual display so it stays off the user's screen. Its header is the
governing reference — read it before querying a session.

MUST verify emacs runs BEFORE exploring code or starting related work.
SHOULD NOT use `--batch` — it skips config loading and `(require 'alan)` fails.
MUST NOT ask users to run your scripts for interactive emacs.
MUST NOT use code to find something that can be found by running emacs.

A **session** — run the template unchanged. It parks instead of exiting,
and you evaluate forms in it with `emacsclient`.

```sh
: > /tmp/debug.log   # else the wait below matches the LAST run's marker
agent-tools run --background --desc "emacs session" nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs --user "" -l /repos/dotfiles/emacs/agent_work_template.el
timeout 180 grep -m1 -a -E '^[0-9.]+ +% ----parked----' < <(tail -n +1 -F --retry /tmp/debug.log)
SOCK=$(sed -n '1s/.* socket \([^ ]*\) .*/\1/p'           /tmp/debug.log)
PID=$( sed -n '1s/^==== span run pid \([0-9]*\) .*/\1/p' /tmp/debug.log)
timeout 30 emacsclient -s "$SOCK" --eval '(length (buffer-list))'
kill "$PID"   # nothing else reaps it
```

`--background` is required; the run never returns. It also means `timeout`
bounds nothing — it wraps the wrapper, which returns once the child starts.
`kill` ends a session, and the pid is line 1's, not the "child pid" the
wrapper prints (that is the nix/xvfb wrapper).

A **work file** — copy it, edit the WORK SECTION, and end with
`(kill-emacs 0)` in place of `(agent-park)` so the run exits by itself.
Cheaper when you already know what you want to find out.

```sh
cp emacs/agent_work_template.el /tmp/agent-work.el
timeout 300 agent-tools run --desc "emacs agent work" nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el
```

Run either under `agent-tools run`: span reports a log-handler failure on
stderr — the one failure the log cannot carry — and agent-tools passes
stderr through.

### Querying a session

The template header has the rest. Take the socket from line 1. NEVER run
`emacsclient` without `-s` or with a guessed name — the socket directory
also holds the user's own editors, named `server<pid>`, and a bare or
mistyped `emacsclient` evaluates your form inside one of them.

Wrap every query: `agent-q` to ask something, `agent-async` to do
something. A bare `--eval` is answered exactly as stock Emacs would answer
it — nothing logged, nothing bounded — deliberately, because the Emacs
under test may use its own server and an agent-shaped reply would be wrong
for those callers, silently so (`server-eval-at` `read`s the reply). The
instrumentation is opt-in per query; the server itself is left alone.

`agent-q` returns a *string* rendering of the value, which is what bounds
it, and logs `:query BODY` … `-> VALUE`, flushed on both sides. Put
everything inside the one `agent-q`: a second form in the same `--eval` is
dropped in silence and the client still exits 0.

Never prompt in the eval itself. Emacs serialises server requests, so an
eval that stops at a prompt jams the whole session and every later query
times out at 124. `agent-async` runs the work on a timer, returns in
milliseconds, and keeps the session queryable *through* the prompt:

```sh
--eval '(agent-async WORK)'                          # => :armed, at once
--eval '(agent-q (minibuffer-depth))'                # => "1", still answering
--eval '(agent-async (execute-kbd-macro (kbd "y")))' # answers it
```

**An exit status of 0 from `agent-async` says nothing about the work.** The
client is gone before the timer runs. The log carries the real outcome:
`<<done>> VALUE` present means it returned, absent with a `:span--debug`
backtrace means it signalled, absent with neither means it is still running
or waiting at a prompt. Run the failure grep before believing an async
query worked.

An error inside `agent-q` gives `*ERROR*: ...` with rc 1 *and* a backtrace
in the log; a bare `--eval` gives rc 1 with no backtrace. Otherwise exit
status is 0 with a value, 124 jammed, or 0 with *empty* output when the
server died mid-request. `server-eval-at` aimed at this session's own
server deadlocks it, and only a kill ends that.

### Reading the log

Read it the same way in both modes — a session writes it while parked.
Two greps, and you need both:

```sh
grep -a -A9999 -- '% ----start----' /tmp/debug.log   # the work section
grep -anE '^[0-9.]+ +! |span--debug' /tmp/debug.log  # failures, anywhere
```

`-a` is required: the log embeds raw subprocess output, so plain `grep` can
classify it as binary and print nothing at all. `message` output appears
here tagged `%%`, not in `*Messages*`.

An empty first grep does **not** mean nothing happened — it means the run
never reached the marker, which is what a work file that fails to load
looks like. Fall back to the failure grep, which covers startup too.

In the failure grep, `:span--debug` is an error that reached the debugger,
i.e. one nothing handled. `!` marks any non-local exit, deliberate ones
included — `ignore-errors` in `alan-early-init.el` logs
`! :set-startup-frame-size` on every startup under xvfb.

One file holds one run: the template empties the log and writes a
`==== span run` header naming pid, socket and wall clock. Check that line
1's pid is alive before trusting anything below it, because a reader that
starts watching before the run truncates the file sees the previous run's
markers.

Any file the work section writes itself MUST bind `coding-system-for-write`
to `utf-8-emacs-unix`, or `write-region` stops on a coding-system prompt
and hangs Emacs with empty stdout. `span-msg` queues; `span-msg-now`
returns only once the entry is on disk.
