;; -*- lexical-binding: t -*-
;;
;; Agent work template for interactive Emacs sessions.
;;
;; Usage:
;;   cp emacs/agent_work_template.el /tmp/agent-work.el
;;   # edit the WORK SECTION in /tmp/agent-work.el
;;   agent-tools run --desc "emacs agent work" nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el
;;   grep -a -A9999 -- '----start----' /tmp/debug.log
;;
;; Run it under `agent-tools run'.  span reports a failure of the log handler
;; on stderr -- the one failure the log itself cannot carry -- and
;; agent-tools captures stderr and passes it through, so no redirect is
;; needed to see it.
;;
;; The xvfb-run command runs Emacs on a virtual display so it doesn't
;; appear on screen.  GDK_BACKEND=x11 makes PGTK Emacs use the X11
;; backend (which xvfb provides) instead of looking for Wayland.
;;
;; --user "" is required on some shared systems where Emacs cannot
;; determine the current user from the environment (e.g. NFS homes,
;; container-mapped UIDs).  Without it, Emacs skips loading init.el
;; and early-init.el, so (require 'alan) fails silently with no
;; stderr output.
;;
;; Key rules:
;;   - This file is SELF-CONTAINED. Do NOT add -e/--eval flags.
;;   - Do NOT use --batch. It skips normal config and (require 'alan) fails.
;;   - All output goes to /tmp/debug.log (not stdout/stderr).
;;     Use `span-msg` to log; read the log file after emacs exits.
;;   - This file must RETURN before Emacs startup completes.
;;     All work MUST go on timers (run-with-timer), not at top level.
;;   - Top level runs before the work timer, and before anything the timer
;;     `require's has configured itself.  Read config values inside the
;;     timer; a top-level `defvar' captures the pre-`require' value.
;;   - Always end with (kill-emacs 0) inside your work timer.
;;   - NEVER use condition-case. Use condition-case-unless-debug, which logs the error.
;;
;; reading the log:
;;   - Use `grep -a'.  The log embeds raw subprocess output, including
;;     remote shell transcripts, so plain grep can classify the file as
;;     binary and print nothing at all -- a silent false "no matches".
;;   - Characters above #x10FFFF (consult appends them to candidates as
;;     invisible "tofu" markers) land in the log as multi-byte garbage.
;;     That is expected; the entry around them is intact.
;;   - Any file the work section writes ITSELF must bind
;;     `coding-system-for-write' to `utf-8-emacs-unix'.  Writing a consult
;;     candidate or a buffer of raw bytes without it sends `write-region'
;;     into `select-safe-coding-system', which prompts and hangs Emacs with
;;     no output on stdout or stderr.  The span log is already safe -- that
;;     is what `span-file-log-handler' is for.
;;   - `message' output lands in the log tagged `%%', not in *Messages*.
;;     The advice on `message' logs the text and binds `message-log-max'
;;     to nil for the real call, so *Messages* stays empty here.
;;   - `span-max-width' truncates every logged line.  Raise it before
;;     logging long values or they are cut mid-line with no marker.
;;
;; logging framework:
;;   - A span is logged if there are any messages within it
;;   - `!` at the end of the span indicate a non-local exit (error or throw). It is otherwise a normal exit.
;;   - A value whose printer signals renders as an empty string: `cl-prin1'
;;     demotes the error, so the entry reads `x: ' with nothing after it and
;;     the reason arrives separately as `%% cl-prin1: ...'.
;;   - `span-msg' queues; the log is written on a 0.5s timer.  Use
;;     `span-msg-now' for a checkpoint that must survive a segfault or an
;;     external kill -- it returns only once the entry is on disk.
;;   - A log handler that signals destroys its whole batch.  Each failure is
;;     reported on stderr as `span: log handler failed (N consecutive)';
;;     the matching in-band note only survives if the sink recovers.
;;   - Errors past `span-debugger-rearm-limit-per-cycle' in one flush cycle
;;     carry no backtrace; a `warning: debugger re-armed' note marks that point.
;;   - `debug-ignored-errors' is cleared in the setup below, so an unhandled
;;     `end-of-file' or `user-error' is logged with a backtrace instead of
;;     being silently skipped.

;; --- setup (do not modify) -------------------------------------------

(require 'alan)

(elpaca-process-queues)

(defvar log-file "/tmp/debug.log")
(setq span-max-width 100) ;; truncate each line in log; raise for long values

;; Written as a batch on a 0.5s timer.  `span-file-log-handler' is the
;; supported file sink: it keeps the write off Tramp, off lock files, and
;; out of `select-safe-coding-system'.  Do not hand-roll this -- every one
;; of those is a way to hang the run with an empty log.
;; A function, not a plain path: the work section may still redirect the
;; log by setting `log-file', and the handler follows it.
(setq span-log-handler (span-file-log-handler (lambda () log-file)))

;; use non-interactive debugger that prints to logs
(advice-add #'debug :override #'span--debug)

;; span logs an error by way of the debugger, and Emacs skips the debugger
;; for anything in `debug-ignored-errors' -- which by default holds
;; `end-of-file', `user-error', `search-failed' and friends.  That hides
;; real failures, including an unbalanced paren in this very file, which
;; would otherwise leave nothing behind but a bare `! :load'.  An error a
;; `condition-case' catches still does not reach the debugger, so clearing
;; this only surfaces errors that really went unhandled.
;; Packages loaded later re-add their own entries as they load.
(setq debug-ignored-errors nil)

;; xvfb has no window manager, so "maximized" doesn't work.
;; force a reasonable frame size for agent work.
(run-with-timer 0.5 nil (lambda () (set-frame-size (selected-frame) 120 40)))

;; check if things are running and whether we are at top-level or no
(run-with-timer 0 1 #'span-msg "heartbeat")

;; log flushes every 0.5 seconds, at most this many entries
(setq span-message-limit-per-cycle 100000)

;; --- WORK SECTION (example) ---------------------------------------

(run-with-timer
 3 nil
 (lambda ()
   (span-msg "----start----") ;; typically only read logs after markers like this

   (span-msg "log-file: %S" log-file)
   (span-msg "%S" source-directory) ;; this exact revision should be used for code searching
   (span-msg "%S" (locate-library "evil-collection"))

   ;; successful execution generate this log:
   ;; 1.357   :read-from-minibuffer test (y or n)
   ;; 1.457     :timer execute-kbd-macro
   ;; 1.458       ! :timer
   ;; 1.458   %% test (y or n) y ;; captured from `message' call
   ;; 1.459   % result: t
   (run-with-timer 0.1 nil #'execute-kbd-macro (kbd "y"))
   (with-timeout (0.2)
     (span-msg "result: %S" (y-or-n-p "test")))

   ;; make sure emacs instance is killed, this or externally
   ;; DO NOT kill all emacs running and NEVER kill the emacs you are running in
   (kill-emacs 0)))

;; this file must finish before startup can happen! put all work on timers
