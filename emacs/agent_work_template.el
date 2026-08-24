;; -*- lexical-binding: t -*-
;;
;; Agent work template for interactive Emacs sessions.
;;
;; Usage:
;;   cp emacs/agent_work_template.el /tmp/agent-work.el
;;   # edit the WORK SECTION in /tmp/agent-work.el
;;   nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el 2>/dev/null
;;   grep -a -A9999 -- '----start----' /tmp/debug.log
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
;;   - Any log file you write yourself MUST bind `coding-system-for-write'
;;     to `utf-8-emacs-unix'.  Formatting a consult candidate or a buffer
;;     of raw bytes into a `write-region' without it prompts for a coding
;;     system and hangs Emacs with no output on stdout or stderr.
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
;;   - A log handler that signals destroys its whole batch.  The next batch
;;     carries `warning: log handler failed, N entries lost'.
;;   - Errors past `span-debugger-rearm-limit-per-cycle' in one flush cycle
;;     carry no backtrace; a `warning: debugger re-armed' note marks that point.

;; --- setup (do not modify) -------------------------------------------

(require 'alan)

(elpaca-process-queues)

(defvar log-file "/tmp/debug.log")
(setq span-max-width 100) ;; truncate each line in log; raise for long values

;; defers and written as batch on timers
(setq span-log-handler
      (lambda (msg)
        ;; `utf-8-emacs-unix' encodes every character a Lisp string can hold,
        ;; including raw bytes and the above-#x10FFFF characters consult
        ;; appends to its candidates.  Leaving the coding system unspecified
        ;; sends `write-region' into `select-safe-coding-system', which finds
        ;; no safe choice and prompts -- hanging Emacs, or with
        ;; `inhibit-interaction' signalling and dropping the whole log batch.
        (let ((coding-system-for-write 'utf-8-emacs-unix)
              (inhibit-interaction t))
          (write-region msg nil log-file t 'no-message ""))))

;; use non-interactive debugger that prints to logs
(advice-add #'debug :override #'span--debug)

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
