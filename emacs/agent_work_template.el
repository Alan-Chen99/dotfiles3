;; -*- lexical-binding: t -*-
;;
;; Agent work template for interactive Emacs sessions.
;;
;; Usage:
;;   cp emacs/agent_work_template.el /tmp/agent-work.el
;;   # edit the WORK SECTION in /tmp/agent-work.el
;;   nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" env GDK_BACKEND=x11 emacs -l /tmp/agent-work.el 2>/dev/null
;;   grep -A9999 -- '----start----' /tmp/debug.log
;;
;; The xvfb-run command runs Emacs on a virtual display so it doesn't
;; appear on screen.  GDK_BACKEND=x11 makes PGTK Emacs use the X11
;; backend (which xvfb provides) instead of looking for Wayland.
;;
;; Key rules:
;;   - This file is SELF-CONTAINED. Do NOT add -e/--eval flags.
;;   - Do NOT use --batch. It skips normal config and (require 'alan) fails.
;;   - All output goes to /tmp/debug.log (not stdout/stderr).
;;     Use `span-msg` to log; read the log file after emacs exits.
;;   - This file must RETURN before Emacs startup completes.
;;     All work MUST go on timers (run-with-timer), not at top level.
;;   - Always end with (kill-emacs 0) inside your work timer.

;; --- setup (do not modify) -------------------------------------------

(require 'alan)

(elpaca-process-queues)

(defvar log-file "/tmp/debug.log")
(setq span-max-width 100) ;; truncate each line in log

;; defers and written as batch on timers
(setq span-log-handler
      (lambda (msg)
        (let ((inhibit-interaction t))
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
