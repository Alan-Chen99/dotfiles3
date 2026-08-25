;; -*- lexical-binding: t -*-
;;
;; Agent work template for interactive Emacs sessions.
;;
;; Two ways to run it.  Both use the same file; they differ only in how
;; the WORK SECTION ends.
;;
;; 1. SESSION -- what you get by running this file unchanged, straight
;;    from the repo.  Nothing to copy, nothing to edit.  It starts, logs,
;;    and then parks: Emacs stays up and you evaluate forms in it with
;;    emacsclient.  Run it in the background; it will not exit on its own.
;;
;;      timeout 900 agent-tools run --desc "emacs session" \
;;        nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" \
;;        env GDK_BACKEND=x11 emacs --user "" \
;;        -l /repos/dotfiles/emacs/agent_work_template.el
;;
;;      # wait for it to park (no sleep: block on the log)
;;      timeout 180 grep -m1 -a -E '^[0-9.]+ +% ----parked----' \
;;        < <(tail -n +1 -F --retry /tmp/debug.log)
;;
;;      SOCK=$(sed -n '1s/^==== span run pid [0-9]* socket \([^ ]*\) .*/\1/p' /tmp/debug.log)
;;      PID=$( sed -n '1s/^==== span run pid \([0-9]*\) .*/\1/p'              /tmp/debug.log)
;;
;;      timeout 30 emacsclient -s "$SOCK" --eval '(length (buffer-list))'
;;      kill "$PID"          # done; SIGTERM runs kill-emacs-hook and logs it
;;
;; 2. WORK FILE -- copy it, edit the WORK SECTION, and end with
;;    (kill-emacs 0) instead of (agent-park).  The run exits by itself and
;;    you just read the log.  This is the cheaper path when you already
;;    know what you want to find out.
;;
;;      cp emacs/agent_work_template.el /tmp/agent-work.el
;;      timeout 300 agent-tools run --desc "emacs agent work" \
;;        nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" \
;;        env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el
;;      grep -a -A9999 -- '% ----start----' /tmp/debug.log       # the work section
;;      grep -anE '^[0-9.]+ +! |span--debug' /tmp/debug.log      # failures, anywhere
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
;; Nothing inside Emacs bounds the run.  `timeout' around the command is
;; the whole story, and it is strictly better than an in-Emacs timer would
;; be: it catches a work section that never finishes, a file that failed to
;; load before arming anything, and an Emacs wedged inside Lisp -- which no
;; Emacs timer can catch, because a wedged Emacs runs no timers.
;;
;; Exceeding the timeout is not by itself a reason to kill.  The process is
;; still there, and a hung Emacs is usually the thing you wanted to look
;; at: leave it and attach, or kill it, as the situation warrants.
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
;;   - Always end the work timer with `agent-park' or (kill-emacs 0).
;;     Falling off the end leaves Emacs running with nothing scheduled --
;;     harmless in itself, but then only your `timeout' ends the run.
;;   - NEVER use condition-case. Use condition-case-unless-debug, which logs the error.
;;
;; querying a parked session:
;;   - The socket path is on LINE 1 of the log and nowhere else.  Take it
;;     from there.  Do not grep the body for it: a marker written in the
;;     WORK SECTION also appears in the `:timer' entry that prints the work
;;     lambda's source, so a body grep matches twice, and the source match
;;     carries the format string rather than the value.  Markers emitted by
;;     the setup below (`----parked----') do not have that problem, but they
;;     tell you WHEN the session is ready, not WHERE it is.
;;   - NEVER run emacsclient without -s, and never with a guessed name.
;;     The socket directory also holds the user's own editors, which are
;;     named server<pid>.  A bare or mistyped emacsclient silently
;;     evaluates your form inside one of them.  This session is named
;;     agent-work-<pid>, outside that namespace, so a typo exits 2 instead.
;;   - Check emacsclient's OWN exit status.  Piping it into head reports
;;     head's 0 and hides a timeout.
;;   - Ask for what you need -- (length x), (type-of x), (cl-count-if ...)
;;     -- rather than the object.  Results print through `pp' with
;;     `print-length' and `print-level' bound (see `agent-query-print-length'),
;;     which bounds most values but not one enormous string.
;;   - Do NOT return what `span-fmt-to-string' returns.  That string keeps
;;     the elided objects alive in its text properties, and printing it
;;     prints them: measured at 247x for a 174-element tail, and enough to
;;     take the session to 1GB and wedge it on a longer one.  Strip it with
;;     `substring-no-properties' first.
;;   - Nothing reaps a parked session.  It runs until you `kill' the pid on
;;     line 1 of the log, or the outer `timeout' ends it.  Confirm
;;     /proc/PID/cmdline names this file before killing anything.
;;   - Prefer `kill'.  Plain SIGTERM runs `kill-emacs-hook', so the
;;     shutdown lands in the log; the outer `timeout' does not reach Emacs
;;     that way and leaves the log ending wherever the last flush did,
;;     exactly as `kill -9' would.  Anything that must survive either one
;;     has to go through `span-msg-now'.
;;   - Bound each query with `timeout'; a form that never returns otherwise
;;     hangs the caller too.  Exit 124 means the session is wedged in Lisp:
;;     no timers run and the log is frozen at the last flush, so killing the
;;     pid is the only way out.
;;
;; reading the log:
;;   - The log holds exactly this run: the setup below empties it and writes
;;     a `==== span run' line naming the pid, the socket and the wall clock.
;;     `head -1' is how you tell a fresh log from one left by an earlier
;;     attempt -- and a stale log is easy to hit, because a reader that
;;     starts watching before the run has truncated the file sees the
;;     previous run's markers.  Check that line 1's pid is alive.
;;   - The `----start----' grep shows the work section and nothing before it.
;;     An empty result does NOT mean nothing happened -- it means the run
;;     never reached the marker, which is what a work file that fails to
;;     load looks like.  Read the whole log, or the failure grep, instead.
;;     Anchor on `% ----start----': the bare string also occurs inside
;;     backtrace frames, because the work lambda's own source contains it.
;;   - The failure grep matches two different things.  `:span--debug' is an
;;     error that reached the debugger, i.e. one nothing handled.  `!' marks
;;     any non-local exit, deliberate ones included -- the `ignore-errors' in
;;     alan-early-init.el logs `! :set-startup-frame-size' on every startup
;;     here, because xvfb has no display size to report.
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
;;   - `span-max-width' truncates every logged line, leaving no marker at
;;     the cut.  Raise it before logging long values.  Keep it clear of
;;     `span-fmt-print-limit' (the print budget, 100): a printed value
;;     ends near that column, so the "..." that marks an elided part sits
;;     just past it, and a width of 100 cuts off the ellipses themselves.
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

;; Before `require', because this is what decides whether the `require'
;; below reads alan.el or a stale alan.elc.  alan.el sets this too, but by
;; then it has already been loaded from whichever file won.
(setq load-prefer-newer t)

(require 'alan)

(elpaca-process-queues)

(defvar log-file "/tmp/debug.log")
;; Cut each logged line here.  200 rather than 100 because
;; `span-fmt-print-limit' is 100: printed values end around that column
;; and their "..." markers sit just past it, so a width of 100 removes
;; the only sign that anything was elided -- and buys about 2% of the log
;; for it.  Raise this further before logging long values.
(setq span-max-width 200)

;; Name the query server here so the log header can carry its socket path.
;; `server--file-name' only expands `server-name' against the socket
;; directory, so the path is known before anything is listening; the server
;; itself starts at the END of this setup, once a failure there would be
;; logged rather than aborting the load in silence.
;;
;; The config does eventually start a server on its own -- `with-editor'
;; does it about a second in -- but relying on that costs two things.  The
;; header is written before it happens, so the socket would have to be
;; recovered by grepping the body; and that server is named server<pid>,
;; the same namespace the user's own editors occupy in this directory, so
;; a mistyped name reaches a real editing session instead of failing.
;; Naming it ourselves fixes both.  `with-editor' still calls
;; `server-start' later, but it reuses `server-name', so the path below
;; stays valid.
(require 'server)
(setq server-name (format "agent-work-%s" (emacs-pid)))
(defvar agent-socket (server--file-name))

;; The log is opened for append and nothing else truncates it, so without
;; this a re-run leaves two runs in one file -- both timestamp series
;; starting at zero -- and a reader anchored on the first marker it finds
;; silently gets the older run.  The header line carries the only absolute
;; time, the only pid, and the only socket path in the file.
(span-file-log-reset log-file
                     (format "==== span run pid %s socket %s started %s ===="
                             (emacs-pid)
                             agent-socket
                             (format-time-string "%Y-%m-%d %H:%M:%S%z")))

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

;; Last, deliberately.  An error here is a top-level error during `-l',
;; which abandons the rest of this file -- so anything the diagnosis needs
;; has to already be in place: the log to write to, the debugger override
;; that turns the error into a logged backtrace.  Started any earlier, a
;; failure leaves nothing at all behind -- no log, no stderr -- and the run
;; just sits there until your `timeout' ends it.
(server-start)

;; --- parked session (do not modify) ----------------------------------

(defvar agent-query-print-length 200
  "`print-length' for values returned to emacsclient.")

(defvar agent-query-print-level 6
  "`print-level' for values returned to emacsclient.")

(defun agent-park ()
  "End the work section without exiting, leaving the session queryable.
Nothing reaps the session: it runs until the caller kills the pid on line 1
of the log, or the outer `timeout' ends it."
  ;; The 1/sec heartbeat shows the run is alive during work.  A parked
  ;; session can idle for many minutes, and every one of those lines lands
  ;; between the work output and whatever is read next.
  (cancel-function-timers #'span-msg)
  (span-msg-now "----parked---- socket=%s" agent-socket))

;; Bound the value sent back to emacsclient.  `server-eval-and-print' pp's
;; the result with whatever print settings are current, so an unbounded
;; value is tens of KB of the caller's context, and a circular one -- easy
;; to reach from a live buffer or marker -- never finishes printing at all.
(defun agent--bounded-print (orig expr proc)
  (let ((print-length agent-query-print-length)
        (print-level agent-query-print-level)
        (print-circle t))
    (funcall orig expr proc)))

(advice-add 'server-eval-and-print :around #'agent--bounded-print)

;; --- WORK SECTION ----------------------------------------------------
;;
;; Runs on a timer, after startup.  It MUST end with one of:
;;   (agent-park)    leave the session up for emacsclient  [default]
;;   (kill-emacs 0)  run to completion and exit
;;
;; Driving interactive Emacs from here looks like this -- the pattern, and
;; the log it produces:
;;
;;   (run-with-timer 0.1 nil #'execute-kbd-macro (kbd "y"))
;;   (with-timeout (0.2)
;;     (span-msg "result: %S" (y-or-n-p "test")))
;;
;;   1.357   :read-from-minibuffer test (y or n)
;;   1.457     :timer execute-kbd-macro
;;   1.458       ! :timer
;;   1.458   %% test (y or n) y      ;; captured from the `message' call
;;   1.459   % result: t

(run-with-timer
 3 nil
 (lambda ()
   (span-msg "----start----") ;; typically only read logs after markers like this

   (span-msg "log-file: %S" log-file)
   (span-msg "socket:   %s" agent-socket)
   (span-msg "%S" source-directory) ;; this exact revision should be used for code searching

   ;; Leave it up to be queried.  Replace with (kill-emacs 0) in a copied
   ;; work file that should run and exit on its own.
   ;; DO NOT kill all emacs running and NEVER kill the emacs you are running in
   (agent-park)))

;; this file must finish before startup can happen! put all work on timers
