;; -*- lexical-binding: t -*-
;;
;; Agent work template for interactive Emacs sessions.
;;
;; Emacs runs on a virtual display so it stays off the user's screen.
;; GDK_BACKEND=x11 makes PGTK Emacs use the X11 backend xvfb provides.
;; --user "" is required here: without it Emacs skips init.el and
;; early-init.el, and (require 'alan) fails with nothing on stderr.
;;
;; Two ways to run this file, differing only in how the WORK SECTION ends.
;;
;; 1. SESSION -- run it unchanged, straight from the repo.  It parks
;;    instead of exiting, and you query it with emacsclient.
;;
;;      : > /tmp/debug.log   # else the wait below matches the LAST run
;;      agent-tools run --background --desc "emacs session" \
;;        nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" \
;;        env GDK_BACKEND=x11 emacs --user "" \
;;        -l /repos/dotfiles/emacs/agent_work_template.el
;;
;;      timeout 180 grep -m1 -a -E '^[0-9.]+ +% ----parked----' \
;;        < <(tail -n +1 -F --retry /tmp/debug.log)
;;
;;      SOCK=$(sed -n '1s/.* socket \([^ ]*\) .*/\1/p'           /tmp/debug.log)
;;      PID=$( sed -n '1s/^==== span run pid \([0-9]*\) .*/\1/p' /tmp/debug.log)
;;      timeout 30 emacsclient -s "$SOCK" --eval '(length (buffer-list))'
;;      kill "$PID"
;;
;;    `--background' is required, the run never returns on its own.  It
;;    also means `timeout' bounds nothing: it wraps the wrapper, which
;;    returns as soon as the child starts.  `kill' is what ends a session,
;;    and the pid to kill is the one on line 1 of the log -- not the "child
;;    pid" the wrapper prints, which is the nix/xvfb wrapper.  Killing it
;;    ends the whole run, and SIGTERM runs `kill-emacs-hook', so the
;;    shutdown is logged.
;;
;; 2. WORK FILE -- copy it, edit the WORK SECTION, and end with
;;    (kill-emacs 0) instead of (agent-park), so the run exits by itself.
;;    Cheaper when you already know what you want to find out.
;;
;;      cp emacs/agent_work_template.el /tmp/agent-work.el
;;      timeout 300 agent-tools run --desc "emacs agent work" \
;;        nix shell nixpkgs#xvfb-run -c xvfb-run -a -s "-screen 0 1920x1080x24" \
;;        env GDK_BACKEND=x11 emacs --user "" -l /tmp/agent-work.el
;;
;; Run either under `agent-tools run'.  span reports a failure of the log
;; handler on stderr -- the one failure the log itself cannot carry -- and
;; agent-tools passes stderr through.
;;
;; Key rules:
;;   - This file is SELF-CONTAINED.  Do NOT add -e/--eval flags.
;;   - Do NOT use --batch.  It skips normal config and (require 'alan) fails.
;;   - All output goes to /tmp/debug.log, not stdout.  Log with `span-msg'.
;;   - This file must RETURN before startup completes, so all work goes on
;;     timers, never at top level.  Top level also runs before anything the
;;     timer `require's has configured itself, so read config values inside
;;     the timer; a top-level `defvar' captures the pre-`require' value.
;;   - End the work timer with `agent-park' or (kill-emacs 0).
;;   - NEVER use condition-case.  Use condition-case-unless-debug, which logs.
;;
;; querying a session:
;;   - Take the socket from LINE 1 of the log.  NEVER run emacsclient
;;     without -s, or with a guessed name: this directory also holds the
;;     user's own editors, named server<pid>, and a bare or mistyped
;;     emacsclient evaluates your form inside one of those.  The session is
;;     agent-work-<pid>, outside that namespace, so a typo exits 2.
;;
;;   - WRAP EVERY QUERY: `agent-q' to ask something, `agent-async' to do
;;     something.  A bare --eval is answered exactly as stock Emacs would
;;     answer it -- nothing logged, nothing bounded -- and that is
;;     deliberate.  The Emacs under test may use its own server, and a
;;     reply shaped for an agent would be wrong for those callers, silently
;;     so: `server-eval-at' `read's what comes back, and a bounded
;;     rendering of (number-sequence 1 10000) reads without error as a
;;     27-element list ending in the symbol `...'.  So the instrumentation
;;     is opt-in, per query, and the server itself is left alone.
;;
;;   - `agent-q' returns a STRING -- the rendering of the value, not the
;;     value.  That is what bounds it: 77 bytes for the list above against
;;     51143 bare.  It also logs `:query BODY', whatever BODY logged, and
;;     `-> VALUE', flushing on both sides, so the log is complete when the
;;     client returns and a query stuck at a prompt shows as a `:query'
;;     span with no `->'.
;;
;;   - ONE FORM PER --eval.  A second one is dropped in silence and the
;;     client still exits 0 -- `server-eval-and-print' reads only the first
;;     -- so put everything inside the one `agent-q', where a `progn' runs
;;     it all.
;;
;;   - Check emacsclient's OWN exit status.  0 with a value is success; 1
;;     prints `*ERROR*: ...' for a Lisp error; 124 is the jam below; 0 with
;;     EMPTY output means the server died mid-request.  Piping into `head'
;;     reports head's 0 and hides all of these.  An error inside `agent-q'
;;     also leaves a `:span--debug' backtrace in the log; a bare --eval
;;     gets no backtrace, because `server-execute' handles the error before
;;     any debugger runs.
;;
;;   - PUT WORK ON A TIMER, with `agent-async', and never prompt in the
;;     eval itself.  Emacs serialises server requests (bug#71223, see
;;     `server--process-filter-active'), so an eval that stops at a prompt
;;     jams the whole session: every later emacsclient times out at 124 and
;;     the only way out is the X display, below.  A timer moves the work
;;     out of the filter, the client returns in milliseconds, and the
;;     session stays queryable THROUGH the prompt:
;;
;;       --eval (agent-async WORK)          ;; => :armed, at once
;;       --eval (agent-q (minibuffer-depth));; => "1", still answering
;;       --eval (agent-async (execute-kbd-macro (kbd "y")))   ;; answers it
;;
;;     THE EXIT STATUS NOW MEANS NOTHING about the work.  The client is
;;     gone before the timer runs, so rc is 0 whatever happens.  Read the
;;     log instead, where `agent-async' leaves three distinguishable
;;     states:
;;
;;       `<<done>> VALUE'  present            -- BODY returned
;;       absent, with a `:span--debug' entry  -- BODY signalled
;;       absent, with neither                 -- still running, or at a prompt
;;
;;     Never read `:armed' as success, and never take "no `*ERROR*'" for
;;     "no error"; run the failure grep before believing an async query
;;     worked.  Within a single eval the same trick works without a second
;;     client -- arm the answer before prompting:
;;
;;       (agent-q (run-with-timer 0.1 nil #'execute-kbd-macro (kbd "y"))
;;                (y-or-n-p "..."))
;;
;;   - `server-eval-at' to this session's OWN server deadlocks it: the
;;     outer request holds the filter, so the nested one is never served
;;     and Emacs waits in `accept-process-output' forever.  Only a kill
;;     ends that.
;;
;;   - If a query does jam the session, the log names it: the last `:query'
;;     span is the one with no `->' under it.  Recover through the X
;;     display, which the jam does not reach.  Do NOT kill the blocked
;;     client -- once the prompt is answered it returns its value normally,
;;     so run anything that might prompt under `agent-tools run
;;     --background' and read its capture file.  A client killed
;;     mid-request leaves the server logging `connection broken by remote
;;     peer' against the query it was still answering.
;;
;;       DISPLAY=:99            # from /proc/<pid>/environ of the session
;;       W=$(for w in $(xdotool search --onlyvisible --class emacs); do
;;             eval $(xdotool getwindowgeometry --shell $w)
;;             echo "$((WIDTH*HEIGHT)) $w"; done | sort -rn | head -1 | cut -d' ' -f2)
;;       xdotool windowfocus $W; xdotool key y      # or: xdotool type ...
;;
;;     The frame is picked by area because the class also matches a 10x10
;;     leader window and any warning popup, and PGTK Emacs cannot tell you
;;     the id itself -- its `outer-window-id' is nil.  There is no window
;;     manager, so `windowfocus' is what makes the key land; `xdotool key
;;     --window' on its own does not.
;;
;; reading the log:
;;   - The log holds exactly one run: the setup below empties it and writes
;;     a `==== span run' line naming the pid, the socket and the wall clock.
;;     Check that line 1's pid is alive before trusting anything below it --
;;     a reader that starts watching before the run truncates the file sees
;;     the PREVIOUS run's markers.
;;   - The `----start----' grep shows the work section and nothing before
;;     it.  An empty result does NOT mean nothing happened -- it means the
;;     run never reached the marker, which is what a work file that failed
;;     to load looks like.  Use the failure grep, which covers startup too.
;;     Anchor on `% ----start----': the bare string also occurs inside
;;     backtrace frames, because the work lambda's own source contains it.
;;   - The failure grep matches two things.  `:span--debug' is an error that
;;     reached the debugger, i.e. one nothing handled.  `!' marks any
;;     non-local exit, deliberate ones included -- `ignore-errors' in
;;     alan-early-init.el logs `! :set-startup-frame-size' on every startup
;;     here, because xvfb has no display size to report.
;;   - Use `grep -a'.  The log embeds raw subprocess output, so plain grep
;;     can classify the file as binary and print nothing at all.
;;   - `message' output lands in the log tagged `%%', not in *Messages*.
;;   - `span-max-width' truncates every logged line, leaving no marker at
;;     the cut.  Raise it before logging long values, and keep it clear of
;;     `span-fmt-print-limit' (the print budget, 100): a printed value ends
;;     near that column and the "..." marking an elided part sits just past
;;     it, so a width of 100 cuts off the ellipses themselves.
;;   - `span-msg' queues; the log is written on a 0.5s timer.  Use
;;     `span-msg-now' for a checkpoint that must survive a segfault or a
;;     SIGKILL -- it returns only once the entry is on disk.
;;   - Any file the work section writes ITSELF must bind
;;     `coding-system-for-write' to `utf-8-emacs-unix'.  Emacs strings hold
;;     raw bytes and characters above #x10FFFF, which no ordinary coding
;;     system encodes, so `write-region' stops in `select-safe-coding-system'
;;     on a `Select coding system' prompt -- which blocks the session like
;;     any other prompt.  The span log is already safe; that is what
;;     `span-file-log-handler' is for.
;;   - A span is logged only if something was logged inside it.
;;   - `debug-ignored-errors' is cleared in the setup below, so an unhandled
;;     `end-of-file' -- an unbalanced paren in your work file -- is logged
;;     with a backtrace instead of being skipped.

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

;; Name the server here rather than leaving it to `with-editor', which
;; starts one about a second in under the name server<pid> -- the same
;; namespace the user's own editors occupy in this directory, where a
;; mistyped name reaches a real editing session instead of failing.
;; `server--file-name' just expands `server-name' against that directory,
;; so the path is known before anything is listening and the log header
;; below can carry it.  `with-editor' reuses `server-name', so the path
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

;; The heartbeat is how the log distinguishes a session that is merely busy
;; from one that has stopped, and it carries the minibuffer depth because
;; that is the distinction you need when a query times out: timers keep
;; running inside a prompt, so the beat continues with mb>0, while an Emacs
;; wedged in Lisp runs no timers at all and the log simply stops.  Held in a
;; variable so `agent-park' can re-arm exactly this timer and not whatever
;; the work section scheduled.
(defvar agent-heartbeat-timer nil)

(defvar agent-park-heartbeat-seconds 30
  "Heartbeat period once parked.
1/sec is right while work runs and far too noisy for an idle session --
it would put thousands of lines between the work output and the next
read -- but stopping it altogether costs the liveness signal.")

(defun agent--heartbeat ()
  (span-msg "heartbeat mb=%d" (minibuffer-depth)))

(setq agent-heartbeat-timer (run-with-timer 0 1 #'agent--heartbeat))

;; log flushes every 0.5 seconds, at most this many entries
(setq span-message-limit-per-cycle 100000)

;; Last, deliberately.  An error here is a top-level error during `-l' and
;; abandons the rest of this file, so the log and the debugger override
;; have to be in place already; started earlier, a failure leaves no log
;; and no stderr at all.
(server-start)

;; --- parked session (do not modify) ----------------------------------

(defun agent-park ()
  "End the work section without exiting, leaving the session queryable.
Nothing reaps the session -- it runs until the pid on line 1 of the log is
killed."
  (cancel-timer agent-heartbeat-timer)
  (setq agent-heartbeat-timer
        (run-with-timer agent-park-heartbeat-seconds
                        agent-park-heartbeat-seconds
                        #'agent--heartbeat))
  (span-msg-now "----parked---- socket=%s" agent-socket))

;; NOTHING here replaces `server-eval-and-print'.  This template exists to
;; debug Emacs, and the Emacs being debugged may use its own server --
;; `with-editor', `org-protocol', a subprocess running emacsclient,
;; `server-eval-at' between instances.  An agent-shaped reply would be
;; wrong for every one of them, and silently so: `server-eval-at' `read's
;; what comes back, and a bounded rendering of (number-sequence 1 10000)
;; reads without error as a 27-element list ending in the symbol `...'.
;; Stock clients therefore get stock `pp' output, which is what is meant to
;; be read back.
;;
;; The agent opts in per query instead, with `agent-q' below.

;; Wrap a query in this to get the log entry, the bound, and the backtrace.
;; It returns a STRING -- the rendering, not the value -- so that no query
;; can flood the caller: (number-sequence 1 10000) is 75 bytes rendered
;; against 51143 from `pp'.  Being visibly a string, it also cannot be
;; mistaken for something to read back.
;;
;; `span--context' is here rather than around the whole request because
;; `server-execute' wraps the eval in a plain `condition-case': the error
;; is handled, so the debugger span builds backtraces with never runs.  The
;; innermost handler is the one that decides, and this is inside it.  span
;; wraps the process filter itself with the same `:server' context, but
;; that is too far out to affect this.
(defmacro agent-q (&rest body)
  "Evaluate BODY as an agent query; return a bounded rendering of its value.
Logs `:query' with BODY, whatever BODY logged, and `-> VALUE', flushed on
entry and on exit -- so a query still sitting at a prompt appears as a
`:query' span with no `->', and the log is complete the moment the client
returns."
  (let ((label (format "%S" (if (cdr body) (cons 'progn body) (car body)))))
    `(span--context :server
       (span (:query "%s" ,label)
         (span-flush)
         (span-flush-log)
         (let* ((v (progn ,@body))
                (s (substring-no-properties (span-fmt-to-string v))))
           (span-msg "-> %s" s)
           (span-flush-log)
           s)))))

;; The protocol for anything that is not a plain question.  BODY runs on a
;; timer, i.e. outside the server filter, which is what keeps the session
;; answering queries while BODY sits at a prompt -- see the header.
;;
;; It also fixes what the exit status cannot say.  The client returns the
;; instant BODY is armed, so its 0 means "armed", never "worked": an error
;; inside BODY arrives long after the client is gone.  `<<done>>' is logged
;; only by a BODY that returned, so the log carries three distinguishable
;; states -- `<<done>>' present is success, absent with a backtrace is
;; failure, absent with neither is still running or waiting at a prompt.
(defmacro agent-async (&rest body)
  "Arm BODY on a timer and return `:armed' at once.
Logs `<<done>> VALUE' if BODY returns.  If BODY signals, that line is
never written and `span--debug' logs the backtrace instead."
  `(progn
     (run-with-timer
      0 nil
      (lambda ()
        (let ((v (progn ,@body)))
          (span-msg "<<done>> %s"
                    (substring-no-properties (span-fmt-to-string v)))
          (span-flush-log))))
     :armed))

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
