;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'span-fmt)
;; (cl-declaim (optimize (safety 0) (speed 3)))
(eval-when-compile
  (require 'backtrace))
(autoload 'backtrace-print-to-string "backtrace")
(autoload 'backtrace-to-string "backtrace")
(autoload 'backtrace-get-frames "backtrace")
(autoload 'backtrace--expand-ellipsis "backtrace")
(autoload 'backtrace--to-string "backtrace")


;;; Overview
;;
;; A span is a named region of execution; notes attach to the span that
;; encloses them.  A span reaches the log only if something was logged
;; inside it, so instrumentation that stays quiet costs only a push and a
;; pop.
;;
;; There are two independent "flush" steps.  Confusing them is the most
;; common mistake:
;;
;;   `span-flush'      emits the enclosing spans' headers into the pending
;;                     list, so notes recorded afterwards appear under the
;;                     right span.  Reaches no sink.
;;   `span-flush-log'  hands the whole pending list to `span-log-handler'.
;;                     This is the step that reaches a buffer, stdout or a
;;                     file.  Normally driven by a 0.5s timer.
;;
;; Recording an entry, cheapest first:
;;
;;   `span-note'    record; formatting is deferred to flush time.  Emits
;;                  nothing until the enclosing span is logged for some
;;                  other reason.
;;   `span-notef'   as `span-note' but runs `span-flush' first, so the entry
;;                  appears even when the enclosing span is otherwise quiet.
;;                  The normal choice for instrumentation.
;;   `span-msg'     formats eagerly with `format-message', then `span-notef'.
;;                  A plain function, so it takes no (:ts ...) / (:unsafe ...)
;;                  format specs -- those are macro-only, see span-fmt.el.
;;   `span-msg-now' as `span-msg', then `span-flush-log': the entry is in the
;;                  sink before the call returns.  Roughly 25x the cost of
;;                  `span-msg', so it is a checkpoint tool, not a default.
;;   `span-dbg' / `span-dbgf'   log `expr: value' pairs, deferred / flushed.
;;
;; `span-log-handler' receives one string per flush.  That string is an
;; ordinary Emacs string: it may carry text properties, raw 8-bit bytes, and
;; characters above #x10FFFF (consult appends those to its candidates).  A
;; handler must cope with all of it; `span-file-log-handler' is the worked
;; example.  Encoding belongs to the handler, not here, because the shipped
;; sinks disagree about it -- a buffer and `message' both want the string as
;; it is, and only a file wants bytes.
;;
;; `span-file-log-handler' only appends.  `span-file-log-reset' empties the
;; file first, so that one file holds one run rather than a pile of runs
;; whose timestamps all start at zero.
;;
;; If a handler signals, the batch it was handed is already off the pending
;; list and is gone.  That failure is reported on stderr rather than through
;; the log, because the log is the channel that just failed.

(defmacro span-fmt (&rest body)
  "Evaluate BODY as a span format spec and return the formatted string.
See span-fmt.el for the spec language.  The short version: a bare
expression and (:ts EXPR) are evaluated AND formatted at the call site,
while (:unsafe EXPR) and (:unsafe-ts EXPR) capture EXPR now and format it
at flush time -- so a printer that signals aborts the caller in the first
form and degrades to a log entry in the second."
  (cl-destructuring-bind (fn . val) (span-fmt-parse body)
    `(funcall ,fn ,val)))

;; allowed entries in "span--stack":
;; (span-s . time)
;; (span-s . (time . obj))
;; (span-s . span) ; "normalized" per span--unsafe-top-obj-normalized

;; allowed entries in "notes":
;; (span-s . time)
;; (span-s . (time . obj))

;; allowed entries in "span--pending-log-list":
;; (depth span-s time . obj)
(eval-and-compile
  (cl-defstruct (span-s (:constructor span-s<-create)
                        (:copier nil)
                        (:conc-name span-s<-))
    "stores static data for a span or note.
designed to be created at compile time and used as constant"
    tag
    fmt-fn
    ;; fmt-static
    (file (or (bound-and-true-p byte-compile-current-file) buffer-file-name))
    (line (or (when (fboundp 'byte-compile--warning-source-offset)
                (byte-compile--warning-source-offset))
              (when buffer-file-name (point))))))

(eval-and-compile
  (defun span--parse-fmt-spec (args tag)
    (cond
     ((not args)
      (span--parse-fmt-spec `(,"") tag))

     (t
      (cl-destructuring-bind (fn . val) (span-fmt-parse args)
        ;; (when (bound-and-true-p byte-native-compiling)
        ;;   (setq fn (native-compile fn)))
        (let ((s `(eval-when-compile
                    (span-s<-create
                     :tag ,tag
                     :fmt-fn ,fn))))
          (if val
              `(cons ,s (cons (span--time) ,val))
            `(cons ,s (span--time))))))))

  (defun span--parse-span-spec (obj)
    (cond
     ((keywordp obj)
      (span--parse-fmt-spec nil obj))

     ((and (listp obj) (keywordp (car-safe obj)))
      (span--parse-fmt-spec (cdr obj) (car obj)))

     (t (error "invalid args")))))

;; (defmacro span-log (&rest args)
;;   (declare (indent 1))
;;   `(span--note ,(span--parse-fmt-spec args nil)))

(cl-defstruct (span (:constructor span<-create)
                (:copier nil)
                (:conc-name span<-))
  time
  obj

  depth
  logged

  notes
  )

(defsubst span--normalize-payload (e)
  (cond
   ;; (time . obj)
   ((consp (car e))
    (span<-create :time (car e) :obj (cdr e)))

   ;; time
   (t
    (span<-create :time e))))


(defvar span--handles-message t)

(defun span--dont-clear-message ()
  'dont-clear-message)

(defmacro span-with-no-minibuffer-message (&rest forms)
  `(let
       ((inhibit-message t)
        (set-message-function #'always)
        (clear-message-function #'span--dont-clear-message))
     ,@forms))


;; see span-s docs
(defvar span--stack (list
                     (cons (eval-when-compile (span-s<-create :tag ::))
                           (span<-create :depth -1 :logged t))))

(defvar span--cur-context :redisplay)
;; :redisplay :command :timer :require

(defvar span--context-locals nil)
(defvar span--blocking-time nil)

(defun span-var (sym)
  "Value of SYM in the current span context, as a settable place.

`span--context-locals' is rebound by `span--context', so a value stored
here lasts for the current redisplay, command, or timer callback and is
gone in the next one.  Use it for state that should not leak between
contexts."
  (alist-get sym span--context-locals))

(gv-define-expander span-var
  (lambda (do sym)
    (gv-letplace (getter setter) `(alist-get ,sym span--context-locals)
      (funcall do getter setter))))

(defsubst span--time ()
  (let (current-time-list)
    (current-time)))

(defsubst span--unsafe-top-obj-normalized (stack)
  (let* ((top (car stack))
         (val (cdr top)))
    (if (recordp val) ; unormalized form cannot be a record
        val
      (setf (cdr top) (setq val (span--normalize-payload val)))
      val)))


(defsubst span--unsafe-note (e)
  (let ((top (span--unsafe-top-obj-normalized span--stack)))
    (if (span<-logged top)
        (span--log-note e (span<-depth top))
      (push e (span<-notes top))))
  nil)

(defun span--note (e)
  (let ((inhibit-quit t))
    (span--unsafe-note e)))

(defun span--note-and-flush (e)
  ;; TODO: this can be more efficient
  (let ((inhibit-quit t))
    (span--unsafe-flush-stack)
    (span--unsafe-note e)))

(defmacro span-note (&rest args)
  "Record a note on the innermost span, formatting it at flush time.

ARGS is a span format spec (see `span-fmt'), optionally preceded by
:tag TAG to change the marker the entry is printed with.

The note is buffered on the enclosing span and is written only if that
span is logged -- which happens when anything else inside it is flushed,
or when it exits non-locally.  A span in which nothing but `span-note'
ran therefore stays invisible.  That is the point: this is the form to
reach for when instrumenting a hot path, where the common case should
cost nothing.  Use `span-notef' when the entry must appear regardless."
  (declare (indent 0))
  (let (tag)
    (when (eq (car-safe args) :tag)
      (pop args)
      (setq tag (pop args)))
    `(span--note ,(span--parse-fmt-spec args tag))))

(defmacro span-notef (&rest args)
  "Record a note on the innermost span and make it visible.

Like `span-note', but runs `span-flush' first, so the enclosing spans are
emitted and this entry is guaranteed to appear rather than depending on
something else in the span being logged.  The `f' is for that stack
flush; it does NOT write to `span-log-handler' -- that still waits for
`span-flush-log' on the next timer tick.  Use `span-msg-now' when the
entry has to be in the sink before the call returns.

This is the normal choice for instrumentation that should always show up."
  (declare (indent 0))
  (let (tag)
    (when (eq (car-safe args) :tag)
      (pop args)
      (setq tag (pop args)))
    `(span--note-and-flush ,(span--parse-fmt-spec args tag))))

(defun span-msg (&rest args)
  "Format ARGS with `format-message' and record the result via `span-notef'.

Use this to log a value already in hand.  Being an ordinary function, it
evaluates and formats ARGS at the call site, so it accepts no (:ts ...)
or (:unsafe ...) format specs -- those are understood only by the macros
`span-note', `span-notef', `span-dbg' and `span', and written here a spec
like (:unsafe-ts x) is read as a function call and signals `void-function'.

Formatting at the call site also means a value whose printer signals
aborts the caller.  To defer that risk to flush time, use
`span-notef' with (:unsafe-ts VALUE) instead."
  (span-notef (:unsafe (apply #'format-message args))))

(defun span-msg-now (&rest args)
  "Like `span-msg', but the entry reaches `span-log-handler' before returning.

`span-msg' only queues; the pending list is written on a 0.5s timer, and
`span--kill-emacs-hook' drains it on a normal exit.  Neither helps when
Emacs dies without running hooks -- a segfault, an external SIGKILL, or a
wedge in code that never yields to the timer.  This is the checkpoint
form for those cases: after it returns, the entry is in the sink.

It costs roughly 25x a plain `span-msg' (a formatting pass plus a real
write, against a list push), so use it to mark progress, not to log.

Inside `span-log-handler' itself this degrades to a plain `span-msg':
`span-flush-log' refuses to re-enter, and the entry goes out with the
next batch.  A failing handler cannot propagate out of here either -- see
`span--flush-log-impl' -- so a checkpoint never aborts the code it marks."
  (apply #'span-msg args)
  (span-flush-log))

(eval-and-compile
  (defun span--macro-backquote (arg)
    (list '\` arg))
  (defun span--macro-comma (arg)
    (list '\, arg))
  (defun span--format-dbg-args (args)
    (let ((first t))
      (mapcar
       (lambda (x)
         (if first
             (progn
               (setq first nil)
               (concat (span-fmt-to-string x) ": "))
           (concat "; " (span-fmt-to-string x) ": ")))
       args)))

  (defun span--handle-dbg-args (args)
    (let ((argss (span--format-dbg-args args))
          (forms (mapcar
                  (lambda (x) (span--macro-comma `(:ts ,x)))
                  args)))
      (span--macro-backquote
       `(span--format-dbg-rt
         ',argss
         ,@forms)))))

(defun span--format-dbg-rt (spec &rest args)
  (apply #'concat (cl-mapcar #'concat spec args)))


(defmacro span-dbg (&rest args)
  "Log each of ARGS as `EXPR: VALUE', deferring the note like `span-note'.
The expression text is captured at compile time, so there is no need to
repeat it in a format string.  Values are printed at flush time."
  `(span-note
     ,(span--handle-dbg-args args)))
(defmacro span-dbgf (&rest args)
  "As `span-dbg', but flush the span stack like `span-notef' so it always shows."
  `(span-notef
     ,(span--handle-dbg-args args)))

(defvar span-blocking-log-limit 0.05)
(defun span--toggle-blocking ()
  (if span--blocking-time
      (progn
        (let ((time (float-time (time-subtract (span--time) span--blocking-time))))
          (when (> time span-blocking-log-limit)
            ;; (span-fmt-parse '("blocking: %.3f" time))
            (span-notef "blocking: %.3f" time)))
        (setq span--blocking-time nil))
    (setq span--blocking-time (span--time))))

(defun span--unsafe-on-err (flush)
  (span-note :tag "!" "%s" (span-s<-tag (caar span--stack)))
  ;; (when (or flush (eq span--cur-context :redisplay))
  (when flush
    (span--unsafe-flush-stack)))

;; dynamic vars are faster in unwind protect
(defvar span--tmp-sucess nil)
(defvar span--tmp-switch-blocking nil)

(defmacro span--unsafe-with (obj &rest rest)
  (declare (indent 1))
  (let ((blocking t)
        (prevent-unwind-quit t)
        (track-sucess t)
        (track-blocking t)
        (flush-on-err t)
        (vars nil)
        (on-unwind nil)
        (body nil))
    (while (keywordp (car-safe rest))
      (pcase (pop rest)
        (:prevent-unwind-quit (setq prevent-unwind-quit (pop rest)))
        (:track-sucess (setq track-sucess (pop rest)))
        (:track-blocking (setq track-blocking (pop rest)))
        (:flush-on-err (setq flush-on-err (pop rest)))
        (:blocking (setq blocking (pop rest)))
        (k (error "span--unchecked: invalid option %S" k))))

    (cl-assert (symbolp track-sucess))

    (setq body rest)

    ;; do this first; if fails, nothing happens at all
    (push `(span--stack (cons ,obj span--stack)) vars)

    (when track-sucess
      (push `(span--tmp-sucess nil) vars)
      (push
       `(unless span--tmp-sucess
          (span--unsafe-on-err ,flush-on-err))
       on-unwind)
      (setq body
            `((prog1
                  ,(macroexp-progn body)
                (setq span--tmp-sucess t)))))

    (when track-blocking
      (push `(span--tmp-switch-blocking (xor span--blocking-time ,blocking)) vars)
      (push `(when span--tmp-switch-blocking (span--toggle-blocking)) on-unwind)
      (setq body
            `((when span--tmp-switch-blocking (span--toggle-blocking))
              ,@body)))

    (when (and prevent-unwind-quit on-unwind)
      (push `(inhibit-quit t) vars))

    (setq vars (nreverse vars))
    (setq on-unwind (nreverse on-unwind))

    (if on-unwind
        `(let ,vars
           (unwind-protect
               ,(macroexp-progn body)
             ,@on-unwind))
      `(let ,vars
         ,(macroexp-progn body)))))

(defmacro span--unchecked (obj &rest rest)
  (declare (indent 1))
  `(span--unsafe-with ,(span--parse-span-spec obj)
     ,@rest))

(defmacro span--with-macro (obj blocking flush-on-err &rest body)
  (declare (indent 3))
  `(let ((span--tmp-inhibit-quit-old inhibit-quit))
     (span--unsafe-with ,obj
       :blocking ,blocking
       :flush-on-err ,flush-on-err
       (let ((inhibit-quit span--tmp-inhibit-quit-old))
         ,@body))))

(defun span--with-cb (obj blocking flush-on-err cb)
  (span--with-macro obj blocking flush-on-err
    (funcall cb)))

(defmacro span (obj &rest rest)
  "Run the body in REST inside a span named by OBJ.

OBJ is a keyword, or a list whose head is a keyword followed by a span
format spec, as in (:my-tag \"x=%s\" (:unsafe-ts x)).

The span reaches the log only if something was logged inside it, so
wrapping a function that stays quiet costs a push and a pop and nothing
else.  A body that exits non-locally is marked with `!' in the log.

Keyword options may precede the body:

  :blocking      whether time spent in the body counts as the session
                 being stuck (default t).  A blocking stretch longer than
                 `span-blocking-log-limit' logs a `blocking:' note.  Pass
                 nil, or a condition, for a body that is expected to wait
                 without the user minding -- see
                 `span--wrap-accept-process-output'.
  :flush-on-err  on a non-local exit, also flush the enclosing span stack
                 so the failure is visible rather than buffered on a span
                 that may never be logged (default t)."
  (declare (indent 1))
  (let ((blocking t)
        (flush-on-err t))
    (while (keywordp (car-safe rest))
      (pcase (pop rest)
        (:blocking (setq blocking (pop rest)))
        (:flush-on-err (setq flush-on-err (pop rest)))
        (k (error "span: invalid option %S" k))))
    (if (bound-and-true-p byte-compile-current-file)
        (macroexp-let2* nil
            ((span--tmp-obj (span--parse-span-spec obj))
             (span--tmp-blocking blocking)
             (span--tmp-flush-on-err flush-on-err))
          `(span--with-macro
               ,span--tmp-obj
               ,span--tmp-blocking
               ,span--tmp-flush-on-err
             ,@rest))
      `(span--with-cb ,(span--parse-span-spec obj) ,blocking ,flush-on-err (lambda () ,@rest)))))


;; see span--log-note
(defvar span--pending-log-list nil)
(defvar span--pending-log-list-len 0)
(defvar span--n-backtrace-made-this-cycle 0)
(defvar span--n-debugger-rearmed-this-cycle 0)

(defvar span-message-limit-per-cycle 3000
  "Maximum entries recorded per flush cycle; the rest are dropped.
`span-flush-log' notes how many were dropped.  Bounds the damage when a
loop logs without limit, which would otherwise exhaust memory before the
next timer tick.")

(defvar span-debugger-rearm-limit-per-cycle 10
  "How often `span--debug' may re-arm the Emacs debugger per flush cycle.
Emacs disables the debugger after one entry until the next non-macro
input event.  Re-arming restores a backtrace for every error; the limit
keeps an error storm from spending the whole cycle in the debugger.")

(defmacro span--pending-log-list-push (entry)
  `(let ((l span--pending-log-list-len))
     (setq span--pending-log-list-len (1+ l))
     (when (< l span-message-limit-per-cycle)
       (push ,entry span--pending-log-list))))

(defun span--log-note (e depth)
  ;; e is one of
  ;; (span-s . time)
  ;; (span-s . (time . obj))

  ;; we need to push this to span--pending-log-list
  ;; (depth span-s time . obj)

  ;; notes are one extra indent from parent
  (cl-incf depth)
  (span--pending-log-list-push
   (if (consp (cadr e))
       ;; (span-s . (time . obj))
       (cons depth e)
     ;; (span-s . time)
     (list depth (car e) (cdr e)))))

;; span--stack
;; (span--unsafe-push :abc4)
;; (span--unsafe-flush-stack)
(defun span--unsafe-flush-stack ()
  (let ((cur span--stack) c ans)
    (while (not (span<-logged (span--unsafe-top-obj-normalized cur)))
      (push (car cur) ans)
      (setq cur (cdr cur)))
    (setq c (if cur (1+ (span<-depth (cdar cur))) 0))

    (setq cur ans)
    (while cur
      (let* ((x (car cur))
             (e (cdr x)))
        (setf (span<-logged e) t)
        (setf (span<-depth e) c)

        ;; x: (span-s . span)
        ;; need to make: (depth span-s time . obj)
        (span--pending-log-list-push `(,c ,(car x) ,(span<-time e) . ,(span<-obj e)))

        (mapc (lambda (v) (span--log-note v c)) (nreverse (span<-notes e)))
        (setf (span<-notes e) nil))
      (setq cur (cdr cur))
      (cl-incf c))))

(defun span-flush ()
  "Emit the enclosing spans' headers into the pending log list.

This makes the current span position visible, so notes recorded after it
are printed under the right span instead of being buffered on a span that
may never be logged.  It does NOT reach `span-log-handler'; that is
`span-flush-log'."
  (let ((inhibit-quit t))
    (span--unsafe-flush-stack)))

(defvar span-max-width 1000
  "Truncate every logged line to this many characters, or nil for no limit.
Applied per line after formatting, so a long value is cut with no marker.
Raise it before logging long values.")
(defun span--maybe-truncate-str (s)
  (declare (indent 0))
  (if (and span-max-width (length> s span-max-width))
      (substring s 0 span-max-width)
    s))

(defun span-format-one (e)
  "Format one pending log entry E into its printed line(s).

Errors from the entry's own format function are caught and replaced with
an `error (span-format-one)' line, so one bad entry cannot destroy the
batch.  Note this does NOT cover a value whose printer signals: `cl-prin1'
demotes that internally, so such a value renders as an empty string and
the reason surfaces separately as a `cl-prin1:' message."
  (cl-destructuring-bind (depth s time . obj) e
    (let* ((inhibit-redisplay t)
           (backtrace-on-redisplay-error nil)
           (msg
            ;; (condition-case-unless-debug err
            (condition-case err
                (funcall (span-s<-fmt-fn s) obj)
              (error
               (format
                "error (span-format-one): %s\n%s\n%s\n%s"
                (span-fmt-to-string err)
                (span-fmt-to-string (span-s<-fmt-fn s))
                (span-fmt-to-string obj)
                (current-buffer)))))
           (lines (split-string msg "\n"))
           (prefix
            (format
             "%.3f %s%s"
             (float-time (time-subtract time before-init-time))
             (make-string (* depth 2) (eval-when-compile (string-to-char " ")))
             (or (span-s<-tag s) "%")))

           (first-str (format "%s %s" prefix (car lines)))
           (rest-str (mapcar
                      (lambda (x)
                        (span--maybe-truncate-str
                          (format
                           "%s> %s"
                           (make-string (1- (length prefix)) (eval-when-compile (string-to-char " ")))
                           x)))
                      (cdr lines))))
      (concat (mapconcat #'span--maybe-truncate-str (cons first-str rest-str) "\n") "\n"))))


(defvar span--log-buf nil)
(defun span--get-or-create-log-buf ()
  (if (and span--log-buf (buffer-live-p span--log-buf))
      span--log-buf
    (setq span--log-buf (generate-new-buffer "*span*" t))
    (with-current-buffer span--log-buf
      (messages-buffer-mode)
      (buffer-disable-undo)
      ;; see backtrace.el
      (add-function :around (local 'cl-print-expand-ellipsis-function)
                    #'backtrace--expand-ellipsis))
    span--log-buf))

(defvar span-log-handler #'span-default-log-handler
  "Function called with one string per flush, to put the log somewhere.

MSG is an ordinary Emacs string and may carry text properties, raw 8-bit
bytes, and characters above #x10FFFF.  A handler has to cope with all of
it.  Encoding is the handler's business precisely because the sinks
disagree: `span-default-log-handler' inserts into a buffer and
`ci--redirect-to-stdout' calls `message', both of which want the string
unchanged, while only a file wants bytes -- see `span-file-log-handler'.

A handler runs from a timer, at an arbitrary point in unrelated code.  It
must not prompt, must not block, and should not depend on the current
buffer or `default-directory'.  If it signals, the batch it was given is
destroyed; see `span--flush-log-impl' for what happens then.")

(defun span-default-log-handler (msg)
  "Append MSG to the *span* buffer.
Cannot fail on content: a buffer holds anything a Lisp string can."
  (with-current-buffer (span--get-or-create-log-buf)
    (let ((buffer-read-only nil))
      (span-with-no-minibuffer-message
       (save-excursion
         (goto-char (point-max))
         (insert-before-markers msg))))))

(defun span--file-log-write (file msg append)
  "Write MSG to FILE, guarded so the write cannot re-enter Lisp or prompt.

FILE is an absolute file name, or a function of no arguments returning
one; a function is called here rather than by the caller so that even
that call, and the `expand-file-name' after it, happen under the guards.
APPEND is passed through to `write-region'.

Each binding closes off one way a log write can re-enter Lisp, block, or
prompt -- which matters because this runs from a timer, at arbitrary
points in unrelated code:

  `default-directory' and `file-name-handler-alist'
      `write-region' expands FILE against `default-directory', so a remote
      one routes the log write through Tramp.  With both bound, a write
      from a remote buffer performs no Tramp operations at all.
  `write-region-post-annotation-function'
      runs even though START is a string, because `write-region' seeds its
      annotation buffer list unconditionally.
  `create-lockfiles'
      `write-region' locks the file it writes, including a file it is not
      visiting.
  `coding-system-for-write' plus an explicit `encode-coding-string'
      encoding in Lisp cannot prompt.  Leaving the choice to `write-region'
      sends an unencodable character into `select-safe-coding-system',
      which asks the user -- and hangs a session that has no user.
      `utf-8-emacs-unix' encodes everything an Emacs string can hold.

The sixth argument of `write-region' is omitted deliberately: it is
LOCKNAME, not MUSTBENEW.  Passing a non-nil value there makes Emacs
derive a lock file name from it on every single write."
  (let ((default-directory "/")
        (file-name-handler-alist nil)
        (write-region-post-annotation-function nil)
        (create-lockfiles nil)
        (coding-system-for-write 'binary)
        (inhibit-interaction t))
    (write-region (encode-coding-string msg 'utf-8-emacs-unix)
                  nil
                  (if (functionp file) (expand-file-name (funcall file)) file)
                  append 'no-message)))

(defun span--file-log-target (file)
  "Resolve FILE for `span--file-log-write': expand a name, pass a function on."
  (if (functionp file)
      file
    (let ((file-name-handler-alist nil))
      (expand-file-name file))))

(defun span-file-log-handler (file)
  "Return a `span-log-handler' that appends the log to FILE.

FILE is a file name, or a function of no arguments returning one.  Pass a
function when the destination is chosen after the handler is installed --
the agent harness does this, so that a work section can still redirect the
log by setting its `log-file' variable.  FILE should be absolute; a
relative name is expanded against \"/\", with file-name handlers disabled
so that even that expansion cannot reach Tramp.

The write itself is `span--file-log-write', whose docstring explains what
each guard is for.  This handler only ever appends, so call
`span-file-log-reset' at setup if the log should hold one run."
  (let ((target (span--file-log-target file)))
    (lambda (msg)
      (span--file-log-write target msg t))))

(defun span-file-log-reset (file &optional header)
  "Empty FILE, then write HEADER and a newline if HEADER is non-nil.

FILE is a file name or a function of no arguments returning one, as for
`span-file-log-handler', and the write is guarded the same way.  Call this
at setup, before anything is logged.

`span-file-log-handler' appends, and nothing else truncates: without this
a second run leaves two runs in one file, both timestamp series starting
at zero, and a reader anchored on the first marker it finds silently gets
the older one.  HEADER exists so the first line can say which run this is
-- wall clock and pid, neither of which the relative timestamps carry."
  (span--file-log-write (span--file-log-target file)
                        (if header (concat header "\n") "")
                        nil))

(defvar span-log-handler-failure-limit 5
  "Consecutive `span-log-handler' failures tolerated before falling back.
Past this many, the handler is replaced by `span-default-log-handler' so
that the framework still has a sink that works.")

(defvar span--consecutive-log-handler-failures 0)

(defun span--last-resort (fmt &rest args)
  "Report FMT and ARGS on stderr, bypassing the log entirely.

This exists for failures of `span-log-handler' itself, which cannot be
reported through the log.  A batch handed to a handler is already off
`span--pending-log-list', so a handler that signals destroys it -- and any
note about the loss is destroyed the same way by the next failure, which
is why a sustained outage otherwise reports only its final cycle.

The payload is reduced to printable ASCII and written with `princ' to
`external-debugging-output'.  ASCII on that stream reaches stderr through
a bare putc per character: no file-name handlers, no coding-system
selection, no hooks.  Anything else would consult `coding-system-for-write'
and `standard-display-table'."
  (let* ((str (condition-case nil
                  (apply #'format fmt args)
                (error "unformattable report")))
         (ascii (mapconcat (lambda (c)
                             (char-to-string
                              (if (and (>= c 32) (< c 127)) c ??)))
                           str "")))
    (let ((standard-display-table nil))
      (princ (concat "span: " ascii "\n") 'external-debugging-output))))

(defun span--flush-log-impl (pending)
  "Format PENDING and hand it to `span-log-handler', containing any failure.

A handler that signals has already destroyed PENDING, so re-raising would
only route the report into the channel that just failed -- and into a
timer, where it becomes a `%% Error running timer' line in that same dead
log.  The failure is therefore reported through `span--last-resort'
instead, and not propagated: a flush must not abort whatever code
`span-msg-now' was called from.  The in-band note is kept as well, since
it survives and usefully marks the hole whenever the sink recovers."
  (with-temp-buffer ;; prevent accidental interference with current buffer
    (let ((msg (mapconcat #'span-format-one pending "")))
      (let ((span--handles-message nil)
            (debug-on-message nil))
        (span :span-log-handler
          (condition-case err
              (progn
                (funcall span-log-handler msg)
                (setq span--consecutive-log-handler-failures 0))
            (error
             (cl-incf span--consecutive-log-handler-failures)
             (span--last-resort
              "log handler failed (%d consecutive), %d entries lost: %S"
              span--consecutive-log-handler-failures (length pending) err)
             (span-notef "warning: log handler failed, %s entries lost: %s"
               (length pending) (span-fmt-to-string err))
             (when (and (>= span--consecutive-log-handler-failures
                            span-log-handler-failure-limit)
                        (not (eq span-log-handler #'span-default-log-handler)))
               (setq span-log-handler #'span-default-log-handler)
               (span--last-resort
                "log handler replaced by span-default-log-handler after %d failures"
                span--consecutive-log-handler-failures)))))))))

(defvar span--is-flushing nil)

(setq-default debugger #'span--debug)
(setq-default non-essential nil)

(defmacro span--always-debug (&rest form)
  (let ((err-sym (make-symbol "err")))
    `(condition-case-unless-debug ,err-sym
         (progn ,@form)
       (t
        (let ((inhibit-debugger t))
          (signal (car ,err-sym) (cdr ,err-sym)))))))

(defmacro span--context (context &rest body)
  (declare (indent 1))
  (cl-assert (keywordp context))
  (let* ((is-redisp (eq context :redisplay))
         ;; (inhibit-debugger is-redisp)
         ;; (do-debug (not inhibit-debugger))
         )
    `(let* ((span--cur-context ,context)

            (debugger ,(if is-redisp '#'span--debug '#'debug))
            (non-essential ,is-redisp)
            ;; (signal-hook-function ,(if is-redisp '#'span--signal-hook-function nil))
            ;; ,@(when is-redisp '((signal-hook-function nil)))
            (signal-hook-function nil)
            ;; (inhibit-debugger nil)
            (debug-on-error t)
            (debug-on-quit t)

            (throw-on-input nil)
            (inhibit-message nil)
            (message-log-max 1000)
            (set-message-function #'set-message-functions)
            (clear-message-function #'clear-minibuffer-message)

            (delay-mode-hooks nil)
            ;; (delayed-mode-hooks nil)

            (span--context-locals nil)
            (span--handles-message t))

       ,@(if is-redisp '((setq internal-when-entered-debugger -1)) nil)

       (span--always-debug
        ,@body))))

(defun span-flush-log ()
  "Hand everything pending to `span-log-handler'.

This is the step that actually reaches a buffer, stdout or a file, as
opposed to `span-flush', which only positions entries within their spans.
Normally driven by a 0.5s timer and by `span--kill-emacs-hook'; call it
directly, or use `span-msg-now', when an entry must land immediately.

Re-entrant calls are refused, so a `span-msg-now' from inside a log
handler queues instead of recursing."
  (when (and span--pending-log-list (not span--is-flushing))
    (span :span--flush-log
      (let ((inhibit-quit t)
            (prev-len span--pending-log-list-len)
            (span--is-flushing t)
            (pending (nreverse span--pending-log-list)))
        (setq span--pending-log-list nil)
        (setq span--pending-log-list-len 0)
        (setq span--n-backtrace-made-this-cycle 0)
        (setq span--n-debugger-rearmed-this-cycle 0)
        (when (> prev-len span-message-limit-per-cycle)
          (span-notef
            "warning: %s has been ommited due to too many messages"
            (- prev-len span-message-limit-per-cycle)))
        (let (inhibit-quit)
          (span--flush-log-impl pending))))))

(defun span--flush-log-timer-fn ()
  (unwind-protect
      (span-flush-log)
    (run-with-timer 0.5 nil #'span--flush-log-timer-fn)))

(defvar span-log-timer nil)

(unless span-log-timer
  (setq span-log-timer t)
  (run-with-timer 0.5 nil #'span--flush-log-timer-fn))

(defun span--kill-emacs-hook ()
  (span-notef :span--kill-emacs-hook)
  ;; kill emacs hook should not hang or throw
  (with-demoted-errors "error in span-flush-log during kill-emacs-hook: %S"
    (let ((inhibit-interaction t))
      (with-timeout (1) (span-flush-log)))))

(add-hook 'kill-emacs-hook #'span--kill-emacs-hook 100)

(defmacro span-wrap (sym &optional arglist &rest rest)
  "Advise function SYM so that each call runs inside a span.

Defines `span--wrap-SYM' and installs it as :around advice.  With no
ARGLIST the span is tagged :SYM and the arguments are not logged; with an
ARGLIST the body in REST runs inside the span, wrapping a call to the
original.  A leading `_' in REST is replaced by the :SYM keyword.

:with FN uses FN instead of `span' as the wrapper macro, which is how
`span-wrap-redisplay' installs a different context.

This is for permanently instrumenting a known function in this file.  For
ad-hoc tracing of an arbitrary function, use `span-instrument'."
  (declare (indent 2))
  (cl-assert (symbolp sym))
  (let* ((adv-sym (intern (concat "span--wrap-" (symbol-name sym))))
         (wrap-with 'span)
         (_ (while (keywordp (car-safe rest))
              (pcase (pop rest)
                (:with (setq wrap-with (pop rest)))
                (k (error "span-wrap: invalid option %S" k)))))
         (_ (when (eq (car-safe (car-safe rest)) '_)
              (setcar (car rest) (intern (concat ":" (symbol-name sym))))))
         (defun-form
          (cond
           (rest
            `(defun ,adv-sym (span--tmp-orig-fun ,@arglist)
               (,wrap-with
                ,@rest
                (,(if (memq '&rest arglist) 'apply 'funcall)
                 span--tmp-orig-fun ,@(remove '&rest (remove '&optional arglist))))))
           (arglist
            `(defun ,adv-sym (span--tmp-orig-fun &rest span--tmp-body)
               (,wrap-with
                ,arglist
                (apply span--tmp-orig-fun span--tmp-body))))
           (t
            `(defun ,adv-sym (span--tmp-orig-fun &rest span--tmp-body)
               (,wrap-with
                ,(intern (concat ":" (symbol-name sym)))
                (apply span--tmp-orig-fun span--tmp-body)))))))
    `(progn
       ,defun-form
       (advice-add #',sym :around #',adv-sym))))

(defmacro span-quickwrap (sym)
  "Advise SYM with a span that logs its arguments.
Shorthand for the common `span-wrap' case."
  `(span-wrap ,sym (&rest args)
     (_ (:seq args))))


(defun span--instrument-with (sym)
  (lambda (fn &rest args)
    (let* ((verbose (get sym 'span--instrument-verbose))
           (backtrace (get sym 'span--instrument-backtrace))
           (callback (get sym 'span--instrument-callback))
           (time (get sym 'span--instrument-time))
           (buffer (current-buffer))
           (msg
            (if verbose
                (span-fmt-to-string (cons sym args))
              (span-fmt `(span-fmt-to-string ,(:seq (cons sym args)))))))
      (span (:: (:unsafe msg))
        (span-msg "buf: %s" buffer)
        (when backtrace
          (span--backtrace))
        (when callback
          (funcall callback))
        ;; (span-msg "args: %s" args)
        (let* ((start-time (span--time))
               (res
                (unwind-protect (apply fn args)
                  (when time
                    (span-notef "took: %.3f" (float-time (time-subtract (span--time) start-time))))
                  (unless (eq (current-buffer) buffer)
                    (span-msg "buf (changed): %s" (current-buffer))))))
          (if verbose
              (span-msg "%s -> %s" sym (span-fmt-to-string res))
            (span-notef "%s -> %S" sym res))
          res)))))

(defun span-add-instrument (sym verbose backtrace time callback)
  "Install tracing advice on SYM.  See `span-instrument' for the options."
  (setf (get sym 'span--instrument-verbose) verbose)
  (setf (get sym 'span--instrument-backtrace) backtrace)
  (setf (get sym 'span--instrument-callback) callback)
  (setf (get sym 'span--instrument-time) time)
  (advice-add sym :around (span--instrument-with sym)))

(defmacro span-instrument (sym &rest rest)
  "Trace calls to SYM: log its arguments, its return value, and REST.

Ad-hoc counterpart to `span-wrap', meant to be evaluated interactively
while investigating.  Remove it with `span-uninstrument'.

  :verbose    print arguments and result in full, at the call site rather
              than at flush time.  Costs more and can abort the traced
              call if a value's printer signals; without it both are
              printed at flush time.
  :backtrace  log a backtrace at every call.
  :time       log how long each call took.

REST runs inside the span on entry, so it can log extra context."
  (declare (indent 1))
  (cl-assert (symbolp sym))
  (let ((verbose nil)
        (backtrace nil)
        (time nil))
    (while (keywordp (car-safe rest))
      (pcase (pop rest)
        (:verbose (setq verbose (pop rest)))
        (:backtrace (setq backtrace (pop rest)))
        (:time (setq time (pop rest)))
        (k (error "span-instrument: invalid option %S" k))))
    `(span-add-instrument #',sym ,verbose ,backtrace ,time (lambda () ,@rest))))

(defun span-uninstrument (sym)
  "Remove the tracing advice `span-instrument' installed on SYM."
  (advice-remove sym (span--instrument-with sym)))

(advice-add #'message :around #'span--wrap-message)
(defun span--wrap-message (orig-fun format-string &rest args)
  (if span--handles-message
      (if (and format-string (not (string-empty-p format-string)))
          (let ((msg (apply #'format format-string args)))
            (when message-log-max
              (span-notef :tag "%%" "%s" msg))
            ;; (unless (and (eq clear-message-function #'span--dont-clear-message)
            ;;              (eq set-message-function #'always))
            (let (message-log-max)
              (funcall orig-fun "%s" msg)))
        (funcall orig-fun nil))
    (apply orig-fun format-string args)))

(defvar span--in-message-functions 0)
(advice-add #'set-message-functions :around #'span--wrap-set-message-functions)
(defun span--wrap-set-message-functions (orig-fun message)
  (span :set-message-functions
    (span-dbg set-message-functions)
    ;; recursive invocations is legal, especially for garbage-collection-messages
    ;; we put them in *span* instead
    (if (>= span--in-message-functions 1)
        (progn
          (span-notef "recursive call to set-message-functions: %s" message)
          'already-handled)
      (let ((span--in-message-functions (1+ span--in-message-functions)))
        (funcall orig-fun message)))))

(advice-add #'clear-minibuffer-message :around #'span--wrap-clear-minibuffer-message)
(defun span--wrap-clear-minibuffer-message (orig-fun)
  (span :clear-minibuffer-message
    (if (>= span--in-message-functions 2)
        (progn
          (span-dbgf set-message-functions)
          (span-notef "recursive call to clear-minibuffer-message")
          'dont-clear-message)
      (let ((span--in-message-functions (1+ span--in-message-functions)))
        (funcall orig-fun)))))



(advice-add #'timer-event-handler :around #'span--wrap-timer-event-handler)
(defun span--wrap-timer-event-handler (orig-fun timer)
  (span--context :timer
    (span--unchecked (:timer (:unsafe-ts (timer--function timer)))
      (funcall orig-fun timer))))


(advice-add #'command-execute :around #'span--wrap-command-execute)
(defun span--wrap-command-execute (orig-fun cmd &rest args)
  (span--context :command
    (span--unchecked (:command-execute "%s(%s)" (:unsafe-ts cmd) (buffer-name (current-buffer)))
      ;; :flush-on-err nil
      (let (inhibit-quit)
        (apply orig-fun cmd args)))))

(defmacro span-wrap-redisplay (obj &rest rest)
  (declare (indent 1))
  (let ((keywords nil))
    (while (keywordp (car-safe rest))
      (push (pop rest) keywords)
      (push (pop rest) keywords))

    `(span--unchecked ,obj
       :blocking nil
       :flush-on-err nil
       ,@(nreverse keywords)
       (let (inhibit-quit)
         (span--context :redisplay
           ;; (span-dbgf "context: redisplay")
           ,@rest)))))

(defun span--wrap-recursive-edit (orig-fun)
  (span-wrap-redisplay :recursive-edit
    (funcall orig-fun)))
(advice-add #'recursive-edit :around #'span--wrap-recursive-edit)

(advice-add #'read-from-minibuffer :around #'span--wrap-read-from-minibuffer)
(advice-add #'read-string :around #'span--wrap-read-from-minibuffer)
(defun span--wrap-read-from-minibuffer (orig-fun &rest args)
  (span-wrap-redisplay (:read-from-minibuffer (car args))
    (apply orig-fun args)))

(advice-add #'read-key-sequence :around #'span--wrap-read-key-sequence)
(advice-add #'read-key-sequence-vector :around #'span--wrap-read-key-sequence)
(defun span--wrap-read-key-sequence (orig-fun &rest args)
  (span-wrap-redisplay (:read-key-sequence (car args))
    (apply orig-fun args)))

(advice-add #'read-event :around #'span--wrap-read-event)
(defun span--wrap-read-event (orig-fun &rest args)
  (span-wrap-redisplay (:read-event (:seq args))
    (apply orig-fun args)))

(advice-add #'x-popup-menu :around #'span--wrap-x-popup-menu-span)
(defun span--wrap-x-popup-menu-span (orig-fun &rest args)
  (span-wrap-redisplay :x-popup-menu-span
    (apply orig-fun args)))

;; (span-wrap yes-or-no-p (&rest args)
;;   :with span-wrap-redisplay
;;   (_ (:seq args)))

;; (span-wrap y-or-n-p (&rest args)
;;   :with span-wrap-redisplay
;;   (_ (:seq args)))

(span-wrap sit-for (&rest args)
  :with span-wrap-redisplay
  (:sit-for (:seq args)))

(span-wrap sleep-for (&rest args)
  :with span-wrap-redisplay
  (:sleep-for (:seq args)))


(defvar span--require-buf " *span-require*")

(defun span--wrap-require (orig-fun feature &rest args)
  (span--context :require
    (span--unchecked (:require feature)
      (with-current-buffer (get-buffer-create span--require-buf t)
        (cl-letf (((span-var 'current-require-or-load) feature))
          (let (inhibit-quit)
            (span-with-no-minibuffer-message
             (apply orig-fun feature args))))))))
(advice-add #'require :around #'span--wrap-require `((depth . 50)))

(defun span--wrap-load (orig-fun file &rest args)
  (span--context :require
    (span--unchecked (:load file)
      (with-current-buffer (get-buffer-create span--require-buf t)
        (cl-letf (((span-var 'current-require-or-load) file))
          (let (inhibit-quit)
            (apply orig-fun file args)))))))
(advice-add #'load :around #'span--wrap-load `((depth . 50)))


(defun span--before-provide (&rest args)
  (span-notef "provide %S" (:seq args)))

(advice-add #'provide :before #'span--before-provide)


(cl-pushnew 'quit-nodebug debug-ignored-errors)
(setf (get 'quit-nodebug 'error-conditions) '(quit-nodebug))
(setf (get 'quit-nodebug 'error-message) "quit")

(defun span--wrap-accept-process-output (orig-fn &optional process seconds millisec just-this-one)
  (let ((inhibit-quit-old inhibit-quit))
    (span--unchecked (:accept-process-output (:unsafe-ts process))
      :blocking (or inhibit-quit-old (not non-essential))

      (span-note
        "seconds:%s millisec:%s just-this-one:%s"
        seconds millisec just-this-one)

      (when (and (not inhibit-quit-old) non-essential (input-pending-p))
        (span-flush)
        ;; (span--backtrace)
        (if throw-on-input
            (let (inhibit-quit)
              (setq quit-flag throw-on-input)
              (eval '(ignore nil) t))
          (signal 'quit-nodebug nil)))

      (when inhibit-quit-old
        (span-flush))

      ;; TODO: if seconds is nonzero, we should use throw-on-input
      (let ((inhibit-quit inhibit-quit-old))
        (funcall orig-fn process seconds millisec just-this-one)))))

(advice-add #'accept-process-output :around #'span--wrap-accept-process-output)


(advice-add #'redisplay :around #'span--wrap-redisplay)
(defun span--wrap-redisplay (orig-fn &optional force)
  (span-wrap-redisplay (:explicit-redisplay force)
    :flush-on-err t
    :blocking t
    (funcall orig-fn force)))

(advice-add #'redisplay--pre-redisplay-functions :around #'span--wrap-redisplay--pre-redisplay-functions)
(defun span--wrap-redisplay--pre-redisplay-functions (orig-fn windows)
  (span-wrap-redisplay (:pre-redisplay-functions (:unsafe-ts windows))
    :flush-on-err t
    :blocking t
    (let ((pre-redisplay-function #'ignore))
      (funcall orig-fn windows))))

(advice-add #'debug :around #'span--wrap-debug)
(defun span--wrap-debug (orig-fn &optional type &rest args)
  (span :debug
    (if (eq type 'error)
        (let* ((signal-args (car args))
               (err-sym (car-safe signal-args))
               (data (cdr-safe signal-args)))
          (span-notef "error: %s" (:unsafe-ts (cons err-sym data))))
      (span-notef "debug: %s %s" (:unsafe-ts type) (:unsafe-ts args)))
    (if (or noninteractive
            (and (eq t (framep (selected-frame)))
                 (equal "initial_terminal" (terminal-name))))
        ;; #'debug will (kill-emacs -1), preventing caller from catching the error
        (apply #'span--debug type args)
      (apply orig-fn type args))))

(defun span--get-frames (base)
  "Collect backtrace frames using `mapbacktrace' directly.
Returns a list of (EVALD FUN ARGS FLAGS), avoiding the autoload
of `backtrace.el' that `backtrace-get-frames' would trigger."
  (let ((frames nil))
    ;; mapbacktrace have no additional lisp callbacks
    (mapbacktrace (lambda (evald fun args flags)
                    (push (list evald fun args flags) frames))
                  (or base 'span--get-frames))
    (nreverse frames)))

(defun span--backtrace--to-string (frames)
  (let* ((time-start (span--time))
         (backtrace-line-length 100)
         (max-redisplay-ticks (* 10 max-redisplay-ticks))
         ;; convert (evald fun args flags) lists to backtrace-frame records
         (bt-frames (mapcar (lambda (f)
                              (backtrace-make-frame
                               :evald (nth 0 f) :fun (nth 1 f)
                               :args (nth 2 f) :flags (nth 3 f)))
                            frames))
         (bt-fmt (backtrace--to-string bt-frames)))
    (format "backtrace(%.3fs):\n%s"
            (float-time (time-subtract (span--time) time-start))
            bt-fmt)))

(defvar span--is-in-backtrace nil)
(defun span--backtrace (&optional base eager)
  (span--context :span--internal
    (span-flush)
    (if span--is-in-backtrace
        (span :span--backtrace
          (span-notef "!! recursive invocation of span--backtrace"))
      (let ((span--is-in-backtrace t)
            (inhibit-quit nil))
        (cl-incf span--n-backtrace-made-this-cycle)
        (if (> span--n-backtrace-made-this-cycle 10)
            ;; perhaps its fine if we collect many backtraces if we dont flush them?
            (span-notef "too many backtraces; ommiting")
          (if eager
              (span-notef
                (:unsafe
                 (span--backtrace--to-string
                  (cdr (span--get-frames base)))))
            (span-notef
              `(span--backtrace--to-string
                ,(:unsafe (cdr (span--get-frames base)))))))))))

(defun span--debug (type &rest args)
  ;; Noninteractive debugger. Re-raises immediately; defers backtrace formatting on a timer.
  ;; Does NOT:
  ;;  - call any hooks or callbacks
  ;;  - create temporary buffers
  ;;  - autoload backtrace.el (deferred to timer)
  ;;  - invoke span-log-handler (deferred to timer)
  (let (
        ;; (2026/3/11)
        ;; observed segfault in message3_nolog here: `FRAME_MINIBUF_WINDOW(SELECTED_FRAME())` turns out nil in gc somehow
        ;; message have other side effects so we disable
        (garbage-collection-messages nil)
        (inhibit-debugger t))
    (if (eq type 'error)
        (let* ((signal-args (car args))
               (err-sym (car-safe signal-args))
               (data (cdr-safe signal-args)))
          (span (:span--debug "error: %s %s" (:unsafe-ts (cons err-sym data)) (:unsafe-ts (buffer-name (current-buffer))))
            (span-flush)
            (span--backtrace #'span--debug)
            ;; Entering the debugger disables it until the next non-macro input
            ;; event.  A session that takes no keyboard input -- an agent run, a
            ;; batch job, a burst of errors inside one command -- therefore gets
            ;; a backtrace for the first error and nothing for the rest.
            ;; Re-arming is budgeted per flush cycle so a storm cannot spend the
            ;; whole cycle building backtraces.
            (cond
             ((< span--n-debugger-rearmed-this-cycle
                 span-debugger-rearm-limit-per-cycle)
              (cl-incf span--n-debugger-rearmed-this-cycle)
              (setq internal-when-entered-debugger -1))
             ;; One warning as the budget runs out, so a log that goes quiet
             ;; reads as a spent budget rather than as no further errors.
             ((= span--n-debugger-rearmed-this-cycle
                 span-debugger-rearm-limit-per-cycle)
              (cl-incf span--n-debugger-rearmed-this-cycle)
              (span-notef
                "warning: debugger re-armed %s times this cycle; errors until the next flush carry no backtrace"
                span-debugger-rearm-limit-per-cycle)))
            (signal err-sym data)))
      (span-notef "debug: %s %s" (:unsafe-ts type) (:unsafe-ts args))
      ;; TODO: should quit here if is here too many times, since might hang
      )
    nil))

(defun span--signal-hook-function (error-symbol data)
  (let ((signal-hook-function nil))
    (span (:span--signal-hook-function "%s %s" error-symbol (:ts data))
      (span-flush)
      (span--backtrace #'span--signal-hook-function)
      (signal error-symbol data))))

(advice-add #'tramp-file-name-handler :around #'span--wrap-tramp-file-name-handler)
(defun span--wrap-tramp-file-name-handler (orig-fn &rest args)
  (span (:tramp-file-name-handler "%S %S" (buffer-name (current-buffer)) (:seq args))
    (span-dbg
     inhibit-quit
     throw-on-input
     non-essential
     )
    ;; (span-with-no-minibuffer-message
    (apply orig-fn args)))


(advice-add #'command-error-default-function :around #'span--wrap-command-error-default-function)
(defun span--wrap-command-error-default-function (orig-fun data context caller)
  ;; command-error-default-function writes directly to *Messages* through a c function
  ;; see minibuffer-error-function
  ;; (let ((string (error-message-string data)))
  ;;   (span-msg "%s%s" (if caller (format "%s: " caller) "") string))
  (span :command-error-default-function
    (funcall orig-fun data context caller)))

;; this dont always catch, sometimes Fkill_emacs called from c
(span-wrap kill-emacs (&rest args)
  (_ (:seq args))
  (span-flush)
  ;; log flused in #'span--kill-emacs-hook
  )

(provide 'span)
