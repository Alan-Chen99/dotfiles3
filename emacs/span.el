;; -*- lexical-binding: t -*-

(require 'span-fmt)
;; (cl-declaim (optimize (safety 0) (speed 3)))

(defmacro span-fmt (&rest body)
  (cl-destructuring-bind (fn . val) (span-fmt-parse body)
    `(funcall ,fn ,val)))

;; allowed entries in "span--stack":
;; (span-s . time)
;; (span-s . (time . obj))
;; (span-s . span) ; "normalized"

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


(defvar span--stack (list
                     (cons (eval-when-compile (span-s<-create :tag :dummy))
                           (span<-create :depth -1 :logged t))))

(defvar span--cur-context :redisplay)
;; :redisplay :command :timer :require

(defvar span--context-locals nil)
(defvar span--blocking-time nil)

(defun span-var (sym)
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
  (declare (indent 0))
  (let (tag)
    (when (eq (car-safe args) :tag)
      (pop args)
      (setq tag (pop args)))
    `(span--note ,(span--parse-fmt-spec args tag))))

(defmacro span-notef (&rest args)
  (declare (indent 0))
  (let (tag)
    (when (eq (car-safe args) :tag)
      (pop args)
      (setq tag (pop args)))
    `(span--note-and-flush ,(span--parse-fmt-spec args tag))))

(defmacro span-msg (&rest args)
  `(span-notef (:unsafe (format-message ,@args))))

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
               (concat (cl-prin1-to-string x) ": "))
           (concat "; " (cl-prin1-to-string x) ": ")))
       args)))

  (defun span--handle-dbg-args (args)
    (let ((argss (span--format-dbg-args args))
          (forms (mapcar
                  (lambda (x) (span--macro-comma `(:unsafe (cl-prin1-to-string ,x))))
                  args)))
      (span--macro-backquote
       `(span--format-dbg-rt
         ',argss
         ,@forms)))))

(defun span--format-dbg-rt (spec &rest args)
  (apply #'concat (cl-mapcar #'concat spec args)))


(defmacro span-dbg (&rest args)
  `(span-note
     ,(span--handle-dbg-args args)))
(defmacro span-dbgf (&rest args)
  `(span-notef
     ,(span--handle-dbg-args args)))

(defun span--toggle-blocking ()
  (if span--blocking-time
      (progn
        (let ((time (float-time (time-subtract (span--time) span--blocking-time))))
          (when (> time 0.1)
            ;; (span-fmt-parse '("blocking: %.3f" time))
            (span-notef "blocking: %.3f" time)))
        (setq span--blocking-time nil))
    (setq span--blocking-time (span--time))))

(defun span--unsafe-on-err (flush)
  (span-note :tag "!" "%s" (span-s<-tag (caar span--stack)))
  (when (or flush (eq span--cur-context :redisplay))
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
        (_ (error "invalid"))))

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
  (declare (indent 1))
  (let ((blocking t)
        (flush-on-err t))
    (while (keywordp (car-safe rest))
      (pcase (pop rest)
        (:blocking (setq blocking (pop rest)))
        (:flush-on-err (setq flush-on-err (pop rest)))))
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


(defvar span--pending-log-list nil)
(defvar span--pending-log-list-len 0)
(defvar span--n-backtrace-made-this-cycle 0)

(defmacro span--pending-log-list-push (entry)
  `(let ((l span--pending-log-list-len))
     (setq span--pending-log-list-len (1+ l))
     (when (< l 1000)
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
  (let ((inhibit-quit t))
    (span--unsafe-flush-stack)))

(defun span-format-one (e)
  (condition-case-unless-debug err
      (cl-destructuring-bind (depth s time . obj) e
        (format-message
         "%.3f %s%s %s\n"
         (float-time (time-subtract time before-init-time))
         (make-string (* depth 2) (string-to-char " "))
         (or (span-s<-tag s) "%")
         (funcall (span-s<-fmt-fn s) obj)))
    (error
     (format-message "error (span-format-one): %S" err))))

(defvar span--log-buf nil)
(defun span--get-or-create-log-buf ()
  (if (and span--log-buf (buffer-live-p span--log-buf))
      span--log-buf
    (setq span--log-buf (generate-new-buffer "*span*" t))
    (with-current-buffer span--log-buf
      (messages-buffer-mode)
      (buffer-disable-undo))
    span--log-buf))

(defun span--flush-log-impl (pending)
  (let* ((msg (mapconcat #'span-format-one pending "")))
    (with-current-buffer (span--get-or-create-log-buf)
      (let ((buffer-read-only nil)
            (span--handles-message nil)
            (debug-on-message nil))
        (span-with-no-minibuffer-message
         (save-excursion
           (goto-char (point-max))
           (insert-before-markers msg)))))))

(defvar span--is-flushing nil)

(setq-default debugger #'span--debug)
(setq-default non-essential nil)

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
            ,@(when is-redisp '((signal-hook-function nil)))
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
       ,@body)))

(defun span--flush-log ()
  (unwind-protect
      (when (and span--pending-log-list (not span--is-flushing))
        (span :span--flush-log
          (let ((prev-len span--pending-log-list-len)
                (span--is-flushing t)
                (pending (nreverse span--pending-log-list)))
            (setq span--pending-log-list nil)
            (setq span--pending-log-list-len 0)
            (setq span--n-backtrace-made-this-cycle 0)
            (when (> prev-len 1000)
              (span-notef
                "warning: %s has been ommited due to too many messages"
                (- prev-len 1000)))
            (let (inhibit-quit)
              (span--flush-log-impl pending)))))
    (run-with-timer 0.5 nil #'span--flush-log)))

(defvar span-log-timer nil)

(unless span-log-timer
  (setq span-log-timer t)
  (run-with-timer 0.5 nil #'span--flush-log))

(defmacro span-wrap (sym &optional arglist &rest rest)
  (declare (indent 2))
  (cl-assert (symbolp sym))
  (let* ((adv-sym (intern (concat "span--wrap-" (symbol-name sym))))
         (wrap-with 'span)
         (_ (while (keywordp (car-safe rest))
              (pcase (pop rest)
                (:with (setq wrap-with (pop rest))))))
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
  `(span-wrap ,sym (&rest args)
     (_ (:seq args))))

(defun span--format-instrument (sym args)
  (cl-prin1-to-string (cons sym args)))

(defun span--instrument-with (sym)
  (lambda (fn &rest args)
    (span (:: `(span--format-instrument ,sym ,(:seq args)))
      (span-msg "buf: %s" (current-buffer))
      (apply fn args))))

(defun span-add-instrument (sym)
  (advice-add sym :around (span--instrument-with sym)))

(defmacro span-instrument (sym)
  (cl-assert (symbolp sym))
  `(span-add-instrument #',sym))

(advice-add #'message :around #'span--wrap-message)
(defun span--wrap-message (orig-fun format-string &rest args)
  (if span--handles-message
      (if (and format-string (not (string-empty-p format-string)))
          (let ((msg (apply #'format-message format-string args)))
            (when message-log-max
              (span-notef :tag "%%" "%s" msg))
            ;; (unless (and (eq clear-message-function #'span--dont-clear-message)
            ;;              (eq set-message-function #'always))
            (let (message-log-max)
              (funcall orig-fun "%s" msg)))
        (funcall orig-fun nil))
    (apply orig-fun format-string args)))



(advice-add #'timer-event-handler :around #'span--wrap-timer-event-handler)
(defun span--wrap-timer-event-handler (orig-fun timer)
  (span--context :timer
    (span--unchecked (:timer `(cl-prin1-to-string ,(:unsafe (timer--function timer))))
      (funcall orig-fun timer))))


(advice-add #'command-execute :around #'span--wrap-command-execute)
(defun span--wrap-command-execute (orig-fun cmd &rest args)
  (span--context :command
    (span--unchecked (:command-execute "%s(%s)" `(cl-prin1-to-string ,(:unsafe cmd)) (buffer-name (current-buffer)))
      :flush-on-err nil
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


(defvar span--require-buf (get-buffer-create " *span-require*" t))

(defun span--wrap-require (orig-fun feature &rest args)
  (span--context :require
    (span--unchecked (:require feature)
      (with-current-buffer span--require-buf
        (cl-letf (((span-var 'current-require-or-load) feature))
          (let (inhibit-quit)
            (span-with-no-minibuffer-message
             (apply orig-fun feature args))))))))
(advice-add #'require :around #'span--wrap-require `((depth . 50)))

(defun span--wrap-load (orig-fun file &rest args)
  (span--context :require
    (span--unchecked (:load file)
      (with-current-buffer span--require-buf
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
    (span--unchecked (:accept-process-output (:unsafe process))
      :blocking (or inhibit-quit-old (not non-essential))

      (span-note
        "seconds:%S millisec:%S just-this-one:%S"
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
  (span-wrap-redisplay (:pre-redisplay-functions (:unsafe windows))
    :flush-on-err t
    :blocking t
    (let ((pre-redisplay-function #'ignore))
      (funcall orig-fn windows))))


(span-wrap debug (&optional type &rest args)
  (:debug (:seq args))
  (if (eq type 'error)
      (let* ((signal-args (car args))
             (err-sym (car-safe signal-args))
             (data (cdr-safe signal-args)))
        (span-notef "error: %S" `(list ,(:unsafe err-sym) ,(:unsafe data))))
    (span-notef "debug: %S %S" type (:unsafe args))))


(autoload 'backtrace-to-string "backtrace")
(autoload 'backtrace-get-frames "backtrace")

(defvar span--is-in-backtrace nil)
(defun span--backtrace (&optional base)
  (span--context :span--internal
    (span :span--backtrace
      (span-flush)
      (unless span--is-in-backtrace
        (let ((span--is-in-backtrace t)
              (inhibit-quit nil))
          (cl-incf span--n-backtrace-made-this-cycle)
          (when (< span--n-backtrace-made-this-cycle 10)
            (span-notef
              "backtrace:\n%s"
              `(backtrace-to-string
                ,(:unsafe (cdr (backtrace-get-frames base)))))))))))

(defun span--debug (type &rest args)
  (if (eq type 'error)
      (let* ((signal-args (car args))
             (err-sym (car-safe signal-args))
             (data (cdr-safe signal-args)))
        (span (:span--debug "error: %S" `(list ,(:unsafe err-sym) ,(:unsafe data)))
          (span--backtrace)
          (let ((inhibit-debugger t))
            (signal err-sym data))))
    (span-notef "debug: %S %S" type (:unsafe args))
    ;; TODO: should quit here if is here too many times, since might hang
    )
  nil)


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


(provide 'span)
