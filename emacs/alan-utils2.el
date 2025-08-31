;; -*- lexical-binding: t -*-
;; utils loaded after elpaca, so can use dependencies

(require 'dash)
(require 'general)

(require 'alan-utils)
(require 'alan-elpaca)

(defvar alan-dotemacs-dir nil)

;; (string-prefix-p "evil-collection-" (symbol-name 'evil-collection-rg))

(declare-function evil-collection-require "evil-collection")

(eval-and-compile
  (defun require-if-is-bytecompile--one (feature)
    (when (bound-and-true-p byte-compile-current-file)
      (when (string-prefix-p "evil-collection-" (symbol-name feature))
        (require 'evil-collection)
        (evil-collection-require (intern (string-remove-prefix "evil-collection-" (symbol-name feature)))))
      (require feature))
    nil))

(defmacro require-if-is-bytecompile (&rest features)
  (dolist (feature features)
    (if (consp feature)
        (when (eval (cdr feature))
          (require-if-is-bytecompile--one (car feature)))
      (require-if-is-bytecompile--one feature)))
  nil)

(defun alan-wrap-callback (fn)
  (let ((buf (current-buffer)))
    (lambda (&rest args)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (apply fn args))))))


(defmacro callback-lambda (arglist &rest body)
  (declare (indent defun))
  `(alan-wrap-callback (lambda ,arglist ,@body)))

(defun alan-lazy-call (feature fn args)
  (if (featurep feature)
      (apply fn args)
    (let ((callback (callback-lambda () (apply fn args))))
      (with-eval-after-load feature
        ;; TODO: should remove from after-load-alist
        (funcall callback)))))

(defmacro make-lazy (name feature fn)
  (declare (indent 2))
  `(defun ,name (&rest args)
     (alan-lazy-call ,feature ,fn args)))

(defun alan-clear-keymap (km)
  (let ((parent (keymap-parent km)))
    ;; https://stackoverflow.com/questions/45782226/emacs-unbind-all-keys-of-some-mode-map
    (setf (cdr km) nil)
    (set-keymap-parent km parent)))

(defmacro clear-and-backup-keymap (keymapsym)
  `(progn
     (defvar ,(intern (concat (symbol-name keymapsym) "-origional"))
       (copy-keymap ,keymapsym))
     (alan-clear-keymap ,keymapsym)))

(defun call-interactively-with-remap (f)
  (call-interactively (or (command-remapping f) f)))


;; https://stackoverflow.com/questions/13306807/how-to-check-if-point-is-at-the-point-of-indentation-in-emacs-lisp
(defun alan-point-at-eol-p ()
  ;;(interactive)
  (= (save-excursion (move-end-of-line nil) (point)) (point)))


(defun force--noerr (cb)
  ;; cant seem to be able to do this with just condition-case?
  ;; condition-case doesnt work with (top-level) for instance
  (let ((inhibit-quit t) success ans)
    (catch 'failed
      (unwind-protect
          (progn
            (condition-case err
                (let ((inhibit-quit nil))
                  (setq ans (funcall cb)))
              ((debug error)
               (span-notef "error when running %S: %S" cb err)
               nil))
            (setq success t))
        (unless success
          (span-notef "error when running %S" cb)
          (throw 'failed nil))))
    ans))

(defmacro force-noerr (&rest body)
  `(force--noerr (lambda () ,@body)))

(defun require-noerr (package)
  (force-noerr
   (require package)))

(defun alan-eval-after-load (file form)
  (declare (indent 1))
  (eval-after-load file
    (lambda ()
      (force-noerr
       (with-current-buffer (get-buffer-create " *eval-after-load*" t)
         (funcall form))))))

(defmacro eval-after-load! (feature &rest body)
  ;; with-eval-after-load but requires feature when byte compiling to silent errors.
  ;; warning: the byte compiler sees feature being loaded even afte this macro
  ;; TODO: maybe its possible to fix above?
  (declare (indent 1) (debug (form def-body)))
  (require-if-is-bytecompile--one feature)
  `(alan-eval-after-load ',feature (lambda () ,@body)))

(defun alan-ignore-error-advice (func &rest args)
  (ignore-errors
    (apply func args)))

(defun record-to-list (r)
  (--map (aref r it) (number-sequence 0 (1- (length r)))))

(defmacro alan-compile-or-keep (&rest form)
  (if (bound-and-true-p byte-compile-current-file)
      `(progn ,@form)
    `(eval '(progn ,@form) t)))

(defmacro alan-always-debug (&rest form)
  (let ((err-sym (make-symbol "err")))
    `(condition-case-unless-debug ,err-sym
         (progn ,@form)
       (t (signal (car ,err-sym) (cdr ,err-sym))))))

(defmacro alan-with-demoted-errors (&rest form)
  (let ((err-sym (make-symbol "err")))
    `(condition-case-unless-debug ,err-sym
         (let ((debugger #'span--debug)
               (inhibit-debugger nil)
               (debug-on-error t)
               (debug-on-quit t))
           ,@form)
       (t (span-notef "error: %s" (error-message-string ,err-sym))))))

;; TODO: this duplicates form
(defmacro alan-quit-on-input (&rest form)
  (let ((success-sym (make-symbol "success"))
        (ans-sym (make-symbol "ans")))
    `(if throw-on-input
         (progn ,@form)
       (let (,success-sym ,ans-sym)
         (while-no-input
           (setq ,ans-sym (progn ,@form))
           (setq ,success-sym t))
         (if ,success-sym
             ,ans-sym
           (let ((debug-on-quit nil))
             (signal 'quit nil)))))))


(defun alan-byte-compile-make-prog (file)
  `(let ((gc-cons-percentage 1.0)
         (load-path ',load-path)
         (load-prefer-newer t)
         (default-directory ',default-directory)
         bytecomp-did-fail)

     (setq-default
      byte-compile-log-warning-function
      (lambda (string position &optional fill level)
        (when (eq level :error)
          (setq bytecomp-did-fail t))
        (with-temp-buffer
          (insert-file-contents byte-compile-current-file)
          (goto-char position)
          (message "\n:%s file=%s,line=%s,col=%s::%s\n"
                   level
                   byte-compile-current-file (line-number-at-pos) (current-column)
                   string))))

     (require 'bytecomp)
     (message "compiling: %s" ',file)
     (byte-compile-file ',file)
     (kill-emacs (if bytecomp-did-fail 1 0))))

;; (alan-byte-compile-one "/home/alan/dotfiles_new/emacs/alan-cxx.el")
(defun alan-byte-compile-one (file)
  (cl-assert (file-exists-p file))
  (cl-destructuring-bind (exitcode stdout stderr)
      (elpaca-process-call (elpaca--emacs-path) "--batch" "--eval"
                           (let ((print-escape-newlines t) (print-level nil) (print-circle nil))
                             (prin1-to-string (alan-byte-compile-make-prog file))))
    (when stdout
      (message "%s" stdout))
    (when stderr
      (message "%s" stderr))
    exitcode))

(defun alan-byte-compile-files (files)
  (let (bytecomp-did-fail)
    (dolist (file files)
      (let ((exitcode (alan-byte-compile-one file)))
        (setq bytecomp-did-fail (or bytecomp-did-fail (not (= exitcode 0))))))
    bytecomp-did-fail))

(defun alan-byte-compile-dotemacs-dir ()
  (alan-byte-compile-files
   (directory-files alan-dotemacs-dir t (rx ".el" eos))))

(provide 'alan-utils2)
