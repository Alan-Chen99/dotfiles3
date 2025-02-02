;; -*- lexical-binding: t -*-
;; utils loaded after elpaca, so can use dependencies

(require 'dash)
(require 'general)

(require 'alan-utils)
(require 'alan-elpaca)

(defun alan-eval-after-load (file form)
  (declare (indent 1))
  (eval-after-load file
    (lambda ()
      (with-current-buffer (get-buffer-create " *eval-after-load*" t)
        (funcall form)))))

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
    (require-if-is-bytecompile--one feature))
  nil)

(defmacro eval-after-load! (feature &rest body)
  ;; with-eval-after-load but requires feature when byte compiling to silent errors.
  ;; warning: the byte compiler sees feature being loaded even afte this macro
  ;; TODO: maybe its possible to fix above?
  (declare (indent 1) (debug (form def-body)))
  (require-if-is-bytecompile--one feature)
  `(alan-eval-after-load ',feature (lambda () ,@body)))

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

(defun alan-ignore-error-advice (func &rest args)
  (ignore-errors
    (apply func args)))

(defun record-to-list (r)
  (--map (aref r it) (number-sequence 0 (1- (length r)))))

(defmacro alan-compile-or-keep (&rest form)
  (if (bound-and-true-p byte-compile-current-file)
      `(progn ,@form)
    `(eval '(progn ,@form) t)))

(provide 'alan-utils2)
