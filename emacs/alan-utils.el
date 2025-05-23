;; -*- lexical-binding: t -*-

(defvar alan-real-early-init nil)
(defvar alan-gc-cons-threshold (* 800000 2))

(setq debug-on-error t)
(setq debug-on-quit t)
(setq gc-cons-threshold (* 800000 200))

(require 'span)
(require 'alan-macros)

(cl-assert (= (minibuffer-depth) 0))

;; https://emacs.stackexchange.com/questions/14706/suppress-message-in-minibuffer-when-a-buffer-is-saved
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Messages.html
(defmacro with-no-minibuffer-message (&rest forms)
  `(let
       ((inhibit-message t)
        (set-message-function (lambda (_x) 'dont-set-message))
        (clear-message-function (lambda () 'dont-clear-message)))
     ,@forms))

(defmacro redirect-minibuffer-message (&rest forms)
  `(let
       ((set-message-function (lambda (msg)
                                (span-notef "%s" msg)
                                t))
        (clear-message-function #'span--dont-clear-message))
     ,@forms))

(defun with-no-minibuffer-message-advice (func &rest args)
  (with-no-minibuffer-message
   (apply func args)))

;; (defvar alan-dependency-alist-new nil)
(defvar alan-record-deps-func nil)
(defvar alan-dependency-alist-new-tmp nil)
(defvar alan-ignored-require nil)
(defvar alan-ignored-require-for nil)

(defun maybe-record-dependency (feature)
  (when-let* ((parent (span-var 'current-require-or-load)))
    (when (symbolp parent)
      (cl-pushnew feature (alist-get parent alan-dependency-alist-new-tmp)))))

(defadvice! alan-require-span (orig-fun feature &optional filename noerror)
  :around #'require
  (unless filename
    (maybe-record-dependency feature))
  (if (or (featurep feature)
          (and (eq (span-var 'current-require-or-load) alan-ignored-require-for)
               (memq feature alan-ignored-require)))
      feature
    (prog1
        (funcall orig-fun feature filename noerror)
      (when alan-record-deps-func
        (funcall alan-record-deps-func feature (nreverse (alist-get feature alan-dependency-alist-new-tmp))))
      (setf (alist-get feature alan-dependency-alist-new-tmp) nil))))


(defvar alan-gc-timer nil)
(defvar alan--did-gc t)

(defun alan-do-gc ()
  (setq alan--did-gc t)
  (let ((prev-gcs-done gcs-done) (prev-gc-elapsed gc-elapsed))
    (span :gc
      (let ((garbage-collection-messages nil)
            (gc-cons-threshold alan-gc-cons-threshold))
        (eval '(ignore nil) t))

      (when (> gcs-done prev-gcs-done)
        (let ((time (- gc-elapsed prev-gc-elapsed)))
          (when (> time 0.1)
            (span-notef "gc: %.3fs" time)))))))

;; so that we do gc even if we leave emacs alone
(defun alan-do-gc-long ()
  ;; (span-msg "alan-do-gc-long")
  (unless alan--did-gc
    (span :gc-long
      ;; (span-flush)
      (alan-do-gc)))
  (setq alan--did-gc nil))

(unless alan-gc-timer
  (setq alan-gc-timer t)
  (run-with-idle-timer 1 t #'alan-do-gc)
  (run-at-time nil 60 #'alan-do-gc-long))

;; so that we can load the entire init file after startup and still work
(defvar alan-end-of-init-hook nil)
(defvar alan-inhibit-end-of-init-hook nil)

(defvar alan-finished-early-init nil)


(defun alan-set-ignore-debug-on-quit-advice (orig-fun &rest args)
  (let ((debug-on-quit nil))
    (apply orig-fun args)))
(defun alan-set-ignore-debug-on-quit (fn)
  (advice-add fn :around #'alan-set-ignore-debug-on-quit-advice))

(defun alan-set-ignore-debug-on-error-advice (orig-fun &rest args)
  (let ((debug-on-error nil))
    (apply orig-fun args)))
(defun alan-set-ignore-debug-on-error (fn)
  (advice-add fn :around #'alan-set-ignore-debug-on-error-advice))

(defun alan-run-per-frame (fn)
  (dolist (f (frame-list))
    (funcall fn f))
  (add-hook 'after-make-frame-functions fn))

(defun alan-read-elisp-data-file (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun alan-write-elisp-data-file (file data)
  (with-temp-file file
    (require 'pp)
    (let (print-circle
          print-level
          print-length
          print-gensym
          (print-symbols-bare t))
      (pp data (current-buffer)))))

(defmacro assert (x)
  (declare (indent 1))
  `(let ((assert--tmp ,x))
     (cl-assert assert--tmp)
     assert--tmp))

(provide 'alan-utils)
