;; -*- lexical-binding: t -*-

(require 'cl-lib)
(cl-assert (version<= "29" emacs-version))

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(set-buffer (get-buffer-create " *initialization*"))

(defvar alan-dotemacs-dir)
(when load-file-name
  (setq alan-dotemacs-dir (file-name-directory (file-chase-links load-file-name))))

(add-to-list 'load-path alan-dotemacs-dir)

(defun ci--redirect-to-stdout (msg)
  (message "%s" msg))
(defvar span-log-handler #'ci--redirect-to-stdout)

(require 'alan-utils)
(require 'alan-config)
(require 'alan-elpaca)

(defvar ci-build-verbose nil)
(defadvice! elpaca--log--redirect (e text &optional _verbosity _replace)
  :before #'elpaca--log
  (when ci-build-verbose
    (span-notef "%-20s %-10s %-10s" (elpaca<-id e) (elpaca--status e) text)))

(require 'alan)

(defun ci-write-failed (e)
  (cl-loop for (status _ info _) in (reverse (elpaca<-log e)) concat
           (span-notef "%-20s %-10s %-10s" (elpaca<-id e) status info))
  (span-notef))

(cl-defun ci-wait-for-pkgs-build ()
  (cl-block nil
    (while t
      (let ((statuses (elpaca--count-statuses)))
        (if (or (alist-get 'other statuses) (alist-get 'blocked statuses))
            (sit-for 0.1)
          (cl-return statuses))))))

(defvar ci-pkgs-statuses)
(setq ci-pkgs-statuses (ci-wait-for-pkgs-build))
(span-dbgf ci-pkgs-statuses)

(defun ci-show-build-errs ()
  (span-flush-log)
  (span-notef)
  (span-notef)
  (span-notef)
  (span-notef)
  (span-notef)
  (span-msg "%s" ci-pkgs-statuses)
  (span-notef)
  (cl-loop for q in elpaca--queues
           do (cl-loop for (_ . e) in (elpaca-q<-elpacas q)
                       for status = (elpaca--status e)
                       do (unless (eq status 'finished) (ci-write-failed e))))
  (kill-emacs (if (alist-get 'failed ci-pkgs-statuses) 1 0)))

(defun ci-load-packages ()
  (while process-queue-thread-exist
    (sit-for 0.1)))

(defun ci-emit-bytecomp-warning (string position &optional _fill level)
  (with-temp-buffer
    (insert-file-contents byte-compile-current-file)
    (goto-char position)
    (let ((standard-output t))
      (princ (format-message ":%s file=%s,line=%s,col=%s::%s\n"
                             level
                             byte-compile-current-file (line-number-at-pos) (current-column)
                             string)))))

(defun ci-byte-compile ()
  (let ((byte-compile-log-warning-function #'ci-emit-bytecomp-warning))
    (span-with-no-minibuffer-message
     (byte-recompile-directory alan-dotemacs-dir 0 'force))
    ;; (with-current-buffer "*Compile-Log*"
    ;;   (span-msg "%s" (buffer-string)))
    ))
