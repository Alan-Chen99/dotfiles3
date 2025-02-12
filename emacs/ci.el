;; -*- lexical-binding: t -*-

(require 'cl-lib)

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
    ;; (when (eq (elpaca<-id e) 'compat)
    ;;   (span--backtrace))
    (span-notef "%-20s %-10s %-10s" (elpaca<-id e) (elpaca--status e) text)))

(require 'alan)

(defun ci-write-failed (e)
  (let ((standard-output t))
    (princ (format-message
            "\n::error ::package %s failed to build\n"
            (elpaca<-id e))))
  (cl-loop for (status _ info _) in (reverse (elpaca<-log e)) concat
           (span-notef "%-20s %-10s %-10s" (elpaca<-id e) status info))
  (span-notef))

(span-instrument elpaca--queue-dependencies :verbose t)
;; (span-instrument elpaca--continue-build :verbose t)

(cl-defun ci-wait-for-pkgs-build ()
  (cl-block nil
    (while t
      (let ((statuses (elpaca--count-statuses)))

        ;; (span :statuses
        ;;   (span-notef "%s" (cl-prin1-to-string (elpaca-get 'compat)))
        ;;   (span-dbgf statuses)
        ;;   (cl-loop for q in elpaca--queues
        ;;            do (cl-loop for (_ . e) in (elpaca-q<-elpacas q)
        ;;                        for status = (elpaca--status e)
        ;;                        do (unless (eq status 'finished)
        ;;                             (span-notef "%s %s" status (elpaca<-id e))))))

        (if (or (alist-get 'other statuses) (alist-get 'blocked statuses))
            (sit-for 0.1)
          (cl-return statuses))))))

(defun ci-show-build-errs ()
  (let ((ci-pkgs-statuses (ci-wait-for-pkgs-build)))
    (span-dbgf ci-pkgs-statuses)
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
    (kill-emacs
     (if (and (version<= "29" emacs-version) (alist-get 'failed ci-pkgs-statuses))
         1 0))))

(defun ci-load-packages ()
  (while process-queue-thread-exist
    (sit-for 0.1)))

(defun ci-byte-compile-core ()
  (let* ((default-directory alan-dotemacs-dir)
         (did-error
          (alan-byte-compile-files
           '("span-fmt.el" "span.el" "alan-macros.el" "alan-utils.el" "alan-utils2.el" "alan-elpaca.el"))))
    (span-dbgf did-error)
    (kill-emacs
     (if (and (version<= "29" emacs-version) did-error)
         1 0))))

(defun ci-byte-compile ()
  (let* ((did-error (alan-byte-compile-dotemacs-dir)))
    (kill-emacs
     (if (and (version<= "29" emacs-version) did-error)
         1 0))))
