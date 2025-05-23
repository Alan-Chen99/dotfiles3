;; -*- lexical-binding: t -*-

(require 'alan-config)
(require 'alan-utils)

(defvar elpaca-installer-version 0.9)
(defvar elpaca-order '(elpaca :repo "https://github.com/Alan-Chen99/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(defvar elpaca-core-date '(-1))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo)
       buffer emacs success)
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (unwind-protect
        (span :elpaca-init
          (setq buffer (assert (pop-to-buffer-same-window "*elpaca-bootstrap*")))
          (assert (zerop
                   (apply #'call-process
                          `("git" nil ,buffer t "clone"
                            ,@(when-let* ((depth (plist-get order :depth)))
                                (list (format "--depth=%d" depth) "--no-single-branch"))
                            ,(plist-get order :repo) ,repo))))
          (assert (zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
          (setq emacs (assert (concat invocation-directory invocation-name)))
          (assert (zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
          (assert (require 'elpaca))
          (assert (elpaca-generate-autoloads "elpaca" repo))
          (setq success t))
      (when buffer
        (with-current-buffer buffer
          (message "*elpaca-bootstrap*:\n%s" (buffer-string)))
        (kill-buffer buffer))
      (unless success
        (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (message "elpaca: generating autoloads")
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(require 'elpaca)
(require 'elpaca-menu-elpa)

;; https://git.savannah.gnu.org is often down, we dont use it
(setq elpaca-menu-elpas
      `((gnu . ((name         . "GNU ELPA")
                (cache . ,(elpaca--read-file (expand-file-name "gnu-elpa.eld" elpaca-cache-directory)))
                (cache-path   . ,(expand-file-name "gnu-elpa.eld" elpaca-cache-directory))
                (packages-url . "https://raw.githubusercontent.com/emacsmirror/gnu_elpa/refs/heads/main/elpa-packages")
                (metadata-url . "https://elpa.gnu.org/packages/")
                (remote       . "https://github.com/emacsmirror/gnu_elpa.git")
                (branch-prefix . "externals")))
        (nongnu . ((name         . "NonGNU ELPA")
                   (cache-path   . ,(expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory))
                   (cache . ,(elpaca--read-file (expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory)))
                   (packages-url . "https://raw.githubusercontent.com/emacsmirror/nongnu_elpa/refs/heads/main/elpa-packages")
                   (metadata-url . "https://elpa.nongnu.org/nongnu/")
                   (remote       . "https://github.com/emacsmirror/nongnu_elpa.git")
                   (branch-prefix . "elpa")))))
(setq elpaca-lock-file (when load-file-name (expand-file-name "../elpaca-lock.eld" load-file-name)))

(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode))

(setq elpaca-queue-limit 10)

(defadvice! alan-elpaca--check-version-advice (orig-fun e)
  ;; fails on magit git-commit. supposedly never throw ig
  :around #'elpaca--check-version
  (condition-case-unless-debug nil
      (funcall orig-fun e)
    (error
     (elpaca--continue-build e))))

(defadvice! alan-elpaca-inject-deps (orig-fun e &optional recache)
  :around #'elpaca--dependencies
  ;; (span-dbgf debugger debug-on-error inhibit-debugger)
  ;; (span--backtrace)
  (let ((ans (funcall orig-fun e recache)))
    (append (plist-get (cdr-safe (elpaca<-order e)) :alan-extra-deps) ans)))

(add-hook 'alan-end-of-init-hook #'elpaca-process-queues)

(setq elpaca-cache-autoloads nil)
(setq elpaca-log-functions '())

(defun alan-elpaca-log-if-building (e)
  (let ((elpaca-log-functions (lambda () t)))
    (elpaca--maybe-log))
  (elpaca--continue-build e))

(add-to-list 'elpaca-build-steps #'alan-elpaca-log-if-building)

(defun alan-elpaca-post-activiate (e)
  (unwind-protect
      (let*
          (
           (order (elpaca<-order e))
           (init-fn (plist-get (cdr-safe order) :alan-init-fn)))
        ;; (message-with-time-and-indent "alan-elpaca-post-activiate: %S" order)
        (when init-fn
          (with-current-buffer (get-buffer-create " *elpaca-post-activiate*")
            (funcall init-fn))))
    (elpaca--continue-build e)))

(add-to-list 'elpaca-build-steps #'alan-elpaca-post-activiate t)
(add-to-list 'elpaca--pre-built-steps #'alan-elpaca-post-activiate t)

(cl-pushnew 'treesit elpaca-ignored-dependencies)

(defun alan-elpaca-queue (order init-fn)
  (if (listp order)
      (setq order (append order `(:alan-init-fn ,init-fn)))
    (setq order `(,order :alan-init-fn ,init-fn)))
  (elpaca--queue order)
  nil)


(defmacro pkg! (order &rest body)
  (declare (indent 1) (debug t))
  (if body
      `(alan-elpaca-queue ,order (lambda () ,@body))
    `(elpaca--queue ,order)))

(discard-input)
(pkg! elpaca-order)

(defalias 'try #'elpaca-try)

(defvar elpaca-log-command-queries)
(with-eval-after-load 'elpaca-log
  (setf (alist-get 'try elpaca-log-command-queries) "#latest #linked-errors"))


(provide 'alan-elpaca)
