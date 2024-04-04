;; -*- lexical-binding: t -*-

(require 'alan-config)
(require 'alan-utils)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (message "elpaca: generating autoloads")
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(require 'elpaca)

(defadvice! alan-elpaca--check-version-advice (orig-fun e)
  ;; fails on magit git-commit. supposedly never throw ig
  :around #'elpaca--check-version
  (condition-case-unless-debug nil
      (funcall orig-fun e)
    (error
     (elpaca--continue-build e))))

(defadvice! alan-elpaca-inject-deps (orig-fun e)
  :around #'elpaca--dependencies
  (let ((ans (funcall orig-fun e)))
    (append (plist-get (cdr-safe (elpaca<-order e)) :alan-extra-deps) ans)))

(add-hook 'alan-end-of-init-hook #'elpaca-process-queues)

(setq elpaca-cache-autoloads nil)
(setq elpaca-log-functions '())

(defun alan-elpaca-log-if-building (e)
  (let ((elpaca-log-functions '(elpaca-log-initial-queues elpaca-log-command-query)))
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

(eval-after-load! elpaca-log
  (setf (alist-get 'try elpaca-log-command-queries) "#latest #linked-errors"))


(provide 'alan-elpaca)
