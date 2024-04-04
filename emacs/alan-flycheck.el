;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'flycheck
  (startup-queue-package 'flycheck 0))

(make-lazy alan-flycheck-deferred 'flycheck 'flycheck-mode)

(setq-default flycheck-mode-line nil)

(eval-after-load! flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-display-errors-function nil)

  (setq flycheck-check-syntax-automatically
        '(
          save
          ;; idle-buffer-switch
          ;; idle-change
          ;; new-line
          mode-enabled
          ))

  ;; remove outdated lints
  ;; TODO: remove lsp-mode caches too?
  (defadvice! alan-flycheck-clear (&rest _args)
    :after #'flycheck-handle-change
    (when (eq flycheck-last-status-change 'finished)
      (flycheck-clear)))


  (add-hook! 'flycheck-status-changed-functions
    (defun my-flycheck-update-on-status-change (status)
      (setq-local
       flycheck-mode-line
       (pcase status
         (`not-checked nil)
         (`no-checker nil)
         (`errored " [!]")
         (`running " [*]")
         (`finished
          (let-alist (flycheck-count-errors flycheck-current-errors)
            (when (or .error .warning)
              (list
               " ["
               (when .error
                 `(:propertize ,(format "%s" .error) face flycheck-error-list-error))
               (when (and  .error .warning) " ")
               (when .warning
                 `(:propertize ,(format "%s" .warning) face flycheck-error-list-warning))
               "]"))))
         (`interrupted " [.]")
         (`suspicious " [?]")))
      )))


(provide 'alan-flycheck)
