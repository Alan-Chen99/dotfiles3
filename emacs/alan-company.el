;; -*- lexical-binding: t -*-

(require 'alan-core)


(pkg! 'company
  (startup-queue-package 'company 80)
  (startup-queue-package 'company-capf 80)
  (startup-queue-package 'company-files 80)

  ;; (add-hook-once! 'pre-command-hook (require 'company))
  )

(pkg! 'company-quickhelp
  (eval-after-load! company
    (startup-queue-package 'company-quickhelp 80)))


(eval-after-load! company

  (add-hook! 'alan-completion-at-point-hook
    (when company-mode
      (company-manual-begin)
      t))

  (setq company-backends
        '(
          company-capf
          ;; company-bbdb
          ;; company-semantic
          ;; company-cmake
          ;; company-clang
          company-files ; TODO: this can hang if the directory have large # of files
          ;; (company-dabbrev-code company-gtags company-etags company-keywords)
          ;; company-oddmuse
          ;; company-dabbrev
          ))
  (let (company-backends)
    (global-company-mode t))

  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 0

   company-format-margin-function nil

   ;; TODO: also flip the up and down keybindings in company-active-map
   ;; company-tooltip-flip-when-above t
   company-tooltip-margin 0
   ;; company-tooltip-align-annotations t ;; what does this do?
   )

  (setq company-frontends
        '(
          ;; company-pseudo-tooltip-unless-just-one-frontend
          company-pseudo-tooltip-frontend ;; cant seem to use the unless-just-one bc quickhelp seems to depend on this
          company-preview-frontend
          ;; company-echo-metadata-frontend
          ))

  (clear-and-backup-keymap company-active-map)

  ;; (setq-default last-is-complete-common nil)
  (defun alan-company-complete-common-or-select ()
    (interactive)
    (when (company-manual-begin)
      (if
          (or
           company-selection-changed
           (eq company-common nil)
           (string= (downcase company-prefix) (downcase company-common))
           ;; (and (eq real-last-command 'company-complete-common-or-select)
           ;;   last-is-complete-common)
           )
          (progn
            (call-interactively #'company-complete-selection)
            ;; (setq-local last-is-complete-common nil)
            )
        (progn
          (call-interactively #'company-complete-common)
          ;; (setq-local last-is-complete-common t)
          ))))

  (general-def company-active-map
    :state 'insert
    ;; "<left>" 'company-abort
    ;; "<left>" (lambda () (interactive) (company-abort) (left-char))
    "<down>" #'company-select-next
    "<up>" #'company-select-previous
    "<.> j" 'alan-company-complete-common-or-select
    ;; recursion caused by <escape> after company-complete in normal state
    ;; TODO: why?
    "<escape>"
    (let ((company-abort-recursion-guard nil)) ;lexical let
      (lambda ()
        (interactive)
        (unless company-abort-recursion-guard
          (setq company-abort-recursion-guard t)
          (unwind-protect
              (progn
                (company-abort)
                (execute-kbd-macro (kbd "<escape>")))
            (setq company-abort-recursion-guard nil)))))
    "<.> <up>" #'company-show-doc-buffer)

  (eval-after-load! eldoc
    (eldoc-add-command #'company-abort)
    (eldoc-add-command #'company-select-next)
    (eldoc-add-command #'company-select-previous)
    (eldoc-add-command #'company-complete))
  )

(eval-after-load! company-capf
  (defadvice! company-capf--nonessential (fn &rest args)
    :around #'company-capf
    (let ((non-essential t))
      (apply fn args))))

(eval-after-load! company-dabbrev
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase nil))

(eval-after-load! company-dabbrev-code
  (setq company-dabbrev-code-ignore-case t))

(eval-after-load! company-etags
  (setq company-etags-ignore-case t))



(eval-after-load! company-quickhelp
  (setq-default company-quickhelp-delay 0.05)

  ;; https://superuser.com/questions/669701/emacs-disable-some-minibuffer-messages
  ;; https://emacs.stackexchange.com/questions/29111/echo-area-disable-help-messages
  (defun suppress-quickhelp-minibuffer-messages (old-fun &rest args)
    (advice-add #'help-window-display-message :override #'ignore)
    (unwind-protect
        (apply old-fun args)
      (advice-remove #'help-window-display-message #'ignore)))
  (advice-add #'company-quickhelp--show :around 'suppress-quickhelp-minibuffer-messages)

  (company-quickhelp-mode))


(provide 'alan-company)
