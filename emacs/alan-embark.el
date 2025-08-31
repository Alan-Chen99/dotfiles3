;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-commands)

(pkg! 'embark
  (startup-queue-package 'embark 0))


(eval-after-load! embark
  (setq
   ;; embark-help-key "S-SPC"
   embark-help-key nil
   embark-cycle-key "SPC"
   embark-prompter #'embark-keymap-prompter
   embark-quit-after-action '((kill-buffer . t) (t . nil)))

  (setq embark-indicators
        '(
          embark-minimal-indicator
          embark--vertico-indicator
          ;; embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  ;; (setq embark-become-keymaps '(evil-motion-state-map))

  (defadvice! embark--become-keymap--override (&rest _args)
    :override #'embark--become-keymap
    evil-motion-state-map)

  (--each embark-keymap-alist (eval `(clear-and-backup-keymap ,(-last-item it))))

  ;; eval (clear-and-backup-keymap)

  ;; embark-keymap-alist

  (general-def embark-general-map
    "S-SPC" #'embark-keymap-help
    "SPC" #'embark-cycle

    "n" (lambda () (interactive) (embark--run-after-command #'evil-normal-state))
    "t" #'alan-top-level-keep-windows

    "a" #'embark-act-all
    "b" #'embark-become
    "c" #'embark-collect
    "e" #'embark-export
    "l" #'embark-live
    "y" #'embark-copy-as-kill

    ;; "r" #'embark-history-remove
    )

  (general-def embark-symbol-map
    "h" #'describe-symbol
    "d" #'embark-find-definition
    "e" #'pp-eval-expression
    )


  ;; embark--act treats embark-become specially
  ;; so we cant define another command to call embark-become
  (defadvice! embark-become--always-full (fn &optional _full)
    :around #'embark-become
    (funcall fn 'full))

  ;; embark-become

  ;; (setq embark-prompter 'embark-completing-read-prompter)


  ;; (general-def embark-file-map
  ;;   "c" #'embark-collect)

  ;; (general-def embark-command-map
  ;;   "c" #'embark-collect)

  ;; (clear-and-backup-keymap embark-collect-mode-map)

  )

(provide 'alan-embark)
