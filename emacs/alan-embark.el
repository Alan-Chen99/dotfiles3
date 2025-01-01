;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'embark
  (startup-queue-package 'embark 0))


(eval-after-load! embark
  (setq
   embark-help-key "S-SPC"
   embark-cycle-key "SPC"
   embark-prompter #'embark-keymap-prompter
   embark-quit-after-action '((kill-buffer . t) (t . nil)))

  (setq embark-indicators
        '(
          ;; embark--vertico-indicator
          ;; embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (general-def embark-general-map
    "c" #'embark-collect)

  (general-def embark-file-map
    "c" #'embark-collect)

  (clear-and-backup-keymap embark-collect-mode-map)

  )

;; (use-package embark
;;   :functions embark-verbose-indicator-override
;;   :autoload (embark-keymap-prompter)
;;   :general
;;   (motion
;;    "S-SPC" #'embark-act)
;;   (embark-buffer-map
;;    "<up>" #'kill-current-buffer
;;    )
;;   (vertico-map
;;    :states 'insert
;;    "S-SPC" #'embark-act)
;;   :init
;;   (startup-queue-package 'embark 50)
;;   (defvar embark-indicators
;;     '(
;;       ;; embark--vertico-indicator
;;       ;; embark-mixed-indicator
;;       embark-highlight-indicator
;;       embark-isearch-highlight-indicator))

;;   (setq-default
;;    embark-help-key "S-SPC"
;;    embark-cycle-key "SPC"
;;    embark-prompter #'embark-keymap-prompter
;;    embark-quit-after-action '((kill-buffer . t) (t . nil)))
;;   ;;    embark-bindings-at-point
;;   ;; :config
;;   ;; embark-keymap-prompter
;;   ;; (setq-default embark-prompter #'embark-keymap-prompter)
;;   )

(provide 'alan-embark)
