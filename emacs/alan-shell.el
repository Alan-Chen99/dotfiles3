;; -*- lexical-binding: t -*-

(require 'alan-core)

(add-to-list 'major-mode-remap-alist (cons #'sh-mode #'bash-ts-mode))

(add-to-list 'auto-mode-alist (cons (rx "/.env" eos) #'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "/.envrc" eos) #'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "-hook" eos) #'bash-ts-mode))

(eval-after-load! sh-script
  (add-hook! 'bash-ts-mode-hook
    (defun alan-setup-bash ()
      (setq-local format-all-formatters '(("Shell" shfmt))))))

(provide 'alan-shell)
