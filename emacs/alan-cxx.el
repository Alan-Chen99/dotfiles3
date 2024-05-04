;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)

(add-to-list 'major-mode-remap-alist (cons #'c-mode #'c++-ts-mode))
(add-to-list 'major-mode-remap-alist (cons #'c++-mode #'c++-ts-mode))
(add-to-list 'major-mode-remap-alist (cons #'c-or-c++-mode #'c++-ts-mode))

(add-hook! 'c++-ts-mode-hook
  (defun alan-setup-cxx ()
    (setq evil-goto-definition-functions
          (list
           #'evil-goto-definition-xref
           #'evil-goto-definition-visit-url))

    (alan-lsp-deferred 'lsp-clangd
      ;; (lsp-completion-mode)
      ;; (setq-local completion-at-point-functions (list #'lsp-completion-at-point))
      )
    ))

(eval-after-load! c-ts-mode
  (modify-syntax-entry (string-to-char "_") "w" c-ts-mode--syntax-table))

(provide 'alan-cxx)
