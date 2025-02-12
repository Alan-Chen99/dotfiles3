;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-lsp)

(pkg! 'csharp-mode)

(add-hook! 'csharp-mode-hook
  (defun alan-setup-csharp ()

    ;; (setq-local format-all-formatters '(("C++" clang-format)))
    (alan-lsp-deferred 'lsp-csharp
      ;; (lsp-completion-mode)
      ;; (setq-local completion-at-point-functions (list #'lsp-completion-at-point))
      )
    ))

(provide 'alan-csharp)
