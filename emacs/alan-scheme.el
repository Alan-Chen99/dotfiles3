;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-lsp)

(pkg! 'lsp-scheme)

(eval-after-load! scheme
  (seq-doseq (p "_-:#><?!%*")
    (modify-syntax-entry p "w" scheme-mode-syntax-table))

  (add-hook! 'scheme-mode-hook
    (defun alan-setup-scheme ()

      (alan-lsp-deferred 'lsp-scheme)

      (setq-local format-all-formatters '(("Scheme" schemat)))

      (tree-sitter-hl-mode-lazy)

      ))


  )

(eval-after-load! lsp-scheme
  (lsp-scheme--guile-register-client)
  (setq lsp-scheme-log-level "warning"))


(provide 'alan-scheme)
