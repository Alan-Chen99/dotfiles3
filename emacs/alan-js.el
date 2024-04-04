;; -*- lexical-binding: t -*-

(require 'alan-core)


(add-to-list 'major-mode-remap-alist (cons #'javascript-mode #'typescript-ts-mode))
(add-to-list 'major-mode-remap-alist (cons #'js-json-mode #'json-ts-mode))

(add-hook! 'typescript-ts-mode-hook
  (defun alan-setup-typescript ()
    (setq-local format-all-formatters '(("TypeScript" prettierd)))
    (alan-lsp-deferred 'lsp-javascript
      (lsp-completion-mode)
      (setq-local completion-at-point-functions (list #'lsp-completion-at-point)))))


(add-hook! 'json-ts-mode-hook
  (defun alan-setup-json ()
    (setq-local format-all-formatters '(("JSON" prettierd)))))


(provide 'alan-js)
