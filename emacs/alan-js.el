;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-lsp)

(require-if-is-bytecompile lsp-completion)

(eval-after-load! lsp-eslint
  ;; (setq lsp-eslint-server-command '("yarn" "eslint-lsp" "--stdio")))
  (setq lsp-eslint-server-command '("eslint-lsp" "--stdio")))

(add-to-list 'major-mode-remap-alist (cons #'javascript-mode #'typescript-ts-mode))
(add-to-list 'major-mode-remap-alist (cons #'js-json-mode #'json-ts-mode))
(add-to-list 'major-mode-remap-alist (cons #'css-mode #'css-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-ts-mode))

(defun alan-setup-typescript ()
  (setq-local format-all-formatters '(("TypeScript" prettierd)))
  ;; (alan-lsp-deferred 'lsp-javascript
  ;; (setq lsp-eslint-server-command '("eslint-lsp" "--stdio"))
  (alan-lsp-deferred '(lsp-javascript lsp-eslint)
    (lsp-completion-mode)
    (setq-local completion-at-point-functions (list #'lsp-completion-at-point))))

(add-hook 'typescript-ts-mode-hook #'alan-setup-typescript)
(add-hook 'tsx-ts-mode-hook #'alan-setup-typescript)


(add-hook! 'json-ts-mode-hook
  (defun alan-setup-json ()
    (setq-local format-all-formatters '(("JSON" prettierd)))))

(add-hook! 'css-ts-mode-hook
  (defun alan-setup-css ()
    (setq-local format-all-formatters '(("CSS" prettierd)))))

(eval-after-load! typescript-ts-mode
  (seq-doseq (p "_#")
    (modify-syntax-entry p "w" typescript-ts-mode--syntax-table)))

(provide 'alan-js)
