;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'markdown-mode)


(eval-after-load! markdown-mode
  ;; (clear-and-backup-keymap markdown-mode-map)

  (setq markdown-enable-math t)
  (setq markdown-fontify-code-blocks-natively t)

  ;; TODO: why is this here
  ;; (face-spec-set 'markdown-header-face-1 '((t :inherit markdown-header-face)) 'face-defface-spec)
  ;; (face-spec-set 'markdown-header-face-2 '((t :inherit markdown-header-face)) 'face-defface-spec)
  ;; (face-spec-set 'markdown-header-face-3 '((t :inherit markdown-header-face)) 'face-defface-spec)
  ;; (face-spec-set 'markdown-header-face-4 '((t :inherit markdown-header-face)) 'face-defface-spec)
  ;; (face-spec-set 'markdown-header-face-5 '((t :inherit markdown-header-face)) 'face-defface-spec)
  ;; (face-spec-set 'markdown-header-face-6 '((t :inherit markdown-header-face)) 'face-defface-spec)

  (add-hook! 'markdown-mode-hook
    (defun alan-setup-markdown ()
      (setq-local evil-shift-width 2)
      (setq-local format-all-formatters '(("Markdown" prettierd))))))


(provide 'alan-markdown)
