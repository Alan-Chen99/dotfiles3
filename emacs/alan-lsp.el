;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'lsp-mode)


;; (defvar my-lsp-keymap-mode-map (make-sparse-keymap))
;; (define-minor-mode my-lsp-keymap-mode "TODO: make lsp-mode-map work with evil"
;;   :keymap my-lsp-keymap-mode-map)


(eval-after-load! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories (rx string-start "/nix/store/"))
  (add-to-list 'lsp-file-watch-ignored (rx string-start "/nix/store/"))

  (defvar lsp-client-packages-original lsp-client-packages)
  (setq lsp-client-packages nil)

  (clear-and-backup-keymap lsp-mode-map)
  (general-def lsp-mode-map
    :states 'motion
    ;; :definer 'minor-mode
    ;; :keymaps 'my-lsp-keymap-mode
    "<leader> <down>" #'lsp-find-references
    "<leader> <up>" #'lsp-execute-code-action)

  ;; (add-hook! 'lsp-mode-hook
  ;;   (my-lsp-keymap-mode (if lsp-mode 1 -1)))

  (advice-add #'lsp--info :around #'with-no-minibuffer-message-advice)

  (setq
   lsp-enable-symbol-highlighting nil
   ;; lsp-enable-indentation nil
   lsp-enable-snippet nil
   ;; TODO
   lsp-lens-enable nil

   lsp-completion-show-detail nil
   lsp-completion-show-kind nil
   lsp-completion-show-label-description nil
   lsp-completion-enable-additional-text-edit nil
   lsp-completion-filter-on-incomplete nil
   lsp-completion-use-last-result nil
   lsp-completion-no-cache t

   lsp-signature-auto-activate t
   lsp-eldoc-render-all t

   lsp-idle-delay 0.2

   ;;TODO: make them show correctly
   lsp-signature-render-documentation nil
   ;; lsp-signature-render-documentation t
   ))

(eval-after-load! lsp-modeline
  (setq
   lsp-modeline-code-actions-segments '(name) ;; disables icons
   ;; lsp-auto-guess-root t

   ))

(provide 'alan-lsp)
