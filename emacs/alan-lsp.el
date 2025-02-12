;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)

(setenv "LSP_USE_PLISTS" "true")
(pkg! 'lsp-mode)


;; (defvar my-lsp-keymap-mode-map (make-sparse-keymap))
;; (define-minor-mode my-lsp-keymap-mode "TODO: make lsp-mode-map work with evil"
;;   :keymap my-lsp-keymap-mode-map)
(require-if-is-bytecompile lsp-completion)

(eval-after-load! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories (rx string-start "/nix/store/"))
  (add-to-list 'lsp-file-watch-ignored-directories (rx "/aliases/"))
  (add-to-list 'lsp-file-watch-ignored-directories (rx "/result"))

  (add-to-list 'lsp-file-watch-ignored-directories (rx ".hypothesis"))
  ;; (add-to-list 'lsp-file-watch-ignored (rx ".hypothesis"))

  (defvar lsp-client-packages-original lsp-client-packages)
  (setq lsp-client-packages nil)

  (clear-and-backup-keymap lsp-mode-map)
  (general-def lsp-mode-map
    :states 'motion
    ;; :definer 'minor-mode
    ;; :keymaps 'my-lsp-keymap-mode
    "<.> <down>" #'lsp-find-references
    "<.> m" #'lsp-goto-type-definition
    "<.> <up>" #'lsp-execute-code-action)

  (add-hook! 'lsp-mode-hook #'evil-normalize-keymaps)
  ;;   (my-lsp-keymap-mode (if lsp-mode 1 -1)))

  (advice-add #'lsp--info :around #'with-no-minibuffer-message-advice)

  ;; prevent markdown that dont specificy lang to be rendered in buffer major mode lang
  ;; since often they arent intended to be so
  (defadvice! lsp--setup-markdown-ignore-orig-mode (fn _mode)
    :around #'lsp--setup-markdown
    (funcall fn nil))

  ;; (defadvice! lsp--render-string-remove-extra-newlines (fn str language)
  ;;   :around #'lsp--render-string
  ;;   (let ((ans (funcall fn str language)))
  ;;     (setq ans (string-join (string-split ans "\n" t) "\n"))
  ;;     ans))

  ;; TODO: do i still need this?
  ;; TODO: this is hack, for c++ clangd bad render
  ;; (setq-default markdown-regex-angle-uri nil)

  (setq
   lsp-enable-symbol-highlighting nil
   ;; lsp-enable-indentation nil
   lsp-enable-snippet nil
   ;; TODO
   lsp-lens-enable nil
   lsp-modeline-code-actions-enable nil

   lsp-completion-show-detail nil
   lsp-completion-show-kind nil
   lsp-completion-show-label-description nil
   lsp-completion-enable-additional-text-edit nil
   ;; lsp-completion-filter-on-incomplete nil
   lsp-completion-use-last-result nil
   lsp-completion-no-cache t

   lsp-signature-auto-activate t
   lsp-eldoc-render-all t

   lsp-idle-delay 0.2

   ;;TODO: make them show correctly
   lsp-signature-render-documentation nil
   ;; lsp-signature-render-documentation t
   )

  ;; (alan-set-ignore-debug-on-error #'lsp--server-binary-present?)

  (add-to-list 'debug-ignored-errors 'lsp-no-code-actions))

(eval-after-load! lsp-modeline
  (setq
   lsp-modeline-code-actions-segments '(name) ;; disables icons
   ;; lsp-auto-guess-root t

   ))

(eval-after-load! lsp-diagnostics
  (defadvice! alan-lsp-diagnostics--flycheck-calculate-level (severity _tags)
    :override #'lsp-diagnostics--flycheck-calculate-level
    (pcase severity
      (1 'error)
      (2 'warning)
      (3 'info)
      (4 'supress)
      ;; (4 'info)
      (_ 'error)))
  (eval-after-load! flycheck
    (flycheck-define-error-level 'supress
      :severity -100
      ;; :compilation-level (get flycheck-level 'flycheck-compilation-level)
      ;; :overlay-category category
      ;; :fringe-bitmap bitmap
      ;; :fringe-face (get flycheck-level 'flycheck-fringe-face)
      ;; :error-list-face face
      )))

(defun alan--lsp-deferred (clients &optional callback)
  (unless (local-variable-p 'header-line-format)
    (setq-local header-line-format ""))
  (when (symbolp clients)
    (setq clients (list clients)))
  (dolist (c clients)
    (startup-queue-package c 100))
  (let ((buf (current-buffer))
        (count 0))
    (dolist (c clients)
      (alan-eval-after-load c
        (lambda ()
          (cl-incf count)
          (when (length= clients count)
            (run-with-idle-timer
             0.05 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (< (buffer-size) 100000)
                     (lsp-deferred)
                     (when callback
                       (funcall callback)))))))))))))

(defmacro alan-lsp-deferred (client &rest body)
  (declare (indent 1))
  `(alan--lsp-deferred ,client (lambda () ,@body)))


(provide 'alan-lsp)
