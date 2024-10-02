;; -*- lexical-binding: t -*-

(require 'alan-core)


(defvar codeium/metadata/api_key "************************************")

(pkg! '(codeium :host github :repo "Exafunction/codeium.el"))
(startup-queue-package 'codeium 0)

(defvar company-mode)

(eval-after-load! codeium
  (general-def insert
    "<.> 0"
    (lambda ()
      (interactive)
      (let ((completion-at-point-functions '(codeium-completion-at-point)))
        ;; (when (featurep 'company)
        ;;   (when company-mode
        ;;     (company-abort)
        ;;     (setq company--cache (make-hash-table :test #'equal :size 10))))
        (completion-at-point))))

  ;; if you dont want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  ;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  (eval-after-load! alan-modeline
    (add-to-list 'alan-modeline-lhs '(-50 (:eval (modeline-maybe-add-space (car-safe codeium-mode-line)))) t))

  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
  (add-hook! 'kill-emacs-hook #'codeium-reset))

(provide 'alan-codeium)
