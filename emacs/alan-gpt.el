;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(pkg! '(gptel
        :repo "karthink/gptel"
        :remotes (("alan" :repo "Alan-Chen99/gptel")))
  (startup-queue-package 'gptel -10)
  (startup-queue-package 'gptel-context -10))

(require-if-is-bytecompile gptel gptel-transient gptel-anthropic gptel-context)

(autoload #'gptel-context-add "gptel-context")

(defvar gpt-api-key "sk-**************************************************************************")
(defvar anthropic-api-key "sk-ant-*****************************************************************************************************")
(defvar openrouter-api-key "sk-or-v1-****************************************************************")

(defun gpt (name)
  (interactive
   (progn
     (require 'gptel)
     (let* ((buffer-name "*gptel*"))
       (list (if current-prefix-arg
                 (read-buffer "Create or choose gptel buffer: "
                              (generate-new-buffer-name buffer-name) nil
                              (lambda (b)
                                (buffer-local-value 'gptel-mode
                                                    (get-buffer (or (car-safe b) b)))))
               buffer-name)))))
  (gptel name nil nil t))

(defun gptn ()
  (interactive)
  (setq current-prefix-arg '(1))
  (call-interactively #'gpt))

(startup-queue-package 'gptel-transient 50)

(defvar gptel--anthropic)

(eval-after-load! gptel
  (setq gptel-api-key gpt-api-key)
  (setq gptel--anthropic (gptel-make-anthropic "Claude" :stream t :key anthropic-api-key))

  ;; (setq gptel-backend gptel--openai)
  ;; (setq gptel-model 'gpt-5)

  (setq gptel-include-reasoning t)
  (setq gptel--system-message
        (format-message
         "You are a helpful assistant. Respond concisely.

Today is %s.
"
         (format-time-string "%Y/%m/%d")))

  (setq gptel-backend
        (gptel-make-openai "openrouter"
          :key openrouter-api-key
          :host "openrouter.ai"
          :endpoint "/api/v1/chat/completions"
          :stream t
          :models '(
                    gpt-5
                    gpt-5.1
                    gpt-5.2
                    gpt-5-mini
                    gpt-5-nano
                    gpt-5-chat
                    gpt-4.1
                    ;;
                    google/gemini-3-pro-preview
                    google/gemini-2.5-pro
                    deepseek/deepseek-r1-0528
                    deepseek/deepseek-v3.2-speciale
                    test-noexist
                    )
          :request-params
          '(
            :reasoning_effort "high"
            ;; :reasoning (:summary "detailed")
            )))

  (setq gptel-model 'google/gemini-3-pro-preview)

  (setq gptel-use-context 'user)

  ;; (setq gptel-backend gptel--anthropic)
  ;; (setq gptel-model 'claude-sonnet-4-20250514)

  (clear-and-backup-keymap gptel-mode-map)
  (general-def gptel-mode-map
    :states 'motion
    "SPC h" #'gptel-system-prompt
    "SPC SPC" #'alan-gptel-send
    "SPC m" #'alan-gptel-model
    "SPC a" #'gptel-abort
    "SPC g" #'gptel-menu
    "a a" #'gptel--suffix-context-buffer)

  (add-hook! 'gptel-mode-hook #'evil-normalize-keymaps))

(defun alan-gptel-send ()
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (gptel-send)))

(defun alan-gptel-model ()
  (interactive)
  ;; uses internal data format
  ;; this return from `transient-infix-read` is only supposed to be used by:
  ;; (cl-defmethod transient-infix-set ((obj gptel-provider-variable) value)
  (cl-destructuring-bind (backend model) (transient-infix-read #'gptel--infix-provider)
    (setq gptel-model model)
    (setq gptel-backend backend)))

(eval-after-load! gptel-context

  (clear-and-backup-keymap gptel-context-buffer-mode-map)
  (general-def gptel-context-buffer-mode-map
    :states 'motion
    "SPC SPC" #'gptel-context-confirm
    ;; "C-c C-k" #'gptel-context-quit
    "RET"     #'gptel-context-visit
    "<down>"       #'gptel-context-next
    "<up>"       #'gptel-context-previous
    "d"       #'gptel-context-flag-deletion
    )

  )

(provide 'alan-gpt)
