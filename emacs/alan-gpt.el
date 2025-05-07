;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(pkg! '(gptel :host github :repo "karthink/gptel"))

(require-if-is-bytecompile gptel gptel-transient gptel-anthropic)

(defvar gpt-api-key "sk-**************************************************************************")
(defvar anthropic-api-key "sk-ant-*****************************************************************************************************")

(defun gpt (name)
  (interactive
   (progn
     (require 'gptel)
     (let* ((backend (default-value 'gptel-backend))
            (backend-name
             (format "*%s*" (gptel-backend-name backend))))
       (list (if current-prefix-arg
                 (read-buffer "Create or choose gptel buffer: "
                              (generate-new-buffer-name backend-name) nil
                              (lambda (b)
                                (buffer-local-value 'gptel-mode
                                                    (get-buffer (or (car-safe b) b)))))
               backend-name)))))
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

  ;; (setq-default gptel-backend gptel--openai)

  (setq-default gptel-backend gptel--anthropic)
  (setq-default gptel-model "Claude:claude-3-7-sonnet-20250219")

  (clear-and-backup-keymap gptel-mode-map)
  (general-def gptel-mode-map
    :states 'motion
    "SPC h" #'gptel-system-prompt
    "SPC SPC" #'alan-gptel-send
    "SPC m" #'alan-gptel-model
    "SPC a" #'gptel-abort
    "SPC g" #'gptel-menu)

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
  (setq-local gptel-model (nth 1 (transient-infix-read #'gptel--infix-provider))))


(provide 'alan-gpt)
