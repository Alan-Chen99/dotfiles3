;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(pkg! '(gptel :host github :repo "karthink/gptel" :protocol ssh
              ;; :remotes ("alan" :repo "Alan-Chen99/gptel" :branch "dev")
              )

  (defun gptn ()
    (interactive)
    (setq current-prefix-arg '(1))
    (call-interactively #'gptel)))


(autoload 'gptel "gptel")
(autoload 'gptel-menu "gptel-transient")

(defvar gpt-api-key)
(setq-default gpt-api-key "***************************************************")

(startup-queue-package 'gptel-transient 50)

(eval-after-load! gptel
  (setq gptel-api-key gpt-api-key)
  (general-def gptel-mode-map
    :states 'motion
    ;; "SPC h" #'gptel-system-prompt
    "SPC SPC"
    (lambda ()
      (interactive)
      (save-excursion
        (goto-char (point-max))
        (gptel-send)))
    "SPC g" #'gptel-menu)
  (defalias 'gpt #'gptel)
  ;; (require 'gptel-transient)
  (add-hook! 'gptel-mode-hook #'evil-normalize-keymaps)

  )

(provide 'alan-gpt)
