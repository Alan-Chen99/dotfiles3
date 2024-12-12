;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(pkg! '(gptel :host github :repo "karthink/gptel"))

(autoload 'gptel "gptel")
(autoload 'gptel-menu "gptel-transient")
(autoload 'gptel-backend-name "gptel-openai")
(autoload 'gptel-abort "gptel")

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

(defvar gpt-api-key)
;; (setq-default gpt-api-key "********************************************************")
(setq-default gpt-api-key "*****************************************************************************")


(startup-queue-package 'gptel-transient 50)

(eval-after-load! gptel
  (setq gptel-api-key gpt-api-key)
  (setq gptel-model "gpt-4-turbo")
  (general-def gptel-mode-map
    :states 'motion
    ;; "SPC h" #'gptel-system-prompt
    "SPC SPC"
    (lambda ()
      (interactive)
      (save-excursion
        (goto-char (point-max))
        (gptel-send)))

    ;; "SPC m" #'gptel-model
    "SPC a" #'gptel-abort
    "SPC g" #'gptel-menu)

  ;; (require 'gptel-transient)
  (add-hook! 'gptel-mode-hook #'evil-normalize-keymaps)

  ;; (span-quickwrap gptel-file-handler)
  )

(provide 'alan-gpt)
