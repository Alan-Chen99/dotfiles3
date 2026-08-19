;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'with-editor
  (startup-queue-package 'with-editor 0))

(eval-after-load! server
  (setq server-log t))


(eval-after-load! with-editor
  (run-with-idle-timer
   ;; delaying with timer makes it less likely to throw
   ;; (file-error "Cannot bind server socket" "Interrupted system call")
   0.5 nil
   #'alan-setup-with-editor)

  (clear-and-backup-keymap with-editor-mode-map)

  (general-def with-editor-mode-map
    :states 'motion
    "SPC SPC" #'with-editor-finish
    [remap alan-kill-current-buffer] #'with-editor-cancel)

  ;; (general-def with-editor-mode-map
  ;;   "SPC SPC" #'with-editor-finish
  ;;   [remap alan-kill-current-buffer] #'with-editor-cancel)
  )

(defvar alan-did-setup-with-editor nil)

(defun alan-setup-with-editor ()
  (require 'with-editor)
  (unless alan-did-setup-with-editor
    (span :with-editor-setup
      (with-temp-buffer
        (span-dbgf (getenv "EDITOR"))
        (setq alan-did-setup-with-editor t)
        (unwind-protect
            (setq process-environment (with-editor process-environment))
          (span-dbgf (getenv "EDITOR")))))))


(provide 'alan-with-editor)
