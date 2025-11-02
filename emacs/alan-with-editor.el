;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'with-editor)

(eval-after-load! with-editor
  (alan-startup-schedual-fn -1000
    (lambda ()
      ;; delaying with timer makes it less likely to throw
      ;; (file-error "Cannot bind server socket" "Interrupted system call")
      (run-with-idle-timer
       0.5 nil
       (callback-lambda ()
         (setq process-environment (with-editor process-environment))))))

  (clear-and-backup-keymap with-editor-mode-map)

  (general-def with-editor-mode-map
    :states 'motion
    "SPC SPC" #'with-editor-finish
    [remap alan-kill-current-buffer] #'with-editor-cancel)

  ;; (general-def with-editor-mode-map
  ;;   "SPC SPC" #'with-editor-finish
  ;;   [remap alan-kill-current-buffer] #'with-editor-cancel)
  )


(provide 'alan-with-editor)
