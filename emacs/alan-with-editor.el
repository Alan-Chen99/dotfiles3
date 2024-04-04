;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'with-editor)

(eval-after-load! with-editor
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
