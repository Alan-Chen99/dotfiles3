;; -*- lexical-binding: t -*-

(require 'alan-core)

(autoload 'vundo "vundo")

(pkg! 'vundo
  (general-def
    :states 'motion
    "g t" #'vundo))

;; TODO: vundo--draw-tree is very slow as it calls current-column many times

(eval-after-load! vundo

  (define-derived-mode vundo-mode special-mode
    ;; replaces the default one, it does too much stuff
    (setq-local
     truncate-lines t
     cursor-in-non-selected-windows nil
     enable-cursor nil))

  (setq-default
   vundo-compact-display t
   vundo-glyph-alist vundo-unicode-symbols
   vundo-window-max-height 10)

  (general-def vundo-mode-map
    :states 'motion
    [remap backward-char] #'vundo-backward
    [remap forward-char] #'vundo-forward
    [remap previous-line] #'vundo-previous
    [remap next-line] #'vundo-next
    "<escape>" #'vundo-quit
    [remap evil-backward-word-begin] #'vundo-stem-root
    [remap evil-forward-word-end] #'vundo-stem-end
    [remap save-buffer] #'vundo-save))

(provide 'alan-vundo)
