;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(popup
        :remotes ("alan" :repo "Alan-Chen99/popup-el" :tag "v2026-02-17")))

(eval-after-load! popup
  (general-def popup-menu-keymap
    "<escape>" 'popup-close))

;; TODO: overwrite popup-menu-fallback to execute the command

(provide 'alan-popup)
