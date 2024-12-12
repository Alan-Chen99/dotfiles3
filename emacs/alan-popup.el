;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(popup :host github :repo "Alan-Chen99/popup-el"))

(eval-after-load! popup
  (general-def popup-menu-keymap
    "<escape>" 'popup-close))

;; TODO: overwrite popup-menu-fallback to execute the command

(provide 'alan-popup)
