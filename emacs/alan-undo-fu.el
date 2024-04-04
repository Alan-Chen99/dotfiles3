;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(require 'undo-fu-autoloads)

(startup-queue-package 'undo-fu 80)

(add-hook-once! 'pre-command-hook
  (require 'undo-fu))

(eval-after-load! undo-fu
  (evil-set-undo-system 'undo-fu))


(provide 'alan-undo-fu)
