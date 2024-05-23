;; -*- lexical-binding: t -*-

(require 'alan-core)

(setq evil-jumps-cross-buffers nil)

(pkg! 'better-jumper
  (startup-queue-package 'better-jumper 70))


(eval-after-load! better-jumper
  (setq better-jumper-context 'buffer)

  (better-jumper-mode +1)

  (add-hook! 'find-file-hook
    (defun alan-find-file-set-jump ()
      (run-with-timer
       0 nil
       (callback-lambda ()
         (better-jumper-set-jump)))))

  (general-def
    [remap evil-jump-forward] #'better-jumper-jump-forward
    [remap evil-jump-backward] #'better-jumper-jump-backward))


(provide 'alan-jump)
