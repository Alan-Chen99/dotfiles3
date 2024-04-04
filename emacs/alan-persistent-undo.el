;; -*- lexical-binding: t -*-

(require 'alan-core)

(require 'undo-fu-session-autoloads)
(startup-queue-package 'undo-fu-session 80)

(autoload 'undo-fu-session--recover-safe "undo-fu-session")
(autoload 'undo-fu-session--save-safe "undo-fu-session")

(defvar persistent-undo-setup #'undo-fu-session--recover-safe)
(defvar persistent-undo-before-save #'undo-fu-session--save-safe)
(defvar persistent-undo-package 'undo-fu-session)
;; TODO: maybe i should use before-change-functions?
;; or first-change-hook
(add-hook! 'find-file-hook
  (defun persistent-undo-lazy-setup ()
    (setq-local undo-fu-session-mode t)
    (let*
        (
         (undo-setup persistent-undo-setup)
         (undo-before-save persistent-undo-before-save)
         (undo-package persistent-undo-package)
         (persistent-undo-setup-ran nil)
         (buf (current-buffer))
         (func              
          (lambda ()
            (unless persistent-undo-setup-ran
              (setq persistent-undo-setup-ran t)
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (funcall undo-setup)))))))
      (when undo-package
        (startup-queue-package undo-package 110)
        (with-eval-after-load undo-package
          (alan-startup-schedual-fn 110 func)))
      (add-hook-once! 'pre-command-hook :local
        (funcall func))
      (add-hook! 'before-save-hook :local
        (funcall undo-before-save)))))

(eval-after-load! undo-fu-session
  (undo-fu-session--directory-ensure))

(provide 'alan-persistent-undo)
