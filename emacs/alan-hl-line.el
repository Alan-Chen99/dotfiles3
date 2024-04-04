;; -*- lexical-binding: t -*-

(require 'alan-core)

(startup-queue-package 'hl-line 100)

(defvar last-command-buffer nil)
(defvar last-command-point nil)
(add-hook! 'post-command-hook
  (defun post-command-record-point ()
    (setq last-command-buffer (current-buffer))
    (when (markerp last-command-point)
      (set-marker last-command-point nil))
    (setq last-command-point (point-marker))
    (set-marker-insertion-type last-command-point t)))

(eval-after-load! hl-line
  (defun global-hl-line-highlight-if-right-buffer (&rest _)
    (with-demoted-errors "error in global-hl-line-highlight-if-right-buffer: %S"
      (when (eq last-command-buffer (current-buffer))
        (save-excursion
          (goto-char last-command-point)
          (global-hl-line-highlight)))))
  (add-hook! 'global-hl-line-mode-hook
    (if global-hl-line-mode
        (add-hook 'after-change-functions 'global-hl-line-highlight-if-right-buffer)
      (remove-hook 'after-change-functions 'global-hl-line-highlight-if-right-buffer)))
  (global-hl-line-mode))


(provide 'alan-hl-line)
