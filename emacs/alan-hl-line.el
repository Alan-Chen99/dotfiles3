;; -*- lexical-binding: t -*-

(require 'alan-core)

(startup-queue-package 'hl-line 100)

;; (defvar last-command-buffer nil)
;; (defvar last-command-point nil)
;; (add-hook! 'post-command-hook
;;   (defun post-command-record-point ()
;;     (setq last-command-buffer (current-buffer))
;;     (when (markerp last-command-point)
;;       (set-marker last-command-point nil))
;;     (setq last-command-point (point-marker))
;;     (set-marker-insertion-type last-command-point t)))

(defvar alan-hl-line-timer nil)

(eval-after-load! hl-line

  ;; ;; (span-instrument global-hl-line-highlight)
  ;; (defadvice! alan-global-hl-line-maybe-cancel-timer (&rest _)
  ;;   :before #'global-hl-line-highlight
  ;;   (when (timerp alan-hl-line-timer)
  ;;     (let ((timer alan-hl-line-timer))
  ;;       (setq alan-hl-line-timer nil)
  ;;       (cancel-timer timer))))

  ;; (defun alan-hl-line-highlight-after-change (&rest _)
  ;;   (unless alan-hl-line-timer
  ;;     (setq alan-hl-line-timer
  ;;           (run-with-timer
  ;;            0 nil
  ;;            (lambda ()
  ;;              (setq alan-hl-line-timer nil)
  ;;              (let ((alan-hl-line-timer t))
  ;;                (global-hl-line-highlight)))))))

  ;; (add-hook! 'global-hl-line-mode-hook
  ;;   (if global-hl-line-mode
  ;;       (add-hook 'after-change-functions 'alan-hl-line-highlight-after-change)
  ;;     (remove-hook 'after-change-functions 'alan-hl-line-highlight-after-change)))
  (global-hl-line-mode))


(provide 'alan-hl-line)
