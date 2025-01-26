;; -*- lexical-binding: t -*-


(require 'alan-core)

(pkg! 'iflipb
  (startup-queue-package 'iflipb 50))

(require-if-is-bytecompile iflipb)

(defvar my-iflipb-buffer-list nil)
(defun my-command-non-iflipb-p (&optional command)
  (setq command (or command last-command))
  ;; originally this but that dont make sense?
  ;; (and (symbolp last-command)
  (or (not (symbolp command))
      (not
       (or
        (memq command
              '(consult-buffer
                switch-to-buffer
                alan-iflipb-kill-current-buffer
                alan-iflipb-pop))
        (string-match-p "^iflipb-" (symbol-name command))))))


(add-hook! 'pre-command-hook
  (defun pre-command-handle-iflipb ()
    (unless (or (minibufferp)
                (not
                 (or (my-command-non-iflipb-p last-command)
                     (my-command-non-iflipb-p this-command))))
      (let ((buf (current-buffer)))
        (unless (eq (car-safe my-iflipb-buffer-list) buf)
          (push (current-buffer) my-iflipb-buffer-list))))))

(defun my-iflipb-buffer-list-normalize ()
  (setq my-iflipb-buffer-list
        (-select #'buffer-live-p
                 (let ((-compare-fn #'eq))
                   (-distinct my-iflipb-buffer-list)))))

(defun alan-iflipb-kill-current-buffer ()
  (interactive)
  (kill-buffer)
  (if (iflipb-first-iflipb-buffer-switch-command)
      (setq last-command 'kill-buffer)
    (if (< iflipb-current-buffer-index (length (iflipb-interesting-buffers)))
        (iflipb-select-buffer iflipb-current-buffer-index)
      (iflipb-select-buffer (1- iflipb-current-buffer-index)))))

(defun alan-iflipb-pop ()
  (interactive)
  (setq my-iflipb-buffer-list (delq (current-buffer) my-iflipb-buffer-list))
  (if (< iflipb-current-buffer-index (length (iflipb-interesting-buffers)))
      (iflipb-select-buffer iflipb-current-buffer-index)
    (iflipb-select-buffer (1- iflipb-current-buffer-index))))

(eval-after-load! iflipb
  (advice-add #'iflipb-first-iflipb-buffer-switch-command
              :override #'my-command-non-iflipb-p)

  (advice-add #'iflipb-restore-buffers :override #'ignore)
  (defadvice! my-iflipb-interesting-buffers ()
    :override #'iflipb-interesting-buffers
    (--filter (or (eq it (current-buffer)) (not (get-buffer-window it)))
              (my-iflipb-buffer-list-normalize)))

  (general-def
    [remap previous-buffer] #'iflipb-previous-buffer
    [remap next-buffer] #'iflipb-next-buffer
    [remap alan-kill-current-buffer] 'alan-iflipb-kill-current-buffer)

  ;; (evil-set-command-property #'iflipb-previous-buffer :jump nil)
  ;; (evil-set-command-property #'iflipb-next-buffer :jump nil)
  ;; (evil-set-command-property #'iflipb-kill-buffer :jump nil)
  ;; (evil-set-command-property 'alan-iflipb-kill-current-buffer :jump nil)
  )

(provide 'alan-iflipb)
