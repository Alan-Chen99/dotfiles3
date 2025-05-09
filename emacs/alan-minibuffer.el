;; -*- lexical-binding: t -*-

(require 'evil)

(require 'alan-core)

(setq enable-recursive-minibuffers t)
(setq evil-want-minibuffer t)
(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq backward-delete-char-untabify-method "all")

;; without this cursor is wrong
;; TODO: why?
(add-hook! 'minibuffer-setup-hook #'evil-insert-state)

(add-hook! 'minibuffer-setup-hook :depth 100 #'evil-normalize-keymaps)
;; (add-hook! 'minibuffer-mode-hook #'evil-normalize-keymaps)

;; (frame-parameter nil 'minibuffer-height)

;; (defvar minibuffer-height 3.0)
(setq resize-mini-windows nil)

;; (setq max-mini-window-height 1)
;; (with-selected-window (minibuffer-window) (window-font-height))
;; (with-selected-window (minibuffer-window) (line-pixel-height))
;; (setq-default line-spacing 0)

(defvar resize-minibuffer-hook nil)
(defun resize-minibuffer (&rest _)
  (with-selected-window (minibuffer-window)
    (if track-mouse
        (setf (frame-parameter nil 'minibuffer-height) (window-pixel-height))
      (let ((minibuffer-height (frame-parameter nil 'minibuffer-height)))
        (window-resize
         (minibuffer-window)
         (-
          (if (integerp minibuffer-height)
              minibuffer-height
            (1+ (round (* minibuffer-height (line-pixel-height)))))
          (window-pixel-height))
         nil nil 'pixelwise))
      (setq max-mini-window-height (window-height))
      (run-hooks 'resize-minibuffer-hook))))

(defun alan-init-minibuffer-size (frame)
  (with-selected-frame frame
    (setf (frame-parameter nil 'minibuffer-height) 3.0)
    (resize-minibuffer)))

(alan-run-per-frame #'alan-init-minibuffer-size)

;; https://emacs.stackexchange.com/questions/45507/which-hooks-guarantee-proper-redisplaying-of-windows-after-windows-configurati
;; TODO: where does default resize kick in?
(defun resize-minibuffer-unless-in-minibuffer (&rest _)
  (unless (window-minibuffer-p)
    (resize-minibuffer)))
(add-hook 'window-size-change-functions #'resize-minibuffer-unless-in-minibuffer)
;; (setq-default read-minibuffer-restore-windows t)
;; not neccessary
;; (add-hook 'minibuffer-exit-hook 'resize-minibuffer)

(evil-define-command alan-minibuffer-resize-count (count)
  (interactive "<c>")
  (setf (frame-parameter nil 'minibuffer-height) (float (if count count 3)))
  (resize-minibuffer))
;; (evil-define-command alan-minibuffer-resize (arg)
;;   (interactive "<a>")
;;   (setq minibuffer-height (float (if arg (string-to-number arg) 3)))
;;   (resize-minibuffer))



;; https://www.reddit.com/r/emacs/comments/re31i6/how_to_go_up_one_directory_when_using_findfile_cx/
(defun alan-up-directory (arg)
  "Move up a directory (delete backwards to /)."
  (interactive "p")
  (let (kill-ring kill-ring-yank-pointer this-command)
    (if (string-match-p "/." (minibuffer-contents))
        (zap-up-to-char (- arg) ?/)
      (delete-minibuffer-contents))))


(general-def minibuffer-local-filename-completion-map
  :states 'insert
  [remap left-char] #'alan-up-directory)

;; https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html
(setq completions-format 'one-column)
;; (setq completions-header-format nil)
(setq completions-max-height 15)
(setq completion-auto-select nil)


(startup-queue-package 'mb-depth 0)
(add-hook-once! 'minibuffer-setup-hook (require 'mb-depth))


(eval-after-load! mb-depth
  (minibuffer-depth-indicate-mode)
  (defadvice! minibuffer-depth-setup-fix-ov (&rest _args)
    :after 'minibuffer-depth-setup
    (when (overlayp minibuffer-depth-overlay)
      (overlay-put minibuffer-depth-overlay 'evaporate nil)
      ;; between vertico--count-ov and vertico--candidates-ov
      (overlay-put minibuffer-depth-overlay 'priority 30)
      (move-overlay minibuffer-depth-overlay (point-min) (point-min)))))



(provide 'alan-minibuffer)
