;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(set-buffer (get-buffer-create " *initialization*"))

(defvar alan-dotemacs-dir)
(when load-file-name
  (setq alan-dotemacs-dir (file-name-directory (file-chase-links load-file-name))))

(add-to-list 'load-path alan-dotemacs-dir)

(require 'alan-utils)

(span-notef "[early init] %s" alan-dotemacs-dir)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; TODO: this doenst work when called after early init?
(defvar alan-real-early-init nil)
(when (and alan-real-early-init (display-graphic-p))
  ;; (face-attribute 'default :background)
  (add-to-list 'default-frame-alist '(background-color . "#000e17"))
  ;; (face-attribute 'default :foreground)
  (add-to-list 'default-frame-alist '(foreground-color . "#afbcbf")))


;; (when-let
;;     (
;;      (font-obj (face-attribute 'default :font))
;;      (font-name (and (fontp font-obj) (font-xlfd-name font-obj))))
;;   font-name)

;; https://www.reddit.com/r/emacs/comments/osscfd/comment/h6ttuoq/
;; (pgtk-use-im-context nil)
(setq pgtk-use-im-context-on-new-connection nil)
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

(require 'alan-config)

(require 'alan-elpaca)

(pkg! 'evil)
(pkg! 'general)
(pkg! 'dash)
(pkg! 'evil-visualstar)
(pkg! 'undo-fu)
(pkg! 'undo-fu-session)
(pkg! 'format-all)
(pkg! 'hydra)


(unless (display-graphic-p)
  (pkg! 'evil-terminal-cursor-changer
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    ;; (setq evil-motion-state-cursor 'box)  ; █
    ;; (setq evil-visual-state-cursor 'box)  ; █
    ;; (setq evil-normal-state-cursor 'box)  ; █
    ;; (setq evil-insert-state-cursor 'bar)  ; ⎸
    ;; (setq evil-emacs-state-cursor  'hbar) ; _

    ;; TODO: find out why (getenv "TERM") returns "dumb"
    (setq etcc-term-type-override 'xterm)

    ;; TODO: maybe i should do this after each evil cursor change?
    (add-hook! 'after-load-theme-hook
      (etcc--evil-set-cursor-color (frame-parameter nil 'cursor-color)))

    ;; TODO: to reset cursor after exit emacs,
    ;; maybe i should add a cursor setting to prompt as someone said here
    ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/12
    ))

;; todo: builtin seq is too old, how to deal with this?
;; note: seq is already loaded, should we reload it?
(pkg! '(seq :build (:not elpaca--activate-package)))

(when (= (length elpaca--queues) 1)
  (let ((q (car elpaca--queues)))
    (elpaca-process-queues)
    (when (eq (elpaca-q<-status q) 'complete)
      ;; (span-notef "alan-start from early init")
      (require 'alan-start))))

(add-hook! 'emacs-startup-hook
  (defun alan-emacs-startup-hook ()
    (span-notef "%s" "[emacs-startup-hook]")
    (let ((message-log-max nil))
      (message "Emacs ready in %s with %d garbage collections."
               (format "%.3f seconds" (float-time (time-subtract (current-time) before-init-time)))
               gcs-done))))

(setq alan-finished-early-init t)

(provide 'alan-early-init)
