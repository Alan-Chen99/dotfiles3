;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)
(setq font-log t)
;; (length font-log)

(set-buffer (get-buffer-create " *initialization*"))

(defvar alan-dotemacs-dir nil)

(require 'alan-utils)

(span-notef "[early init] %s" alan-dotemacs-dir)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(background-mode . dark))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; TODO: this doenst work when called after early init?
(span-dbgf (frame-parameters))

;; (add-hook! 'pre-redisplay-functions
;;   (span-notef "pre-redisplay-functions"))

;; (when-let
;;     (
;;      (font-obj (face-attribute 'default :font))
;;      (font-name (and (fontp font-obj) (font-xlfd-name font-obj))))
;;   font-name)

;; https://www.reddit.com/r/emacs/comments/osscfd/comment/h6ttuoq/
;; (pgtk-use-im-context nil)
(setq-default pgtk-use-im-context-on-new-connection nil)
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
(pkg! '(format-all :repo "Alan-Chen99/emacs-format-all-the-code"))
(pkg! 'hydra)
(pkg! 'compat)

;; todo: builtin seq is too old, how to deal with this?
;; note: seq is already loaded, should we reload it?
(pkg! '(seq :build (:not elpaca--activate-package)))

(when (= (length elpaca--queues) 1)
  (let ((q (car elpaca--queues)))
    (elpaca-process-queues)
    (when (eq (elpaca-q<-status q) 'complete)
      (span-notef "alan-start from early init")
      (require 'alan-start))))

(add-hook! 'emacs-startup-hook
  (defun alan-emacs-startup-hook ()
    (span-notef "%s" "[emacs-startup-hook]")
    (let ((message-log-max nil))
      (message "Emacs ready in %s with %d garbage collections."
               (format "%.3f seconds" (float-time (time-subtract (current-time) before-init-time)))
               gcs-done))))


(span-dbgf (face-attributes-as-vector 'default))
(span-dbgf (frame-parameters))


(ignore-errors
  (span :set-startup-frame-size
    (span-dbgf (ignore-errors (x-display-pixel-width)))
    (span-dbgf (ignore-errors (x-display-pixel-height)))

    ;; (display-monitor-attributes-list) can be used after creating frame

    (when (fboundp 'w32-display-monitor-attributes-list)
      (span-dbgf (ignore-errors (w32-display-monitor-attributes-list))))
    (when (fboundp 'pgtk-display-monitor-attributes-list)
      (span-dbgf (ignore-errors (pgtk-display-monitor-attributes-list))))

    ;; this dictates size/position after un-maximizing
    (add-to-list 'default-frame-alist `(left + -8))
    (add-to-list 'default-frame-alist `(top . 0))

    ;; values from windows
    ;; (- (/ (x-display-pixel-width) 2) (frame-text-width))
    (add-to-list 'default-frame-alist `(width text-pixels . ,(- (/ (x-display-pixel-width) 2) 18)))
    ;; (- (x-display-pixel-height) (frame-text-height))
    (add-to-list 'default-frame-alist `(height text-pixels . ,(- (x-display-pixel-height) 99)))

    (span-notef "done")))

(defun alan-maybe-set-frame-shape (frame)
  (when (eq (window-system frame) 'pgtk)
    ;; this sets size after un-maxmizing on wsl
    ;; it seems that only setting both width and height works
    (span :alan-maybe-set-frame-shape
      (with-selected-frame frame
        (span-dbgf (frame-parameters))
        (span-dbgf (frame-native-height))
        (span-dbgf (frame-geometry))

        (span-dbgf (frame-parameter frame 'fullscreen))
        (set-frame-width nil
                         (- (/ (x-display-pixel-width) 2) (- (frame-native-width) (frame-text-width)))
                         nil 'pixelwise)
        ;; (- (x-display-pixel-height) (frame-native-height))
        (set-frame-height nil
                          (- (x-display-pixel-height) 79)
                          nil 'pixelwise)))))

(add-hook 'after-make-frame-functions #'alan-maybe-set-frame-shape)


(add-hook 'after-make-frame-functions
          (lambda (frame)
            (span-msg "after-make-frame-functions: %s" frame)))

(setq alan-finished-early-init t)

(provide 'alan-early-init)
