;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-theme)


(defvar default-font
  (cond
   ((find-font (font-spec :name "DejaVu Sans Mono")) "DejaVu Sans Mono")
   ((find-font (font-spec :name "Courier New")) "Courier New")
   (t nil)))

(defvar alan-default-font-height)
(setq alan-default-font-height 19)
(defvar alan-current-font-height alan-default-font-height)

(defun alan-set-font-size (size)
  (let ((fontspec (if default-font (font-spec :name default-font :size size) (font-spec :size size))))
    (face-spec-set 'default `((t :font ,fontspec))))
  (when-let
      (
       (font (face-attribute 'default :font))
       (query (and (fontp font) (query-font font))))
    (message "size %s height %s width %s"
             (aref query 2) (+ (aref query 4) (aref query 5)) (aref query 7)))
  ;; (font-info (face-attribute 'default :font))
  ;; (message "%s . %s" (default-font-height) (default-font-width))
  ;; (aref (query-font (face-attribute 'default :font)) 3)
  (setq alan-current-font-height size)
  ;; (run-hooks 'after-load-theme-hook)
  )

(let ((message-log-max nil))
  (with-no-minibuffer-message
   (alan-set-font-size alan-current-font-height)))


(defun alan-font-inc (&optional amt)
  (interactive "p")
  (alan-set-font-size (+ alan-current-font-height (or amt 1))))
(defun alan-font-dec (&optional amt)
  (interactive "p")
  (alan-set-font-size (- alan-current-font-height (or amt 1))))
(defun alan-font-reset ()
  (interactive)
  (alan-set-font-size alan-default-font-height)
  (text-scale-increase 0))

;; https://stackoverflow.com/questions/14606037/advising-an-emacs-interactive-function-before
(defun alan-customize-face ()
  (interactive)
  (global-hl-line-mode 0)
  (show-paren-mode 0)
  (unwind-protect
      (call-interactively 'customize-face)
    (global-hl-line-mode 1)
    (show-paren-mode 1)))

;; (face-attribute 'default :font)

;; (font-spec :name "DejaVu Sans Mono" :size 23)
;; (line-pixel-height)
;; (window-font-height)
;; (font-info (face-font 'default))
;; (display-pixel-height)

;; (font-xlfd-name (find-font (font-spec :name "DejaVu Sans Mono" :size 12)))
;; (font-xlfd-name (font-spec :name "DejaVu Sans Mono" :size 12))
;; (font-xlfd-name (find-font (font-spec :name "DejaVu Sans Mono" :size 19)))
;; (font-xlfd-name (face-attribute 'default :font))
;; (let ((font (find-font (font-spec :name "DejaVu Sans Mono" :size 12))))
;;  (font-put font :size 19)
;;  (font-xlfd-name font)
;;  font)
;; (default-font-height)
;; (font-get (face-attribute 'default :font) :size)
;; (font-xlfd-name (face-attribute 'default :font))
;; (default-font-height)
;; (font-info (font-xlfd-name (face-attribute 'default :font)))
;; (font-get (face-attribute 'default :font) :pixel)
;; (font-get (face-attribute 'default :font) :height)


;; see function face-spec-set
;; (mapc
;;  (lambda (spec-type)
;;      (put face spec-type nil))
;;  '(face-defface-spec face-override-spec
;; (setq-default
;;  evil-search-wrap nil)
;; (set-face-attribute 'evil-ex-lazy-highlight nil
;;  :background 'unspecified :inherit 'font-lock-warning-face)
;;       customized-face saved-face))

;; (defun face-redef (face spec)
;;  (put face 'face-defface-spec spec))


;; disable font and height settings for some builtin faces
;; TODO: do i actualy want this?

(face-spec-set 'fixed-pitch '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'variable-pitch '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'variable-pitch-text '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'fixed-pitch-serif '((t :weight bold :inherit default)) 'face-defface-spec)



(provide 'alan-font)
