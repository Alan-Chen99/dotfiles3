;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-theme)

;; (pkg! 'unicode-fonts)

(defun alan-font-exist (name)
  (when (find-font (font-spec :family name))
    name))

(defvar alan-default-font-height)
(setq alan-default-font-height 25)

;; (face-attribute 'default :font)
(defun alan-init-font-in-frame (frame)
  (with-selected-frame frame
    (let ((font (cond
                 ;; https://github.com/ryanoasis/nerd-fonts/discussions/1103
                 ;;x©x©xxxx
                 ;;xxxxxxxx
                 ;;x😀x😀xx
                 ;;x󰊤x󰊤xx
                 ((alan-font-exist "Hack Nerd Font Propo"))
                 ((alan-font-exist "Iosevka NFP"))
                 ((alan-font-exist "FiraCode NFP"))
                 ((alan-font-exist "Source Code Pro"))
                 ((alan-font-exist "DejaVu Sans Mono"))
                 ((alan-font-exist "Courier New"))
                 (t nil))))
      (when font
        (set-face-attribute 'default frame :font (font-spec :family font :size alan-default-font-height))))))
(alan-run-per-frame #'alan-init-font-in-frame)


;; (setq testfont (x-select-font))
(defun select-font ()
  (interactive)
  (let* ((font (x-select-font))
         (spec (if (fontp font) font (font-spec :name font)))
         (fam (symbol-name (font-get spec :family)))
         (entity (find-font (font-spec :family fam :weight 'regular :slant 'normal))))
    (set-face-attribute 'default (selected-frame) :font entity)
    (message "switching to font %S\nfrom %S" fam (aref (query-font (face-attribute 'default :font)) 1))
    (kill-new (prin1-to-string fam))))

(defun alan-get-font-size ()
  (when (display-graphic-p)
    (when-let ((font (face-attribute 'default :font)))
      (font-get font :size))))

(defun alan-set-font-size (newsz &optional silent)
  (when (display-graphic-p)
    (set-face-attribute 'default (selected-frame) :font (font-spec :size newsz))
    ;; seems to work even though doc of tooltip-frame-parameters claims otherwise
    ;; the "inherit" dont take effect since we only set font size for one frame
    ;; TODO: ig frame specific tooltip size is impossible?
    (setf (alist-get 'font tooltip-frame-parameters) (face-attribute 'default :font))

    (unless silent
      (when-let
          (
           (font (face-attribute 'default :font))
           (query (and (fontp font) (query-font font))))
        (let (message-log-max
              (sz (aref query 2))
              (height (+ (aref query 4) (aref query 5)))
              ;; TODO: isnt correct on windows
              (width (aref query 7)))
          (message "size %s height %s width %s ratio %.3f" sz height width (/ (float height) width)))))))


(defun alan-font-inc (&optional amt)
  (interactive "p")
  (alan-set-font-size (+ (alan-get-font-size) (or amt 1))))
(defun alan-font-dec (&optional amt)
  (interactive "p")
  (alan-set-font-size (- (alan-get-font-size) (or amt 1))))
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

;; (line-pixel-height)
;; (window-font-height)
;; (font-info (face-font 'default))
;; (display-pixel-height)


;; disable font and height settings for some builtin faces
;; TODO: do i actualy want this?
(face-spec-set 'fixed-pitch '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'variable-pitch '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'variable-pitch-text '((t :inherit default)) 'face-defface-spec)
(face-spec-set 'fixed-pitch-serif '((t :weight bold :inherit default)) 'face-defface-spec)


;; (symbol-plist 'tooltip)


(provide 'alan-font)
