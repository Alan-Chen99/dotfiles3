;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-theme)

;; (pkg! 'unicode-fonts)

(defun alan-font-exist (name)
  (when (find-font (font-spec :family name))
    name))

;; ;; Iosevka's vertical metrics (ascent+descent) are ~15-20% taller than Hack's
;; ;; at the same point size. Rescale so fallback lines don't grow taller.
;; ;; Tune the value: M-: (query-font (face-attribute 'default :font)) then compare
;; ;; ascent+descent between Hack and Iosevka at the same :size.
;; (setq face-font-rescale-alist
;;       '(("Iosevka Nerd Font" . 0.85)))

(defun alan--apply-symbol-fontset (&optional frame)
  (let ((frame (or frame (selected-frame)))
        (fontset (face-attribute 'default :fontset frame)))
    (dolist (target '(symbol mathematical))
      ;; Iosevka first (higher coverage), then DejaVuSansM on top (matching height).
      ;; Lookup order: DejaVuSansM → Iosevka (rescaled) → default fallback.
      (set-fontset-font fontset target
                        (font-spec :family "Iosevka Nerd Font")
                        frame 'prepend)
      (set-fontset-font fontset target
                        (font-spec :family "DejaVuSansM Nerd Font")
                        frame 'prepend))))

(defvar alan-default-font-height (or (string-to-number (getenv "EMACS_FONT_HEIGHT")) 25))

;; (face-attribute 'default :font)
(defun alan--set-default-font-spec (family size)
  "Install FAMILY at SIZE as the default face's font override.

The font lives in `face-override-spec' (not `set-face-attribute') so
that `face-spec-recalc' keeps it after any of:

  - a theme is enabled and its `default' face-spec omits `:font'
    (e.g. `ef-owl' specifies only `:background'/`:foreground'), or
  - `face-set-after-frame-default' runs on a frame.

`face-set-after-frame-default' is invoked from
`Freconsider_frame_fonts' (frame.c) via
`dynamic-setting-handle-config-changed-event' when Emacs receives a
`config-changed-event' of type `font-render' or `monospace-font-name'.
`xsettings.c' fires those events when the X server's XSETTINGS
manager announces Xft/DPI/Antialias/Hinting/RGBA or GTK font-name
values.  Servers with an XSETTINGS owner (VcXsrv, Xming, a normal
desktop session) reliably trigger this shortly after Emacs connects.
xvfb has no XSETTINGS owner, so the event never fires and imperative
`:font' assignments appear to stick.

With `set-face-attribute … :font …', the attribute lives outside any
face-spec, so `face-spec-recalc' drops it and the default face falls
back to Emacs's built-in default (DejaVu Sans Mono 17 on this build).
With `face-override-spec', the font is part of the spec and is
reapplied on every recalc."
  (face-spec-set 'default
                 `((t :font ,(font-spec :family family :size size)))
                 'face-override-spec))

(defun alan-init-font-in-frame (frame)
  (with-selected-frame frame
    (let ((font (cond
                 ;; https://github.com/ryanoasis/nerd-fonts/discussions/1103
                 ;;x©x©xxxx
                 ;;xxxxxxxx
                 ;;x·✢*✻xxx ;; claude code progress blink
                 ;;⬝■ ;; opencode progress bar
                 ;;x󰊤x󰊤xxx
                 ;;x★x★xx
                 ;;x😀x😀xxxx
                 ((alan-font-exist "Hack Nerd Font Propo"))
                 ((alan-font-exist "Iosevka NFP"))
                 ((alan-font-exist "FiraCode NFP"))
                 ((alan-font-exist "Source Code Pro"))
                 ((alan-font-exist "DejaVu Sans Mono"))
                 ((alan-font-exist "Courier New"))
                 (t nil))))
      (when font
        (alan--set-default-font-spec font alan-default-font-height)
        (alan--apply-symbol-fontset frame)

        ;; seems to work even though doc of tooltip-frame-parameters claims otherwise
        ;; the "inherit" dont take effect since we only set font size for one frame
        ;; TODO: ig frame specific tooltip size is impossible?
        (setf (alist-get 'font tooltip-frame-parameters) (face-attribute 'default :font))))))

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
    (when-let* ((font (face-attribute 'default :font)))
      (font-get font :size))))

(defun alan-set-font-size (newsz &optional silent)
  (when (display-graphic-p)
    (alan--set-default-font-spec (face-attribute 'default :family) newsz)
    (alan--apply-symbol-fontset)

    ;; so that resizing also changes tooltip size
    (setf (alist-get 'font tooltip-frame-parameters) (face-attribute 'default :font))

    (unless silent
      (when-let*
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
