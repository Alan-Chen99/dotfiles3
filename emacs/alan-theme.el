;; -*- lexical-binding: t -*-

(require 'alan-core)

;; https://www.reddit.com/r/emacs/comments/4v7tcj/does_emacs_have_a_hook_for_when_the_theme_changes/
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defvar alan-is-during-load-theme-adv nil)
(defun add-load-theme-hook-advice (func &rest args)
  (let ((alan-is-during-load-theme-adv t))
    (span (:load-theme (:seq args))
      (span-flush)
      (apply func args)))
  (unless alan-is-during-load-theme-adv
    (span-notef "[after-load-theme-hook]")
    (run-hooks 'after-load-theme-hook)))

;; TODO: this hook runs twice b/c enable-theme calls itself with (enable-theme 'user)
(advice-add #'enable-theme :around #'add-load-theme-hook-advice)
(advice-add #'disable-theme :around #'add-load-theme-hook-advice)
;; (add-load-theme-hook-advice 'enable-theme)
;; (add-load-theme-hook-advice 'disable-theme)
(advice-add #'enable-theme :before
            (lambda (theme)
              (let
                  ((hook (intern (concat (symbol-name theme) "-theme-hook"))))
                (when (boundp hook)
                  (run-hooks hook)))))

(defun transfer-face-attr (to from &rest attrs)
  (--map
   (set-face-attribute to nil it (face-attribute from it nil t))
   attrs))

;; (seq-filter (lambda (el) (not (eq (face-attribute el :height) 'unspecified))) (face-list))


;; TODO: use public api to do this?
;; (defun custom-reset-face (theme symbols)
;;  (let ((theme-settings (get theme 'theme-settings)) res)
;;      (dolist (theme-setting theme-settings)
;;          (if (and (eq (car  theme-setting) 'theme-face)
;;                  (memq (cadr theme-setting) symbols))
;;              (push theme-setting res)))
;;      (put theme 'theme-settings
;;          (--remove (memq it res) theme-settings))))

(defvar custom-push-theme-always-reset-enable nil)
(defadvice! custom-push-theme-always-reset (_prop symbol theme _mode &optional _value)
  :before #'custom-push-theme
  (when custom-push-theme-always-reset-enable
    (let ((theme-settings (get theme 'theme-settings)) res)
      (dolist (theme-setting theme-settings)
        (if (and (eq (car  theme-setting) 'theme-face)
                 (eq (cadr theme-setting) symbol))
            (push theme-setting res)))
      (put theme 'theme-settings
           (--remove (memq it res) theme-settings)))))

(defvar theme-short-list nil)
(setq theme-short-list nil)


(defun switch-theme-silent (theme)
  (with-no-minibuffer-message
   (mapc #'disable-theme custom-enabled-themes)
   (if (custom-theme-p theme)
       (enable-theme theme)
     (load-theme theme 'no-confirm))))

(defvar my-switch-theme-hist nil)
(defun alan-switch-theme (theme)
  (interactive
   (list
    (intern
     (completing-read "Theme: "
                      (mapcar #'symbol-name theme-short-list)
                      nil t nil 'my-switch-theme-hist))))
  (switch-theme-silent theme)
  (message "%s" theme))

(setq custom-safe-themes t)

(defun startup-switch-theme (theme)
  (let ((message-log-max nil))
    (unless custom-enabled-themes
      (switch-theme-silent theme)
      (push (symbol-name theme) my-switch-theme-hist))))

;; (package! spacemacs-common
;;   :elpaca '(spacemacs-theme :type git :host github :repo "lebensterben/spacemacs-theme")
;;   :defines (spacemacs-theme-comment-bg spacemacs-theme-comment-italic)
;;   :functions (true-color-p spacemacs-theme-custom-face)
;;   :init
;;   (setq theme-short-list (append '(spacemacs-light spacemacs-dark) theme-short-list))
;;   (defun spacemacs-theme-custom-face (variant theme-name)
;;     (let
;;         (
;;          (custom-push-theme-always-reset-enable t)
;;          (class '((class color) (min-colors 89)))
;;          (green         (if (eq variant 'dark) (if (true-color-p) "#67b11d" "#67b11d") (if (true-color-p) "#67b11d" "#5faf00")))
;;          ;; (green-bg      (if (eq variant 'dark) (if (true-color-p) "#293235" "#262626") (if (true-color-p) "#edf2e9" "#ffffff")))
;;          (green-bg-s    (if (eq variant 'dark) (if (true-color-p) "#29422d" "#262626") (if (true-color-p) "#dae6d0" "#ffffff")))
;;          (yellow        (if (eq variant 'dark) (if (true-color-p) "#b1951d" "#875f00") (if (true-color-p) "#b1951d" "#875f00")))
;;          (yellow-bg     (if (eq variant 'dark) (if (true-color-p) "#32322c" "#262626") (if (true-color-p) "#f6f1e1" "#ffffff")))
;;          )
;;       (custom-theme-set-faces theme-name
;;                               `(ansi-color-yellow ((,class (:background ,yellow-bg :foreground ,yellow))))
;;                               `(ansi-color-green ((,class (:background ,green-bg-s :foreground ,green)))))))
;;   :config
;;   (setq-default
;;    spacemacs-theme-comment-bg nil
;;    spacemacs-theme-comment-italic t
;;    spacemacs-theme-org-height nil)

;;   (add-hook! 'spacemacs-dark-theme-hook
;;     (spacemacs-theme-custom-face 'dark 'spacemacs-dark))
;;   (add-hook! 'spacemacs-light-theme-hook
;;     (spacemacs-theme-custom-face 'light 'spacemacs-light)))

(pkg! 'ef-themes
  (startup-switch-theme 'ef-winter))

(defvar ef-themes-faces-overwrites)
(setq ef-themes-faces-overwrites
      '(
        ;; `(modeline-file-or-buffer-name ((,c :weight semibold)))
        ;; `(modeline-file-or-buffer-name ((,c)))
        `(mode-line-buffer-id ((,c :foreground ,keyword)))
        `(ansi-color-green ((,c :background ,bg-green-subtle :foreground ,fg-term-green-bright)))
        `(ansi-color-yellow ((,c :background ,bg-yellow-subtle :foreground ,fg-term-yellow-bright)))
        `(markdown-code-face ((,c :inherit ef-themes-fixed-pitch :extend t)))))

(eval-after-load! ef-themes
  (defvar ef-themes-all)
  (setq ef-themes-all (append ef-themes-light-themes ef-themes-dark-themes))
  (defun ef-themes-do-override-one (theme)
    (let*
        (
         (name (symbol-name theme))
         (sym-palette (intern (concat name "-palette")))
         (sym-override (intern (concat name "-palette-overrides")))
         (custom-push-theme-always-reset-enable t)
         (ef-themes-custom-variables nil)
         (ef-themes-faces ef-themes-faces-overwrites))
      (eval `(ef-themes-theme ,theme ,sym-palette ,sym-override) 'lexical)))

  (dolist (theme ef-themes-all)
    (add-hook
     (intern (concat (symbol-name theme) "-theme-hook"))
     (lambda ()
       (with-no-warnings (ef-themes-do-override-one theme)))))

  (add-to-list 'theme-short-list 'ef-day)
  (add-to-list 'theme-short-list 'ef-night))


;; (package! doom-themes
;;   :init
;;   (setq theme-short-list (append '(doom-peacock doom-dracula) theme-short-list)))

;; (aio-with-async
;;   ;; (aio-await (promise! spacemacs-theme))
;;   (aio-await (promise! doom-themes))
;;   (let ((message-log-max nil))
;;     (unless custom-enabled-themes
;;       (let ((theme
;;              ;; 'spacemacs-dark
;;              ;; 'ef-light
;;              ;; 'doom-peacock
;;              'ef-day
;;              ))
;;         (switch-theme-silent theme)
;;         (push (symbol-name theme) my-switch-theme-hist))))
;;   (early-init-append-graphic-frame 'background-color (face-attribute 'default :background))
;;   (early-init-append-graphic-frame 'foreground-color (face-attribute 'default :foreground)))

;; ;; (use-package zenburn-theme)

;; ;; (use-package atom-one-dark-theme)

;; ;; (use-package doom-themes
;; ;;  :config
;; ;;  ;; Global settings (defaults)
;; ;;  ;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;; ;;  ;;  doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; ;;  ;; (load-theme 'doom-one t)

;; ;;  ;; ;; Enable flashing mode-line on errors
;; ;;  ;; (doom-themes-visual-bell-config)
;; ;;  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
;; ;;  ;; ;; (doom-themes-neotree-config)
;; ;;  ;; ;; or for treemacs users
;; ;;  ;; ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;; ;;  ;; ;; (doom-themes-treemacs-config)
;; ;;  ;; ;; Corrects (and improves) org-mode's native fontification.
;; ;;  ;; (doom-themes-org-config)
;; ;;  )

;; ;; (use-package solarized-theme)

;; ;; (use-package sublime-themes)

;; ;; (use-package github-theme)

;; ;; (use-package kaolin-themes)



(defun alan-face-ensure (face prop val)
  (when (eq (face-attribute face prop nil t) 'unspecified)
    (set-face-attribute face nil prop val)))


;; (setq-default face-near-same-color-threshold 70000)
(setq-default face-near-same-color-threshold 50000)
;; (setq-default face-near-same-color-threshold 30000)
;; (clear-face-cache)

(defun set-standard-faces ()
  ;; face-near-same-color-threshold

  ;; (round (/ (color-distance (face-attribute 'default :foreground nil t) (face-attribute 'default :background nil t)) 2))

  ;; (symbol-plist 'default)
  ;; (custom-theme-recalc-face 'default)

  ;; (face-attribute 'default :background nil t)

  ;; (face-attribute 'default :background nil t)
  ;; (face-attribute 'default :inherit nil t)
  ;; (ensure-distant-foreground 'default)


  ;; (color-distance (face-attribute 'modeline-file-or-buffer-name :foreground nil t) (face-attribute 'mode-line :background nil t))
  ;; (color-distance (face-attribute 'modeline-linenum :foreground nil t) (face-attribute 'mode-line :background nil t))
  ;; (color-distance (face-attribute 'default :foreground nil t) (face-attribute 'default :background nil t))



  ;; modeline-project-name
  ;; (face-attribute 'mode-line :distant-foreground nil t)


  ;; TODO: is this a good idea?
  ;; (setq-default face-near-same-color-threshold
  ;;               (min
  ;;                (round (/ (color-distance (face-attribute 'default :foreground nil t) (face-attribute 'default :background nil t)) 2))
  ;;                50000))

  ;; (color-distance (face-attribute 'default :foreground nil t) (face-attribute 'default :background nil t))



  ;; (color-distance (face-attribute 'modeline-project-name :foreground nil t) (face-attribute 'mode-line :background nil t))

  ;; (color-distance (face-attribute 'default :foreground nil t) (face-attribute 'default :background nil t))


  ;; (when (eq (face-attribute 'default :distant-foreground nil t) 'unspecified)
  ;;   (set-face-attribute 'default nil :distant-foreground (face-attribute 'default :background nil t)))



  ;; (face-attribute 'default :distant-foreground nil t)

  (set-face-attribute 'vertical-border nil :distant-foreground (face-attribute 'vertical-border :foreground nil t))


  )
(set-standard-faces)
(add-hook! 'after-load-theme-hook :depth 99 #'set-standard-faces)
;; TODO: do this do anything
(add-hook! 'after-load-theme-hook :depth 100 #'clear-face-cache)


(provide 'alan-theme)
