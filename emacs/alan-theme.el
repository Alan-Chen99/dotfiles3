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
;; (setq theme-short-list nil)


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
  (add-to-list 'theme-short-list 'ef-day)
  (add-to-list 'theme-short-list 'ef-night)
  (add-to-list 'theme-short-list 'ef-winter)
  (add-to-list 'theme-short-list 'ef-frost)

  (startup-switch-theme 'ef-owl))

(defvar ef-themes-faces-overwrites)
(setq ef-themes-faces-overwrites
      '(
        ;; `(modeline-file-or-buffer-name ((,c :weight semibold)))
        ;; `(modeline-file-or-buffer-name ((,c)))

        ;; make `ef-themes-headings' take effect immediately
        `(ef-themes-heading-0 ((,c ,@(ef-themes--heading 0) :foreground ,rainbow-0)))
        `(ef-themes-heading-1 ((,c ,@(ef-themes--heading 1) :foreground ,rainbow-1)))
        `(ef-themes-heading-2 ((,c ,@(ef-themes--heading 2) :foreground ,rainbow-2)))
        `(ef-themes-heading-3 ((,c ,@(ef-themes--heading 3) :foreground ,rainbow-3)))
        `(ef-themes-heading-4 ((,c ,@(ef-themes--heading 4) :foreground ,rainbow-4)))
        `(ef-themes-heading-5 ((,c ,@(ef-themes--heading 5) :foreground ,rainbow-5)))
        `(ef-themes-heading-6 ((,c ,@(ef-themes--heading 6) :foreground ,rainbow-6)))
        `(ef-themes-heading-7 ((,c ,@(ef-themes--heading 7) :foreground ,rainbow-7)))
        `(ef-themes-heading-8 ((,c ,@(ef-themes--heading 8) :foreground ,rainbow-8)))


        `(mode-line-buffer-id ((,c :foreground ,keyword)))
        `(ansi-color-green ((,c :background ,bg-green-subtle :foreground ,fg-term-green-bright)))
        `(ansi-color-yellow ((,c :background ,bg-yellow-subtle :foreground ,fg-term-yellow-bright)))
        `(ansi-color-blue ((,c :background ,bg-blue-subtle :foreground ,fg-term-blue-bright)))
        `(markdown-code-face ((,c :inherit ef-themes-fixed-pitch :extend t)))))

(eval-after-load! ef-themes

  (setq ef-themes-headings
        '((0 1.5)
          (1 1.3)
          (2 1.1)
          ;; (3 1)
          ;; (agenda-date 1.3)
          ;; (agenda-structure variable-pitch light 1.8)
          ;; (t variable-pitch)
          ))


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
      ;; (when (memq theme ef-themes-dark-themes)
      ;;   (push
      ;;    '
      ;;    `(ansi-color-black ((,c :background ,bg-term-black :foreground ,fg-main)))
      ;;    ef-themes-faces))

      (eval `(ef-themes-theme ,theme ,sym-palette ,sym-override) 'lexical)))

  (dolist (theme ef-themes-all)
    (add-hook
     (intern (concat (symbol-name theme) "-theme-hook"))
     (lambda ()
       (with-no-warnings (ef-themes-do-override-one theme))))))



(pkg! 'doom-themes
  (add-to-list 'theme-short-list 'doom-peacock)
  (add-to-list 'theme-short-list 'doom-dracula))

(eval-after-load! doom-themes-base
  ;; note: this need to be before indivial doom themes to take effect
  (setf
   (alist-get 'ansi-color-green doom-themes-base-faces)
   '(:foreground green :background (doom-blend green bg 0.5))
   )

  )

;; (pkg! 'zenburn-theme)

;; (pkg! 'atom-one-dark-theme)

;; ;; (use-package solarized-theme)

;; ;; (use-package sublime-themes)

;; ;; (use-package github-theme)

;; ;; (use-package kaolin-themes)

(face-spec-set 'font-lock-operator-face '((t :weight bold)))
(face-spec-set 'font-lock-bracket-face '((t :weight bold)))

;; (setq-default face-near-same-color-threshold 70000)
;; (setq-default face-near-same-color-threshold 50000)
(setq-default face-near-same-color-threshold 30000)
;; (clear-face-cache)

(defun alan-map-spec-from-face-for (face fn)
  (declare (indent 1))
  ;; see `face-spec-choose' and `face-spec-recalc'
  (let ((default-spec (cadar (get 'default 'theme-face))))
    (mapcar
     (lambda (entry)
       (let* ((display (car entry))
	          (attrs (cdr entry))
              (default-attrs (alist-get display default-spec nil nil #'equal))
              ans-attrs
              key val)
	     (setq attrs (if (null (cdr attrs)) (car attrs) attrs))
	     (setq default-attrs (if (null (cdr default-attrs)) (car default-attrs) default-attrs))

         (setq ans-attrs (copy-sequence default-attrs))
         (while attrs
           (setq key (pop attrs))
           (setq val (pop attrs))
           (plist-put ans-attrs key val))
         (cons display (funcall fn ans-attrs))))
     (cadar (get face 'theme-face)))))

(add-hook! 'after-load-theme-hook
  (defun set-standard-faces-spec ()
    (span-notef "set-standard-faces-spec")

    ;; ensures that, as long as background is visible against default foreground, the text is visible
    (face-spec-set
     'default
     (alan-map-spec-from-face-for 'default
       (lambda (p)
         (unless (plist-get p :distant-foreground)
           `(:distant-foreground ,(plist-get p :foreground))))))

    ;; make vertical-border always same color
    (face-spec-set
     'vertical-border
     (alan-map-spec-from-face-for 'vertical-border
       (lambda (p)
         `(:distant-foreground ,(plist-get p :foreground)))))

    ))

(set-standard-faces-spec)


;; TODO: do this do anything
;; (add-hook! 'after-load-theme-hook :depth 100 #'clear-face-cache)


(provide 'alan-theme)
