;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)


;;; performance
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq bidi-paragraph-direction 'left-to-right)

(setq redisplay-skip-fontification-on-input t)
;; (setq max-redisplay-ticks 500000)
;; (setq max-redisplay-ticks 1000000)
(setq max-redisplay-ticks 2000000)
;; 1000000

(setq-default native-comp-speed 3)

(setq vc-handled-backends '(Git))

;;; logging stuff
;; Do not show the startup screen.
(setq inhibit-startup-message t)
(advice-add #'display-startup-echo-area-message :override #'ignore)

(setq garbage-collection-messages t)
(add-hook! 'alan-end-of-init-hook
  (setq warning-minimum-level :error))

(setq backtrace-on-redisplay-error t)
;; (setq inhibit-eval-during-redisplay nil)


;; disable alarm sound. TODO: is this a good idea?
(setq ring-bell-function 'ignore)


;;; editing

(setq disabled-command-function nil)

;; https://stackoverflow.com/questions/22024765/how-to-copy-paste-without-source-font-lock-in-emacs
;; yank-handled-properties
;; (add-to-list 'yank-excluded-properties 'face)
(setq yank-excluded-properties t)

;; UTF8_STRING goes before text/plain\;charset=utf-8 or else cannot paste into emacs in docker
(setq-default x-select-request-type '(UTF8_STRING text/plain\;charset=utf-8 COMPOUND_TEXT TEXT STRING))

;; TODO: this can hang, if the other prog is not responding
(setq save-interprogram-paste-before-kill 10000)

;; by default, don't do any indent
(setq-default indent-line-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default echo-keystrokes 0.01)

(setq-default backward-delete-char-untabify-method "all")

;; (modify-syntax-entry (string-to-char "-") "w" (standard-syntax-table))
(modify-syntax-entry (string-to-char "_") "w" (standard-syntax-table))

;; those do not account for minibuffer-history-variable being t
;; todo: upstream?
;; (alan-set-ignore-debug-on-error #'previous-matching-history-element)
;; (alan-set-ignore-debug-on-error #'next-matching-history-element)


;; Highlight matching parenthesis in normal mode
(setq-default show-paren-delay 0)
(show-paren-mode t)
;; (set-face-attribute
;;  'show-paren-match nil
;;  :underline 'unspecified
;;  :weight 'unspecified
;;  :inherit 'unspecified
;;  :foreground "red")

(defadvice! indent-region-single-undo (func &rest args)
  :around #'indent-region
  (with-undo-amalgamate
	(apply func args)))

;; https://stackoverflow.com/questions/17534750/emacs-not-displaying-unicode-on-reload
(prefer-coding-system 'utf-8-unix)
;; https://stackoverflow.com/questions/9760290/emacs-change-default-line-ending
;; (setq-default default-buffer-file-coding-system 'utf-8-unix)
;; (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; to get shell to work properly on windows
;; need to chnage cmd encoding to utf-8
;; https://stackoverflow.com/questions/57131654/using-utf-8-encoding-chcp-65001-in-command-prompt-windows-powershell-window

(setq
 vc-make-backup-files t
 make-backup-files t               ; backup of a file the first time it is saved.
 backup-by-copying t               ; don't clobber symlinks
 version-control t                 ; version numbers for backup files
 delete-old-versions t             ; delete excess backup files silently
 delete-by-moving-to-trash t
 kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 100               ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t               ; auto-save every buffer that visits a file
 auto-save-timeout 30              ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 100            ; number of keystrokes between auto-saves (default: 300)
 )

;; https://www.emacswiki.org/emacs/ForceBackups
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook #'force-backup-of-buffer)

;;; ui
;; on windows (window-width) reports 95 but should be 126
;; so these are modified
(setq split-width-threshold 80)
(setq truncate-partial-width-windows 40)

(setq line-number-display-limit-width 2000)

(setq use-dialog-box nil)

;; https://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
;; https://www.emacswiki.org/emacs/SmoothScrolling
(setq-default
 ;; TODO: this is obsolete why was this line here
 ;; redisplay-dont-pause t ;; might impact performace?
 scroll-margin 2
 scroll-step 1
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 )

;; (setq scroll-conservatively 10000)
(setq-default scroll-conservatively 0)
(setq-default scroll-up-aggressively 0.3)
(setq-default scroll-down-aggressively 0.0)

;; tooltip is ugly for wsl
(setq use-system-tooltips nil)

;;; other options

(setq-default use-short-answers t)

(setq confirm-kill-emacs #'y-or-n-p)

;; crashes when native compiling 'kill-buffer

;; ;; https://emacs.stackexchange.com/questions/22569/kill-buffer-with-y-or-n-p-instead-of-yes-or-no-p
;; ;; TODO: move this above
;; (defvar is-yes-or-no-p-inhibit nil)
;; (defadvice! yes-or-no-p-inhibit (orig-fun &rest args)
;;   :around 'yes-or-no-p
;;   (if is-yes-or-no-p-inhibit
;;       (apply #'y-or-n-p args)
;;     (apply orig-fun args)))

;; (defun yes-or-no-p->-y-or-n-p (orig-fun &rest r)
;;   (let ((is-yes-or-no-p-inhibit t))
;;     (apply orig-fun r)))

;; (advice-add #'kill-buffer :around #'yes-or-no-p->-y-or-n-p)
;; (advice-add #'save-buffers-kill-emacs :around #'yes-or-no-p->-y-or-n-p)
;; (advice-add #'revert-buffer :around #'yes-or-no-p->-y-or-n-p)


(defalias 'tde #'toggle-debug-on-error)


(clear-and-backup-keymap messages-buffer-mode-map)
(clear-and-backup-keymap special-mode-map)
(general-def special-mode-map
  "S-0" 'digit-argument
  "S-1" 'digit-argument
  "S-2" 'digit-argument
  "S-3" 'digit-argument
  "S-4" 'digit-argument
  "S-5" 'digit-argument
  "S-6" 'digit-argument
  "S-7" 'digit-argument
  "S-8" 'digit-argument
  "S-9" 'digit-argument)

(add-to-list 'evil-motion-state-modes 'special-mode)

(general-def messages-buffer-mode-map
  "SPC l"
  (lambda () (interactive)
    (cl-letf
        (
         ((symbol-value 'inhibit-read-only) t)
         ((get 'erase-buffer 'disabled) nil)
         )
      (erase-buffer))))


(pkg! 'page-break-lines
  (startup-queue-package 'page-break-lines 50))
(eval-after-load! page-break-lines
  (global-page-break-lines-mode))
;; TODO: the following activates in temp buffers
(define-advice page-break-lines-mode-maybe
    (:override () redefine)
  (page-break-lines-mode 1))

(pkg! 'evil-anzu
  (startup-queue-package 'evil-anzu 75))
(eval-after-load! anzu
  (setq anzu-cons-mode-line-p nil))
(eval-after-load! evil-anzu
  (global-anzu-mode +1))


;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
(startup-queue-package 'autorevert -10)
(eval-after-load! autorevert
  (global-auto-revert-mode t))


(startup-queue-package 'so-long -10)
(eval-after-load! so-long
  (global-so-long-mode 1))



(eval-after-load! help-mode
  (general-def help-mode-map
    :states 'motion

    "K" #'help-go-back
    "J" #'help-go-forward
    "SPC e" #'pp-eval-last-sexp)

  (add-hook 'help-mode-hook
            (lambda ()
              (setq-local xref-backend-functions '(elisp--xref-backend)))))

(eval-after-load! etags
  (remove-hook 'xref-backend-functions 'etags--xref-backend)
  (remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)
  (remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)
  ;; (alan-set-ignore-debug-on-error #'xref-backend-definitions)
  )

;; (defvar read-only-dir-list '("/nix"))
(defvar read-only-dir-list nil)
(defvar read-only-dir-exclude-list '())

(defvar emacs-share-dir)
(setq-default emacs-share-dir
              (expand-file-name
               "../../share/emacs"
               (file-truename (expand-file-name invocation-name invocation-directory))))

(defvar emacs-share-dir-truename)
(setq-default emacs-share-dir-truename (file-truename emacs-share-dir))

;; https://stackoverflow.com/questions/36087572/emacs-check-if-filepath-contains-directory-name
(defun is-in-dir (dir)
  (when (and dir (buffer-file-name))
    (string-prefix-p dir (expand-file-name (buffer-file-name)))))

;; https://dev.to/bravotan/how-to-set-up-emacs-to-open-read-only-depending-on-file-location-5g6m
(add-hook! 'find-file-hook
  (defun find-file-handle-readonly ()
    (when (-any? #'is-in-dir read-only-dir-list)
      (unless (-any? #'is-in-dir read-only-dir-exclude-list)
        (read-only-mode t)))))

(add-hook! 'find-file-hook
  (defun find-file-maybe-set-tab-width ()
    (let ((fname (file-truename (buffer-file-name))))
      (when (or (string-prefix-p emacs-share-dir-truename fname)
                (and (string-prefix-p source-directory fname)))
        (setq-local tab-width 8)))))

(add-to-list 'read-only-dir-list emacs-share-dir)
(add-to-list 'read-only-dir-list (expand-file-name user-emacs-directory))
(when (and source-directory (file-exists-p source-directory))
  (add-to-list 'read-only-dir-list source-directory))

(add-to-list 'read-only-dir-list (expand-file-name "~/.cargo"))


(provide 'alan-simple)
