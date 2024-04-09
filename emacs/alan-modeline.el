;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-theme)
(require 'evil)

;; xdisp.c > decode_mode_spec

(defvar alan-modeline-lhs "%b %f")
(defvar alan-modeline-rhs 'mode-line-modes)

(put 'alan-modeline-lhs 'risky-local-variable t)
(put 'alan-modeline-rhs 'risky-local-variable t)

(defun alan-format-modeline ()
  (span :alan-format-modeline
    ;; (span-msg "%s" (current-buffer))
    ;; (span--backtrace)
    (let* ((lhs (format-mode-line alan-modeline-lhs))
           (rhs (format-mode-line alan-modeline-rhs))
           (rhs-width (string-width rhs)))
      (concat
       lhs
       (propertize
        " "
        'display
        `(space
          :align-to
          (- (+ right right-fringe right-margin scroll-bar)
             ,rhs-width)))
       rhs))))


(defvar-local alan-formatted-modeline nil)
(put 'alan-formatted-modeline 'risky-local-variable t)

;; this is so that we dont do precent replacement again
(defun alan-format-modeline-wrapper ()
  (setq-local alan-formatted-modeline (alan-format-modeline))
  nil)

(setq-default mode-line-format
              `(
                ;; whitespace to pad to first column
                ,(let* ((text " "))
                   (put-text-property 0 1 'display '(space :align-to 0) text)
                   text)
                ;; eldoc want its thing at the top level
                (eldoc-mode-line-string ("" eldoc-mode-line-string " "))
                (:eval (alan-format-modeline-wrapper))
                alan-formatted-modeline))

(mapc
 (lambda (buf)
   (with-current-buffer buf
     (kill-local-variable 'mode-line-format)))
 (buffer-list))





;; (defvar modeline-major-mode)
;; ;; (format-mode-line '(:propertize (:eval (format-mode-line mode-name)) face modeline-mode))

;; ;; (format-mode-line '(:eval (modeline-get-major-mode)))
;; ;; (substring (format-mode-line 'modeline-major-mode) 5 7)
;; (setq modeline-major-mode
;;       `(:propertize mode-name
;;                     face modeline-mode
;;                     mouse-face mode-line-highlight
;;                     local-map ,mode-line-major-mode-keymap)
;;       )
;; (put 'modeline-major-mode 'risky-local-variable t)


;; (setq alan-modeline modeline-major-mode)

(defmacro modeline-defface (face &rest body)
  `(defface ,face
     '((t ,@body))
     ""
     :group 'modeline-faces))

(modeline-defface modeline-linenum :weight bold)

(modeline-defface modeline-project-name)
(modeline-defface modeline-file-path)
(modeline-defface modeline-file-or-buffer-name)

(modeline-defface modeline-unsaved :weight extra-bold)
(modeline-defface modeline-readonly :weight extra-bold)

(modeline-defface modeline-evil :weight bold)

(modeline-defface modeline-mode :weight bold)

(modeline-defface modeline-vcs-icon :weight extra-bold)
(modeline-defface modeline-vcs-text)

(modeline-defface modeline-selection-info :weight bold)


(face-spec-set 'mode-line '((t :box unspecified :foreground unspecified)))
(face-spec-set 'mode-line-active '((t :box unspecified :foreground unspecified)))
(face-spec-set 'mode-line-inactive '((t :box unspecified :foreground unspecified)))

(add-hook! 'after-load-theme-hook
  (defun modeline-set-faces-spec ()
    ;; we want to set a :distant-foreground for mode-line
    ;; in case any is too close to :background of mode-line
    ;; whatever color the buffer name is for the default modeline will surely work
    ;; so we use that
    ;; this need to go before the direct attribute settings below
    (face-spec-set
     'mode-line
     (alan-map-spec-from-face-for 'mode-line-buffer-id
       (lambda (p)
         (unless (plist-get p :distant-foreground)
           (when (plist-get p :foreground)
             `(:distant-foreground ,(plist-get p :foreground)))))))))

(modeline-set-faces-spec)


(add-hook! 'after-load-theme-hook :depth 50
  (defun modeline-set-faces ()
    (unless alan-real-early-init
      (span-notef "modeline-set-faces")

      (transfer-face-attr 'modeline-linenum 'font-lock-keyword-face :foreground)

      (transfer-face-attr 'modeline-project-name 'font-lock-string-face :foreground)
      (transfer-face-attr 'modeline-file-or-buffer-name 'mode-line-buffer-id :foreground)

      (transfer-face-attr 'modeline-unsaved 'font-lock-warning-face :foreground)
      (transfer-face-attr 'modeline-readonly 'font-lock-doc-face :foreground)

      (transfer-face-attr 'modeline-mode 'font-lock-type-face :foreground)

      (transfer-face-attr 'modeline-selection-info 'font-lock-string-face :foreground)

      (transfer-face-attr 'modeline-vcs-icon 'font-lock-doc-face :foreground)
      (transfer-face-attr 'modeline-vcs-text 'font-lock-doc-face :foreground))))

(modeline-set-faces)
(add-hook! 'alan-start-of-init-hook #'modeline-set-faces)


(defun modeline-calc-modified ()
  (cond
   ;; TODO: handle special files
   ((and buffer-file-name
         (not (find-file-name-handler buffer-file-name 'file-exists-p))
         (not (file-exists-p buffer-file-name)))
    (propertize "!" 'face 'modeline-unsaved))

   ((and buffer-file-name (buffer-modified-p))
    (propertize "*" 'face 'modeline-unsaved))
   (buffer-read-only
    (propertize "%%" 'face 'modeline-readonly))
   (t " ")))


(autoload 'project-root "project")

(defun alan-modeline-filepath-fast (path)
  (cond
   ((not (find-file-name-handler path t))
    (let ((dirname (file-name-directory path))
          (filename (file-name-nondirectory path)))
      (concat dirname
              (propertize filename 'face 'modeline-file-or-buffer-name))))
   (t
    (propertize path 'face 'modeline-file-or-buffer-name))))

;; (alan-modeline-filepath-slow-impl "/ssh:x@y.z:/home/")
;; (alan-modeline-filepath-slow-impl "/home/a/b.c")
;; (alan-modeline-filepath-slow-impl "/home/alan/emacs_new/abc.d")

(defun alan-modeline-filepath-slow-impl (path-ipt)
  (let* ((path (expand-file-name path-ipt))
         (dirname (file-name-directory path))
         (filename (file-name-nondirectory path))

         (dirname-local (file-local-name dirname))
         (remote (file-remote-p dirname))

         (project-root
          (unless remote
            (when-let ((project (project-current nil dirname)))
              (file-local-name
               (expand-file-name
                (project-root project))))))

         (ans (list (propertize filename 'face 'modeline-file-or-buffer-name)))
         relative-path)

    (if project-root
        (progn
          (setq relative-path (file-relative-name dirname-local project-root))
          (when (string= relative-path "./")
            (setq relative-path ""))
          (push relative-path ans)

          (push
           (propertize
            (concat
             (file-name-nondirectory (directory-file-name project-root)) "/")
            'face 'modeline-project-name)
           ans))

      (push (abbreviate-file-name dirname-local) ans))

    (when remote
      (push remote ans))

    (apply #'concat ans)))



(defun alan-modeline-filepath-slow (path)
  (let (success ans)
    (force-noerr
     (span (:alan-modeline-filepath-slow "%S" path)
       ;; (span-flush)
       (setq ans (alan-modeline-filepath-slow-impl path)))
     (setq success t))
    (if success
        ans
      (concat "[error]" (alan-modeline-filepath-fast path)))))



(defvar-local alan-prev-buffer-path nil)

;; sepcial mode handled here b/c special-mode-hook dont always run, for ex *messages* buffer, or an error occured somewhere, or if querying buffer local vars
(defvar-local alan-use-short-mode-line-filename nil)


(defvar-local alan-modeline-filename nil)
(defvar-local alan-modeline-buffername nil)
(put 'alan-modeline-filename 'risky-local-variable t)
(put 'alan-modeline-buffername 'risky-local-variable t)

(defvar-local alan-update-modeline-filepath-timer nil)

(defun alan-set-modeline-filepath (slow)
  (let ((buf-path (or buffer-file-name default-directory)))
    (unless (and (not slow)
                 (or (eq buf-path alan-prev-buffer-path)
                     (and buf-path alan-prev-buffer-path
                          (string= buf-path alan-prev-buffer-path))))
      (setq-local alan-prev-buffer-path buf-path)
      (let* ((use-short
              (if alan-use-short-mode-line-filename
                  (eq alan-use-short-mode-line-filename :yes)
                (and (derived-mode-p 'special-mode)
                     (not buffer-file-name)))))
        (if (or use-short (not buf-path))
            (setq-local alan-modeline-filename nil)
          ;; if we are in a debugger, dont go to the faulty slow path
          (if (and slow (not inhibit-debugger))
              (setq-local alan-modeline-filename
                          (alan-modeline-filepath-slow buf-path))

            (setq-local alan-modeline-filename
                        (alan-modeline-filepath-fast buf-path))
            (unless alan-update-modeline-filepath-timer
              (setq-local
               alan-update-modeline-filepath-timer
               (run-with-idle-timer
                0.1 nil
                (callback-lambda ()
                  (unwind-protect
                      (progn
                        (alan-set-modeline-filepath t)
                        ;; only if visible
                        (when (or (buffer-modified-p) (get-buffer-window nil t))
                          (span :alan-set-modeline-filepath
                            (force-mode-line-update))))
                    (setq-local alan-update-modeline-filepath-timer nil))))))))))))


(defun alan-modeline-handle-filename ()

  (alan-set-modeline-filepath nil)

  (if (or buffer-file-name (derived-mode-p 'dired-mode))
      (setq-local alan-modeline-buffername nil)
    (setq-local alan-modeline-buffername
                (propertize (buffer-name) 'face 'modeline-file-or-buffer-name)))

  nil)

(defun doom-modeline-selection-info ()
  (when (or mark-active (and (bound-and-true-p evil-local-mode)
                             (eq evil-state 'visual)))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat
          " "
          (cond
           ;; ((or (bound-and-true-p rectangle-mark-mode)
           ;;      (and (bound-and-true-p evil-visual-selection)
           ;;           (eq 'block evil-visual-selection)))
           ;;  (let ((cols (abs (- (doom-modeline-column end)
           ;;                      (doom-modeline-column beg)))))
           ;;    (format "%dx%dB" lines cols)))

           ((and (bound-and-true-p evil-visual-selection)
                 (eq evil-visual-selection 'line))
            (format "%dL" lines))
           ((> lines 1)
            (format "%dC %dL" (- end beg) lines))
           (t
            (format "%dC" (- end beg))))
          ;; (format " %dW" (count-words beg end))
          ))
       'face 'modeline-selection-info))))

(defvar-local doom-modeline--vcs nil)
(put 'doom-modeline--vcs 'risky-local-variable t)
(defun doom-modeline-update-vcs (&rest _)
  "Update text of vcs state in mode-line."
  (setq-local doom-modeline--vcs
              (when (and vc-mode buffer-file-name)
                (let*
                    (
                     (backend (vc-backend buffer-file-name))
                     (state (vc-state (file-local-name buffer-file-name) backend))
                     (icon
                      (cond ((memq state '(edited added)) "*")
                            ((eq state 'needs-merge) "?")
                            ((memq state '(needs-update removed conflict unregistered)) "!")
                            (t "@")))
                     (str
                      (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
                  (concat
                   (propertize icon 'face 'modeline-vcs-icon)
                   (propertize str 'face 'modeline-vcs-text)
                   " "
                   )
                  ))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs)
(add-hook 'after-save-hook #'doom-modeline-update-vcs)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs)


(defun modeline-maybe-add-space (str)
  (when str
    (unless (equal str "")
      (list " " str))))

(defun anzu--update-mode-line-only-if-loaded ()
  (when (fboundp 'anzu--update-mode-line)
    (modeline-maybe-add-space (anzu--update-mode-line))))


(setq mode-line-position
      `(
        (3 ,(propertize "%l" 'face 'modeline-linenum))
        " "
        (-3 "%p")
        ))

(setq alan-modeline-lhs
      `(
        "%e"
        mode-line-position
        ;; ("" mode-line-client)

        " "
        (:eval (modeline-calc-modified))
        (:eval (alan-modeline-handle-filename))
        alan-modeline-filename
        alan-modeline-buffername
        (:eval (doom-modeline-selection-info))
        (:eval (anzu--update-mode-line-only-if-loaded))
        flycheck-mode-line
        ;; " "
        )
      )

(setq alan-modeline-rhs
      `(
        (doom-modeline--vcs doom-modeline--vcs)
        ;; (vc-mode vc-mode)
        (:propertize mode-name
                     face modeline-mode
                     mouse-face mode-line-highlight
                     local-map ,mode-line-major-mode-keymap)
        " "))

;; (substring-no-properties (format-mode-line 'mode-line-modes))

(provide 'alan-modeline)
