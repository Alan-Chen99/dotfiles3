;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-theme)
(require 'evil)

;; xdisp.c > decode_mode_spec

(defvar alan-modeline-lhs "%b %f")
(defvar alan-modeline-rhs 'mode-line-modes)

(put 'alan-modeline-lhs 'risky-local-variable t)
(put 'alan-modeline-rhs 'risky-local-variable t)

(defun alan-right-align-space (rhs-width &optional frame)
  (propertize
   " "
   'display
   ;; Backport from `mode-line-right-align-edge' in 30
   `(space :align-to (,(- (if frame (frame-pixel-width) (window-pixel-width))
                          (window-scroll-bar-width)
                          (window-right-divider-width)
                          (* (or (cdr (window-margins)) 1)
                             (frame-char-width))
                          rhs-width)))))

(defun doom-modeline-string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width str)
    (* (string-width str) (window-font-width nil 'mode-line)
       (if (display-graphic-p) 1.05 1.0))))

(defun alan-format-modeline ()
  (span :alan-format-modeline
    (with-demoted-errors "error in alan-format-modeline: %S"
      ;; (with-demoted-errors
      ;; (span-msg "%s" (current-buffer))
      ;; (span--backtrace)
      (let* ((lhs (format-mode-line alan-modeline-lhs))
             (rhs (format-mode-line alan-modeline-rhs))
             (rhs-width (doom-modeline-string-pixel-width rhs)))
        (concat
         lhs
         (alan-right-align-space rhs-width)
         rhs)))))


(defvar-local alan-formatted-modeline nil)
(put 'alan-formatted-modeline 'risky-local-variable t)

;; this is so that we dont do precent replacement again
(defun alan-format-modeline-wrapper ()
  (setq-local alan-formatted-modeline (alan-format-modeline))
  nil)

(setq-default mode-line-format
              `(
                ;; whitespace to pad to first column
                ,(eval-when-compile
                   (let* ((text (copy-sequence " ")))
                     (put-text-property 0 1 'display '(space :align-to 0) text)
                     text))
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

;; (get 'mode-line 'face-defface-spec)
;; (get 'mode-line 'face-override-spec)
;; (get 'mode-line 'theme-face)

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
         (let ((ans '(:box unspecified :foreground unspecified)))
           (unless (plist-get p :distant-foreground)
             (when (plist-get p :foreground)
               (setf (plist-get ans :distant-foreground) (plist-get p :foreground))))
           ans))))))

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
            (when-let* ((project (project-current nil dirname)))
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

    (setq ans (apply #'concat ans))

    (if remote
        (propertize
         (concat
          (file-remote-p dirname 'method)
          ":"
          ans)
         'help-echo
         (concat remote ans))

      ans)))



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
(defvar-local alan-modeline-file-and-buffername nil)
;; (put 'alan-modeline-filename 'risky-local-variable t)
;; (put 'alan-modeline-buffername 'risky-local-variable t)
(put 'alan-modeline-file-and-buffername 'risky-local-variable t)

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
                        ;; (span-dbgf "alan-set-modeline-filepath" (current-buffer))
                        (alan-set-modeline-filepath t)
                        ;; only if visible
                        (when (or (buffer-modified-p) (get-buffer-window nil t))
                          (span :alan-set-modeline-filepath
                            (force-mode-line-update))))
                    (setq-local alan-update-modeline-filepath-timer nil))))))
            ))))))


(defun alan-modeline-handle-filename ()

  (alan-set-modeline-filepath nil)

  (if (or buffer-file-name (derived-mode-p 'dired-mode))
      (setq-local alan-modeline-buffername nil)
    (setq-local alan-modeline-buffername
                (propertize (buffer-name) 'face 'modeline-file-or-buffer-name)))

  (let* ((fname (or alan-modeline-filename ""))
         (bname (or alan-modeline-buffername ""))
         (ans (concat fname bname)))
    (setq-local
     alan-modeline-file-and-buffername
     (propertize
      ans
      'mouse-face 'mode-line-highlight
      'help-echo
      (concat
       (or (get-text-property 0 'help-echo fname) fname)
       (or (get-text-property 0 'help-echo bname) bname)))))

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

;; (defvar-local alan-last-process-status nil)
(defvar-local alan-last-buffer-process nil)

;; (advice-add #'internal-default-process-sentinel :override #'alan-internal-default-process-sentinel)
;; (defun alan-internal-default-process-sentinel (proc msg)
;;   (ignore proc)
;;   (setq alan-last-process-status msg))

(defun alan-modeline-process-info ()
  (setq alan-last-buffer-process
        (or (get-buffer-process (current-buffer)) alan-last-buffer-process))

  (when-let* ((proc alan-last-buffer-process))
    (concat
     " "
     ;; (prin1-to-string (process-id proc))
     ;; ":"
     (prin1-to-string (process-status proc))
     ":"
     (prin1-to-string (process-exit-status proc))
     ))
  )

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
        alan-modeline-file-and-buffername
        (:eval (alan-modeline-process-info))
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


(defvar alan-shown-time nil)
(defun alan-update-time (&optional donot-redisp)
  (let ((new (format-time-string "%F %a %I:%M:%S %p")))
    (unless (string= alan-shown-time new)
      (setq alan-shown-time new)
      (unless donot-redisp
        (force-mode-line-update t)))
    new))
(defvar alan-update-time-timer nil)
(ignore-errors
  (cancel-timer alan-update-time-timer))
(progn
  (setq alan-update-time-timer (run-at-time t 1 #'ignore))
  (timer-set-function alan-update-time-timer #'alan-update-time))


(defvar alan--last-seen-command nil)
(defvar alan--n-processed-keys 0)
(defvar alan--displayed-keys "")
(defvar alan--last-key-time (current-time))

(lossage-size 100)

(defun alan--handle-key-pressed (key)
  (unless (consp key)
    (let ((desc (single-key-description key)))
      (if (length= desc 1)
          (setq alan--displayed-keys (concat alan--displayed-keys desc))
        (if (string-suffix-p " " alan--displayed-keys)
            (setq alan--displayed-keys (concat alan--displayed-keys desc " "))
          (setq alan--displayed-keys (concat alan--displayed-keys " " desc " ")))))))

(defvar alan-maybe-update-keys--nesting 0)

(defun alan-maybe-update-keys (&rest _)
  (span :alan-maybe-update-keys
    (let* ((recent (recent-keys t))
           (search-pos (cl-position alan--last-seen-command recent :test #'eq :from-end t))
           (pos (if search-pos
                    (+ search-pos alan--n-processed-keys 1)
                  alan--n-processed-keys))
           ;; TODO: emacs is allowed to regret stuff in (recent-keys) and decide that
           ;; something isnt a key after all, usually in terminal
           ;; this case can prob be handled better
           (ans (if (length> recent pos) (cl-subseq recent pos) (vector)))
           (new-pos (cl-position nil recent :if #'consp :from-end t)))
      (when (length> ans 0)
        (when (> (time-to-seconds (time-subtract (current-time) alan--last-key-time)) 1)
          (setq alan--displayed-keys (concat alan--displayed-keys "  ")))
        (setq alan--last-key-time (current-time))
        (mapc #'alan--handle-key-pressed ans)
        (when (length> alan--displayed-keys 50)
          (setq alan--displayed-keys
                (substring alan--displayed-keys (- (length alan--displayed-keys) 25))))
        ;; TODO: perhaps not do this in pre-command-hook?
        (span :force-mode-line-update
          (force-mode-line-update t)))
      (setq alan--last-seen-command (when new-pos (aref recent new-pos)))
      (setq alan--n-processed-keys (if new-pos (- (length recent) new-pos 1) (length recent)))
      nil)))


(add-hook! 'set-message-functions :depth -100 #'alan-maybe-update-keys)
(add-hook! 'pre-command-hook :depth -100 #'alan-maybe-update-keys)
(advice-add #'read-key-sequence :after #'alan-maybe-update-keys)
(advice-add #'read-key-sequence-vector :after #'alan-maybe-update-keys)
(advice-add #'read-event :after #'alan-maybe-update-keys)

(defun alan-fmt-cmd (cmd)
  (if (symbolp cmd)
      (format "%s" cmd)
    (format "[%s]" (type-of cmd))))

(clear-and-backup-keymap tab-bar-map)
(defvar alan-tabbar-count 0)
(defun alan-do-format-tab-bar ()
  (span :alan-do-format-tab-bar
    (let* ((lhs
            (concat
             (eval-when-compile
               (let* ((text (copy-sequence " %")))
                 (put-text-property 0 1 'display '(space :align-to (+ left left-fringe left-margin)) text)
                 (put-text-property 1 2 'face 'font-lock-warning-face text)
                 ;; (put-text-property 1 2 'help-echo "hello" text)
                 text))
             (format-message "%s" (cl-incf alan-tabbar-count))
             " "
             ;; buffer-file-name
             (format "%-52s" alan--displayed-keys)
             (if (> (time-to-seconds (time-subtract (current-time) alan--last-key-time)) 1)
                 ""
               (let ((cmd (cdr alan--last-seen-command)))
                 ;; (span-dbgf alan--last-seen-command real-last-command)
                 (if (eq cmd real-last-command)
                     (format "| %s" (alan-fmt-cmd cmd))
                   (format "| %s -> %s" (alan-fmt-cmd cmd) (alan-fmt-cmd real-last-command)))))))

           (rhs (concat (alan-update-time t) ""))

           (lhs-limit (- (frame-width) (string-width rhs))))
      (when (< lhs-limit 0)
        (setq lhs-limit 0))
      ;; here we force truncation bc overflowing cause the buffer to move
      ;; but perhaps we can make it not move the buffer and let it overflow
      (when (length> lhs lhs-limit)
        (setq lhs (substring lhs 0 lhs-limit)))
      (concat
       lhs
       (alan-right-align-space (doom-modeline-string-pixel-width rhs) t)
       rhs
       ))))

;; See (info "(elisp) Defining Menus")
(global-set-key [tab-bar]
                '(keymap "tab-bar-this-str-should-do-nothing"
                         (any-sym menu-item (alan-do-format-tab-bar) nil :help "..")))

(setq tooltip-delay 0.1)
(setq tooltip-short-delay 0.1)
;; (setq use-system-tooltips nil)

(tab-bar-mode)


(provide 'alan-modeline)
