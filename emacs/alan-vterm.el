;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-shell)
(require 'alan-theme)
(require 'alan-modeline)
(require 'alan-with-editor)

(require-if-is-bytecompile evil-collection-vterm with-editor server)
;; (pkg! 'mistty)
;; (eval-after-load! mistty
;;   (general-def mistty-mode-map
;;     :states 'motion
;;     "SPC l" 'mistty-clear))

(pkg! '(vterm
        :remotes ("alan" :repo "Alan-Chen99/emacs-libvterm" :tag "v2026-02-17")
        :pre-build
        (progn
          ;; otherwise it will prompt for user interaction
          (setq vterm-always-compile-module t)
          (require 'vterm)
          ;; make it rebuild even if already build
          (vterm-module-compile)
          (require 'vterm-module))
        :files (:defaults "vterm-module.so" "etc")))

;; https://gist.github.com/ram535/a2153fb86f33ecec587d593c1c5e1623

(defmacro vterm-with-send-key (expr)
  `(lambda () (interactive) (vterm-send ,expr)))

(defun alan-vterm--wheel (button col-row fallback)
  "Send SGR mouse BUTTON at window-relative 0-based COL-ROW, or call FALLBACK.
The event goes to the application only when it has enabled mouse
reporting; otherwise FALLBACK scrolls the Emacs window."
  (if (vterm-mouse-reporting-p)
      ;; SGR coordinates are 1-based.  The row decides which tmux pane or
      ;; status line receives the wheel, so it has to be exact.
      (vterm-send-string (format "\e[<%d;%d;%dM" button (1+ (car col-row)) (1+ (cdr col-row))))
    (funcall fallback)))

(defun alan-vterm--point-col-row ()
  (cons (current-column) (count-screen-lines (window-start) (line-beginning-position))))

(defun alan-vterm--mouse-col-row ()
  "Window-relative column and row of the mouse wheel event or the pointer.
Keyboard bindings reach here too, so fall back to the pointer position."
  (posn-col-row
   (if (consp last-input-event)
       (event-start last-input-event)
     (let ((mp (mouse-pixel-position)))
       (posn-at-x-y (cadr mp) (cddr mp) (car mp))))))

(defun vterm-scroll-up ()
  "Scroll up at point: wheel event to the application, else `scroll-down-command'."
  (interactive)
  (alan-vterm--wheel 64 (alan-vterm--point-col-row) #'scroll-down-command))

(defun vterm-scroll-down ()
  "Scroll down at point: wheel event to the application, else `scroll-up-command'."
  (interactive)
  (alan-vterm--wheel 65 (alan-vterm--point-col-row) #'scroll-up-command))

(defun vterm-scroll-up-mouse ()
  "Like `vterm-scroll-up', but at the mouse pointer."
  (interactive)
  (alan-vterm--wheel 64 (alan-vterm--mouse-col-row) #'scroll-down-command))

(defun vterm-scroll-down-mouse ()
  "Like `vterm-scroll-down', but at the mouse pointer."
  (interactive)
  (alan-vterm--wheel 65 (alan-vterm--mouse-col-row) #'scroll-up-command))

(defun alan-vterm-send-ctrl-next-key ()
  "Read the next key and send it to vterm with Control added.
For example, pressing x sends C-x to the terminal."
  (interactive)
  (let* ((key (read-key "C-"))
         (desc (key-description (vector key))))
    (vterm-send (concat "C-" desc))))

(evil-define-operator alan-vterm-delete-char-without-yank (beg end type register)
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-collection-vterm-delete beg end type ?_))

(evil-define-operator alan-vterm-delete-whole-line-without-yank (beg end type register)
  :motion evil-line-or-visual-line
  :type line
  (interactive "<R><x>")
  (evil-collection-vterm-delete beg end type ?_))

(defun alan-vterm--evil-follow-term-cursor-insert ()
  "Enable terminal cursor ownership while in Evil insert state."
  (when (and (derived-mode-p 'vterm-mode)
             (not vterm-copy-mode))
    ;; (vterm-set-follow-cursor-shape t)
    (vterm-set-follow-cursor t)
    (let ((inhibit-redisplay t)
          (inhibit-read-only t))
      (vterm--redraw vterm--term))))

;; in some apps including claude code, cursor is fake (just a character with background)
;; so going to normal mode will put cursor at the end (the real cursor is always at the end)

(defun alan-vterm--evil-freeze-term-cursor-normal ()
  "Disable terminal cursor ownership while in non-insert Evil states."
  (when (and (derived-mode-p 'vterm-mode)
             (not vterm-copy-mode))
    ;; Keep point where the terminal cursor currently is when leaving insert.
    (vterm-set-follow-cursor nil)
    ;; (vterm-set-follow-cursor-shape nil)
    ))

(defvar-local alan-vterm-current-title nil
  "Title the terminal application last set with OSC 0/2, or nil.")

(add-to-list 'alan-modeline-lhs
             '(alan-vterm-current-title (" [" (-60 "" alan-vterm-current-title) "]"))
             t)

(eval-after-load! vterm
  (setq vterm-min-window-width 60)

  (setq vterm-copy-mode-remove-fake-newlines nil)

  (setq vterm-enable-manipulate-selection-data-by-osc52 t)

  (setq vterm-ignore-blink-cursor nil)
  (setq vterm-follow-cursor nil)
  (setq vterm-follow-cursor-shape nil)

  (setq vterm-follow-window-size nil)

  ;; (setq vterm-kill-buffer-on-exit t)
  (setq vterm-kill-buffer-on-exit nil)

  ;; (setq vterm-max-scrollback 1000)
  (setq vterm-max-scrollback 200)
  (setq vterm-clear-scrollback-when-clearing t)

  ;; does not alias with a 30fps frame rate
  (setq vterm-timer-delay 0.15)

  (setq vterm-environment
        (append alan-bash-fns-env-vars
                (list
                 ;; TODO: append instead
                 "PROMPT_COMMAND=vterm_prompt_command"
                 ;; vterm is the terminal emulator for child processes, not
                 ;; tmux/screen. Apps that see TMUX wrap escape sequences in
                 ;; DCS passthrough meant for tmux, but libvterm receives them
                 ;; first and displays garbage.
                 "TMUX="
                 "STY=")))

  (setq vterm-shell "/bin/bash")

  (defadvice! alan-vterm--get-shell ()
    :override #'vterm--get-shell
    ;; (let ((f
    ;;        ;; (expand-file-name "etc/emacs-vterm-bash.sh"
    ;;        ;;                   (file-name-directory (find-library-name "vterm")))
    ;;        (expand-file-name "emacs-shell-bash.sh" alan-dotemacs-dir)))
    ;;   (format
    ;;    "%s --init-file <(echo %s)"
    ;;    vterm-shell
    ;;    (shell-quote-argument
    ;;     ;; (concat
    ;;     ;; ". \"$HOME/.bashrc\"\n"
    ;;     (with-temp-buffer
    ;;       (insert-file-contents f)
    ;;       (buffer-string))
    ;;     t)))
    vterm-shell)


  (defadvice! alan-vterm--get-directory (path)
    :override #'vterm--get-directory
    ;; (span-dbgf path)
    (concat (file-remote-p default-directory) (file-name-as-directory path)))

  (defadvice! alan-vterm--set-title (title)
    :override #'vterm--set-title
    (setq-local alan-vterm-current-title title)
    ;; Titles can arrive without any buffer change that would refresh the mode line.
    (force-mode-line-update))

  ;; (span-instrument vterm--set-directory)
  (clear-and-backup-keymap vterm-mode-map)
  (clear-and-backup-keymap vterm-copy-mode-map)


  ;; (setq vterm-buffer-name-string "* %s")
  (setq vterm-buffer-name-string nil)

  (evil-set-initial-state 'vterm-mode 'insert)

  (general-def vterm-mode-map
    ;; [remap backward-paragraph] #'vterm-previous-prompt
    ;; [remap forward-paragraph] #'vterm-next-prompt

    [remap alan-move-down-screen] #'vterm-next-prompt
    [remap alan-move-up-screen] #'vterm-previous-prompt

    ;; [remap end-of-buffer] #'vterm-reset-cursor-point

    ;; "C-u C-k" sometimes dont work?
    ;; [remap evil-delete-whole-line] (vterm-with-send-key "C-u C-k")
    [remap evil-delete-char] #'alan-vterm-delete-char-without-yank
    [remap evil-delete-whole-line] #'alan-vterm-delete-whole-line-without-yank

    [remap evil-append] #'evil-collection-vterm-append

    [remap alan-completion-at-point] #'vterm-send-tab
    )


  (general-def vterm-mode-map
    :states 'insert
    [remap self-insert-command] #'vterm--self-insert
    "DEL" #'vterm-send-backspace
    "RET" #'vterm-send-return
    "TAB" #'vterm-send-tab
    "<.> t" #'vterm-send-tab

    "<backtab>" #'vterm--self-insert

    "C-k" #'vterm-scroll-up
    "C-J" #'vterm-scroll-down
    "<wheel-up>" #'vterm-scroll-up-mouse
    "<wheel-down>" #'vterm-scroll-down-mouse

    "S-SPC" #'alan-vterm-send-ctrl-next-key
    "<.> 2" #'alan-vterm-send-ctrl-next-key
    ;; "<S-.>" #'vterm-send-next-key

    [remap previous-line] (vterm-with-send-key "<up>")
    [remap next-line] (vterm-with-send-key "<down>")
    [remap left-char] (vterm-with-send-key "<left>")
    [remap right-char] (vterm-with-send-key "<right>")

    "C-S-k" #'vterm-scroll-up-mouse
    "C-S-j" #'vterm-scroll-down-mouse

    [remap move-beginning-of-line] (vterm-with-send-key "<start>")
    [remap move-end-of-line] (vterm-with-send-key "<end>")

    "<home>" (vterm-with-send-key "<home>")
    "<end>" (vterm-with-send-key "<end>")
    "<prior>" (vterm-with-send-key "<prior>")
    "<next>" (vterm-with-send-key "<next>")

    "<.> <right>" (vterm-with-send-key "<escape>")

    ;; "C-O" (vterm-with-send-key "C-O")
    ;; "C-G" (vterm-with-send-key "C-G")

    [remap yank] #'vterm-yank)

  (general-def vterm-mode-map
    :states 'motion
    "SPC a" #'alan-vterm-send-ctrl-next-key

    "SPC l" #'vterm-clear
    "SPC s" #'vterm-sync-window-size
    "SPC t" #'vterm-toggle-follow-cursor
    "SPC <up>" #'alan-vterm-kill-process

    "SPC c" (vterm-with-send-key "C-c")

    "<wheel-up>" #'vterm-scroll-up-mouse
    "<wheel-down>" #'vterm-scroll-down-mouse

    "SPC m" #'vterm-copy-mode
    ;; "<escape>" #'vterm--self-insert
    )

  (general-def vterm-copy-mode-map
    "SPC m" #'vterm-copy-mode-done
    )

  (add-hook! 'after-load-theme-hook
    (defun alan-vterm-resync-all-buffers ()
      ;; Cell colors are baked into text properties as each line is drawn, so
      ;; existing output keeps the previous palette until the buffer is rebuilt
      ;; from libvterm's state.  `run-hooks' reaches only the current buffer's
      ;; local hook value, so one global function walks every vterm buffer.
      ;;
      ;; Exited terminals are skipped: a rebuild reproduces only what libvterm
      ;; holds and would drop the process-exit line Emacs appended, and nothing
      ;; further is ever drawn in them.
      (mapc
       (lambda (buf)
         (with-current-buffer buf
           (when (and (derived-mode-p 'vterm-mode) (vterm-check-proc))
             (vterm-resync-buffer))))
       (buffer-list))))

  (add-hook! 'vterm-mode-hook
    (defun alan-vterm-mode-setup ()
      (span-msg "alan-vterm-mode-setup")

      (setq-local truncate-lines t)

      ;; (setq-local evil-move-cursor-back nil)

      (add-hook (var 'evil-insert-state-entry-hook)
                #'alan-vterm--evil-follow-term-cursor-insert nil t)
      (add-hook (var 'evil-insert-state-exit-hook)
                #'alan-vterm--evil-freeze-term-cursor-normal nil t)

      ;; (alan-vterm--evil-freeze-term-cursor-normal)
      ;; (span-dbg hl-line-mode)
      (setq-local global-hl-line-mode nil)
      (hl-line-mode -1)))

  (startup-queue-package 'evil-collection-vterm 80))

(eval-after-load! evil-collection-vterm
  (span-msg "evil-collection-vterm (eval-after-load!)")
  (let ((evil-collection-state-denylist '(insert))
        (evil-collection-key-blacklist (append '("[[" "]]" "a" "x") evil-collection-key-blacklist)))
    (evil-collection-vterm-setup))

  ;; (general-def vterm-mode-map)
  )

(defun alan-vterm (&optional arg)
  ;; compared to original, this restarts if the process died
  (interactive "P")
  (require 'vterm)

  (alan-setup-with-editor)

  (let* ((name (cond
                ((numberp arg) (format "%s<%d>" vterm-buffer-name arg))
                ((stringp arg) arg)
                (arg vterm-buffer-name)
                (t vterm-buffer-name))))

    (if (and (get-buffer name) (process-live-p (with-current-buffer (get-buffer name) vterm--process)))
        (switch-to-buffer (get-buffer name))
      (let ((dir default-directory))
        (when (get-buffer name)
          (setq dir (with-current-buffer (get-buffer name) default-directory))
          (kill-buffer (get-buffer name)))
        (pop-to-buffer-same-window (let ((default-directory dir)) (get-buffer-create name)))
        (vterm-mode)))))

(defun alan-vterm-run (command)
  (interactive
   (list (read-shell-command "Vterm: ")))
  (let ((vterm-shell command))
    (vterm t)))

(defun alan-vterm--tmux-sessions ()
  "Names of the sessions on the tmux server for `default-directory''s host."
  (with-temp-buffer
    (let ((rc (process-file "tmux" nil t nil "list-sessions" "-F" "#S")))
      (cond
       ((eq rc 0) (split-string (buffer-string) "\n" t))
       ;; No server, or its socket dir absent: there are no sessions yet.
       ((save-excursion
          (goto-char (point-min))
          (re-search-forward "^\\(no server running\\|error connecting to\\)" nil t))
        nil)
       (t (error "tmux list-sessions exited %s: %s" rc (string-trim (buffer-string))))))))

(defun alan-vterm-tmux (session)
  "Show tmux SESSION in a vterm buffer, attaching or creating it.
Completes over existing sessions; a new name creates that session.
On the local host, $EDITOR in the session's panes opens files in this
Emacs, through tmux-editor.sh."
  (interactive
   (list (completing-read "tmux session: " (alan-vterm--tmux-sessions))))
  (when (string-empty-p session)
    (user-error "Empty tmux session name"))
  (require 'vterm)
  (alan-setup-with-editor)
  (let* ((name (format "*tmux:%s*" session))
         (buf (get-buffer name)))
    (if (and buf (process-live-p (buffer-local-value 'vterm--process buf)))
        (switch-to-buffer buf)
      (let ((dir default-directory))
        (when buf
          (setq dir (buffer-local-value 'default-directory buf))
          (kill-buffer buf))
        (pop-to-buffer-same-window (let ((default-directory dir)) (get-buffer-create name)))
        ;; `vterm-mode' hands this string to "sh -c", so it needs quoting.
        ;; Passthrough carries the shell's OSC 51 prompt/directory marks
        ;; (see vterm_printf), which tmux otherwise drops.  It is a window
        ;; option with no per-session scope, hence global.  Mouse mode makes
        ;; tmux request mouse reporting, so `vterm-scroll-up' sends it wheel
        ;; events and tmux scrolls its own history; vterm only ever holds
        ;; the visible screen.
        ;;
        ;; The session outlives this Emacs, and a pane's environment is
        ;; fixed when the pane starts, so $EDITOR is a script that asks tmux
        ;; for the socket on every call.  update-environment refreshes the
        ;; session's copy on each attach.  A remote host cannot reach this
        ;; Emacs's socket, so its environment is left alone.
        (let* ((local (not (file-remote-p dir)))
               (vterm-shell
                (concat
                 "tmux set-option -g allow-passthrough on \\; "
                 (when local
                   (format "set-option -g 'update-environment[90]' EMACS_SOCKET_NAME \\; set-option -g 'update-environment[91]' ALAN_EMACSCLIENT \\; set-environment -g EDITOR %s \\; "
                           (shell-quote-argument (expand-file-name "tmux-editor.sh" alan-dotemacs-dir))))
                 (format "new-session -A -s %s \\; set-option mouse on"
                         (shell-quote-argument session))))
               (process-environment
                (if local
                    (append (list (concat "EMACS_SOCKET_NAME=" (expand-file-name server-name server-socket-dir))
                                  (concat "ALAN_EMACSCLIENT=" with-editor-emacsclient-executable))
                            process-environment)
                  process-environment)))
          (vterm-mode))))))

(defun alan-vterm-kill-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer)) 'current-group))

(provide 'alan-vterm)
