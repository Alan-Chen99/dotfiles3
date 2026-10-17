;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-shell)
(require 'alan-theme)
(require 'alan-modeline)
(require 'alan-with-editor)

(require-if-is-bytecompile consult evil-collection-vterm with-editor server)
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

(defvar-local alan-vterm--id nil
  "Value of ALAN_VTERM_ID in the environment of this buffer's process.")

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

  (add-to-list 'vterm-eval-cmds '("alan-with-editor-open" alan-with-editor-open))

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

  (defadvice! alan-vterm--tag-process (fn &rest args)
    ;; Lets `alan-vterm--remote-tty' find, on a remote host, the process
    ;; this buffer started.
    :around #'vterm-mode
    (let* ((id (substring (md5 (format "%s %s %s %s" (system-name) (emacs-pid) (float-time) (random))) 0 16))
           (vterm-environment (cons (concat "ALAN_VTERM_ID=" id) vterm-environment)))
      (apply fn args)
      ;; After `vterm-mode', whose `kill-all-local-variables' would drop it.
      (setq-local alan-vterm--id id)))

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
    "SPC p" #'alan-vterm-cc-pretty
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
        (let ((vterm-environment (append (alan-with-editor-remote-env) vterm-environment)))
          (vterm-mode))))))

;; vterm's wrapper and `vterm-shell' are bash, so a host without bash needs
;; these.  sh drops the exported bash functions in `vterm-environment', so the
;; shell sends no prompt or directory marks.
(defun alan-vterm-sh (&optional arg)
  "Like `alan-vterm', but run /bin/sh and need no bash on the host."
  (interactive "P")
  ;; Loaded first: setting up vterm inside the let would set the let's
  ;; binding of `vterm-shell', lost when the let ends.
  (require 'vterm)
  (let ((vterm-wrapper-shell "/bin/sh")
        (vterm-shell "/bin/sh"))
    (alan-vterm arg)))

(defun alan-vterm-run (command)
  (interactive
   (list (read-shell-command "Vterm: ")))
  (let ((vterm-shell command)
        (vterm-environment (append (alan-with-editor-remote-env) vterm-environment)))
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

(defun alan-vterm--tmux-session-cwd (session)
  "Working directory of the active pane of tmux SESSION, or nil.
Nil when SESSION does not exist or tmux cannot read the pane's directory.
The path is on `default-directory''s host, without a remote prefix."
  (with-temp-buffer
    ;; "=" matches the name exactly; the trailing ":" picks the session's
    ;; current window, whose active pane is what an attach shows.
    (let ((rc (process-file "tmux" nil t nil "display-message" "-p" "-t"
                            (concat "=" session ":") "#{pane_current_path}")))
      (cond
       ((eq rc 0)
        (let ((path (string-trim-right (buffer-string) "\n")))
          (unless (string-empty-p path) path)))
       ((save-excursion
          (goto-char (point-min))
          (re-search-forward "^\\(no server running\\|error connecting to\\|can't find session\\)" nil t))
        nil)
       (t (error "tmux display-message exited %s: %s" rc (string-trim (buffer-string))))))))

(defun alan-vterm-tmux (session)
  "Show tmux SESSION in a vterm buffer, attaching or creating it.
Completes over existing sessions; a new name creates that session.
$EDITOR in the session's panes opens files in this Emacs, through
tmux-editor.sh, until a client other than this Emacs attaches."
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
        ;; The shell's directory mark arrives only with its next prompt, and
        ;; attaching redraws the screen without one.  A new session starts
        ;; in DIR, so only an existing one needs asking.
        (let ((cwd (let ((default-directory dir)) (alan-vterm--tmux-session-cwd session))))
          (when cwd
            (setq dir (concat (file-remote-p dir) (file-name-as-directory cwd)))))
        (pop-to-buffer-same-window (let ((default-directory dir)) (get-buffer-create name)))
        ;; `vterm-mode' hands this string to "sh -c", so it needs quoting.
        ;; Passthrough carries the shell's OSC 51 prompt/directory marks
        ;; (see vterm_printf), which tmux otherwise drops.  It is a window
        ;; option with no per-session scope, hence global.  Mouse mode makes
        ;; tmux request mouse reporting, so `vterm-scroll-up' sends it wheel
        ;; events and tmux scrolls its own history; vterm only ever holds
        ;; the visible screen.  With the status line off, panes fill the
        ;; whole window.  set-titles forwards the active pane's title
        ;; (OSC 0/2 from its program) to vterm, where it shows in the mode
        ;; line.  These are session options, so other clients of the
        ;; session get them too.
        ;;
        ;; The session outlives this Emacs, and a pane's environment is
        ;; fixed when the pane starts, so $EDITOR is a script that asks tmux
        ;; on every call which editor the last client to attach left in the
        ;; session's environment (update-environment).  On a remote host
        ;; that is remote-editor.sh, whose request reaches this Emacs through
        ;; passthrough; both scripts are copied there.
        (let* ((local (not (file-remote-p dir)))
               (editor (if local
                           (expand-file-name "tmux-editor.sh" alan-dotemacs-dir)
                         (alan-with-editor-install "tmux-editor.sh")))
               (vterm-shell
                (concat
                 "tmux set-option -g allow-passthrough on \\; "
                 "set-option -g 'update-environment[90]' EMACS_SOCKET_NAME \\; "
                 "set-option -g 'update-environment[91]' ALAN_EMACSCLIENT \\; "
                 "set-option -g 'update-environment[92]' ALAN_REMOTE_EDITOR \\; "
                 (format "set-environment -g EDITOR %s \\; " (shell-quote-argument editor))
                 (format "new-session -A -s %s \\; set-option mouse on \\; set-option status off \\; set-option set-titles on \\; set-option set-titles-string '#{pane_title}'"
                         (shell-quote-argument session))))
               (process-environment
                (append (if local
                            (list (concat "EMACS_SOCKET_NAME=" (expand-file-name server-name server-socket-dir))
                                  (concat "ALAN_EMACSCLIENT=" with-editor-emacsclient-executable))
                          (list (concat "ALAN_REMOTE_EDITOR=" (alan-with-editor-install "remote-editor.sh"))))
                        process-environment)))
          (vterm-mode)
          ;; The process is only a tmux client: killing it detaches, and the
          ;; session keeps running, so there is nothing to confirm.
          (set-process-query-on-exit-flag vterm--process nil))))))

(defun alan-vterm-tmux-sh (session)
  "Like `alan-vterm-tmux', but start tmux without bash on the host.
A tmux server this starts lacks the exported bash functions, so its
panes send no prompt or directory marks even if they run bash."
  (interactive
   (list (completing-read "tmux session: " (alan-vterm--tmux-sessions))))
  (require 'vterm)
  (let ((vterm-wrapper-shell "/bin/sh"))
    (alan-vterm-tmux session)))

(defun alan-consult-vterm ()
  "Switch to a vterm buffer, matching its name or its terminal title."
  (interactive)
  (require 'consult)
  (consult--multi
   (list
    (list
     :name "Vterm"
     :category 'buffer
     :state #'consult--buffer-state
     :items
     (lambda ()
       (consult--buffer-query
        :sort 'visibility
        :mode 'vterm-mode
        :as (lambda (buf)
              (let ((title (buffer-local-value 'alan-vterm-current-title buf)))
                (cons (if title
                          (concat (buffer-name buf)
                                  "  "
                                  (propertize title 'face 'completions-annotations))
                        (buffer-name buf))
                      buf)))))))
   :require-match t
   :prompt "Vterm: "
   :history 'consult--buffer-history
   :sort nil))

(defun alan-vterm--remote-tty ()
  "Pty on its host that the process this remote vterm started reads input from.
Locally that process is ssh, whose tty says nothing about the pty
sshd opened on the host; `alan-vterm--id' finds the process there."
  (let ((id (or alan-vterm--id
                (user-error "%s was started without ALAN_VTERM_ID; restart it" (buffer-name)))))
    (with-temp-buffer
      ;; Only the user's own environ files are readable, and a process
      ;; exiting mid-scan fails its grep the same as a non-match.
      (let ((rc (process-file
                 "sh" nil t nil "-c"
                 "for d in /proc/[0-9]*; do
  if [ -O \"$d/environ\" ] && grep -qzx \"ALAN_VTERM_ID=$1\" \"$d/environ\" 2>/dev/null; then
    echo ${d#/proc/} $(ps -o ppid= -p ${d#/proc/}) $(readlink $d/fd/0)
  fi
done"
                 "sh" id)))
        (unless (eq rc 0)
          (error "Scanning %s for ALAN_VTERM_ID exited %s: %s"
                 (file-remote-p default-directory) rc (string-trim (buffer-string))))
        ;; A line short of three fields is a process that exited mid-scan.
        (let* ((procs (seq-filter (lambda (f) (length= f 3))
                                  (mapcar #'split-string (split-string (buffer-string) "\n" t))))
               (pids (mapcar #'car procs))
               ;; The shell and everything it starts inherit the ID; the
               ;; process vterm started is the one whose parent lacks it.
               (roots (seq-remove (lambda (f) (member (nth 1 f) pids)) procs)))
          (pcase roots
            (`((,_ ,_ ,tty)) tty)
            ('() (error "No process on %s carries ALAN_VTERM_ID=%s"
                        (file-remote-p default-directory) id))
            (_ (error "Several processes on %s carry ALAN_VTERM_ID=%s: %S"
                      (file-remote-p default-directory) id roots))))))))

(defun alan-vterm--program-tty ()
  "Terminal that the program shown in this vterm buffer reads input from.
The terminal is on `default-directory''s host, which must be the
host the vterm's process runs on.  A tmux client only displays a
pane; the pane's own tty is where its program runs."
  (let ((tty (if (file-remote-p default-directory)
                 (alan-vterm--remote-tty)
               (or (process-tty-name vterm--process)
                   (error "vterm process %s has no tty" vterm--process)))))
    (or (and (executable-find "tmux" t)
             ;; Not "display-message -c TTY": with no client on TTY it
             ;; answers for some other client, exit status 0.
             (with-temp-buffer
               (let ((rc (process-file "tmux" nil t nil "list-clients" "-F" "#{client_tty} #{pane_tty}")))
                 (cond
                  ((eq rc 0)
                   (seq-some (lambda (line)
                               (pcase (split-string line " ")
                                 (`(,client ,pane) (and (equal client tty) pane))))
                             (split-string (buffer-string) "\n" t)))
                  ((save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^\\(no server running\\|error connecting to\\)" nil t))
                   nil)
                  (t (error "tmux list-clients exited %s: %s" rc (string-trim (buffer-string))))))))
        tty)))

(defun alan-vterm--claude-config-dir ()
  "Claude Code's config dir on `default-directory''s host.
A remote host's $CLAUDE_CONFIG_DIR is not consulted."
  (let ((remote (file-remote-p default-directory)))
    (if remote
        (expand-file-name (concat remote "~/.claude"))
      (expand-file-name (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude")))))

(defun alan-vterm--claude-sessions-on-tty (tty)
  "Live interactive Claude Code sessions reading their input from TTY.
TTY and the sessions are on `default-directory''s host.  Each is the
alist of its ~/.claude/sessions/<pid>.json, which Claude Code rewrites
on /clear and /resume and deletes on exit."
  (let ((remote (file-remote-p default-directory))
        ;; Tramp caches listings and attributes; these files change under it.
        (remote-file-name-inhibit-cache t)
        found)
    (dolist (file (directory-files (expand-file-name "sessions" (alan-vterm--claude-config-dir))
                                   t "\\`[0-9]+\\.json\\'"))
      (let ((s (condition-case nil
                   (with-temp-buffer
                     (insert-file-contents file)
                     (json-parse-buffer :object-type 'alist))
                 ;; The process exited between listing and reading.
                 (file-missing nil))))
        (when (and s
                   (equal (alist-get 'kind s) "interactive")
                   ;; Not `process-attributes': its ttname drops a slash ("/dev/pts13").
                   (equal (file-symlink-p (format "%s/proc/%d/fd/0" (or remote "") (alist-get 'pid s))) tty)
                   ;; A file left by a crashed claude can name a pid since
                   ;; reused; procStart is /proc/<pid>/stat's starttime.
                   (equal (alist-get 'procStart s)
                          (with-temp-buffer
                            ;; Not `insert-file-contents': through Tramp it reads
                            ;; nothing of a file whose reported size is 0.
                            (let ((rc (process-file "cat" nil t nil (format "/proc/%d/stat" (alist-get 'pid s)))))
                              (unless (eq rc 0)
                                (error "cat /proc/%d/stat exited %s: %s" (alist-get 'pid s) rc (buffer-string))))
                            (goto-char (point-min))
                            ;; comm, field 2, may hold spaces; fields after its ")" start at 3.
                            (re-search-forward ") ")
                            (nth 19 (split-string (buffer-substring (point) (point-max)))))))
          (push s found))))
    found))

(defvar-local alan-cc-pretty--session nil
  "Session id this cc-pretty buffer renders.
The session is on `default-directory''s host.")

(defun alan-cc-pretty--revert (&rest _)
  (let* ((id alan-cc-pretty--session)
         (line (line-number-at-pos))
         (inhibit-read-only t)
         ;; agent-tools runs here, so a remote log is rendered from a local
         ;; copy.  The copy outlives the render: cc-pretty's header names
         ;; it for recovering a block with sed.
         (copy (when (file-remote-p default-directory)
                 (let* ((remote-file-name-inhibit-cache t)
                        (pattern (expand-file-name (format "projects/*/%s.jsonl" id)
                                                   (alan-vterm--claude-config-dir)))
                        (log (pcase (file-expand-wildcards pattern)
                               (`(,log) log)
                               ('() (error "No log matches %s" pattern))
                               (logs (error "Several logs match %s: %s" pattern logs))))
                        (copy (expand-file-name
                               (format "alan-cc-pretty/%s/%s.jsonl" (file-remote-p log 'host) id)
                               temporary-file-directory)))
                   (make-directory (file-name-directory copy) t)
                   (copy-file log copy t)
                   copy))))
    (erase-buffer)
    (let ((rc (let ((default-directory (expand-file-name "~/")))
                (call-process "agent-tools" nil t nil "cc-pretty" (or copy id) "--color"))))
      (unless (eq rc 0)
        (error "agent-tools cc-pretty %s exited %s; its output is in %s" (or copy id) rc (buffer-name))))
    ;; Faces as plain `face': `font-lock-face' needs font-lock-mode.
    (let ((ansi-color-apply-face-function
           (lambda (beg end face) (when face (put-text-property beg end 'face face)))))
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (point-min))
    (forward-line (1- line))))

(define-derived-mode alan-cc-pretty-mode special-mode "cc-pretty"
  "A Claude Code session rendered by `agent-tools cc-pretty'; \\[revert-buffer] re-renders."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'alan-cc-pretty--revert))

(defun alan-vterm-cc-pretty ()
  "Render the Claude Code session running in this vterm into another window.
Works through a tmux client too: the session is the one in its active
pane.  In a remote vterm the session is looked up on the remote host."
  (interactive)
  (unless (derived-mode-p 'vterm-mode)
    (user-error "Not a vterm buffer"))
  (require 'ansi-color)
  ;; The vterm's process runs on the host of the directory it started in,
  ;; whose remote prefix `alan-vterm--get-directory' keeps.
  (let* ((default-directory (expand-file-name (concat (file-remote-p default-directory) "~/")))
         (tty (alan-vterm--program-tty))
         (sessions (alan-vterm--claude-sessions-on-tty tty))
         (host (file-remote-p default-directory 'host)))
    (when (cdr sessions)
      (user-error "Several claude sessions on %s%s: %s" (if host (concat host ":") "") tty
                  (mapconcat (lambda (s) (format "pid %s %s" (alist-get 'pid s) (alist-get 'sessionId s)))
                             sessions ", ")))
    (unless sessions
      (user-error "No claude reading input from %s%s" (if host (concat host ":") "") tty))
    (let* ((s (car sessions))
           (dir default-directory)
           (buf (get-buffer-create (format "*cc-pretty:%s%s*"
                                           (if host (concat host ":") "")
                                           (or (alist-get 'name s) (alist-get 'sessionId s))))))
      (with-current-buffer buf
        (unless (derived-mode-p 'alan-cc-pretty-mode)
          (alan-cc-pretty-mode))
        (setq default-directory dir)
        (setq alan-cc-pretty--session (alist-get 'sessionId s))
        (revert-buffer))
      (display-buffer buf '(nil (inhibit-same-window . t))))))

(defun alan-vterm-kill-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer)) 'current-group))

(provide 'alan-vterm)
