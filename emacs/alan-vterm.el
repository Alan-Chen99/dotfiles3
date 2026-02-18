;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-shell)

(require-if-is-bytecompile evil-collection-vterm)
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

(defun vterm-scroll-up ()
  "Scroll up in vterm.
When the application has enabled mouse reporting, send the SGR
mouse scroll-up escape sequence.  Otherwise, fall back to
`scroll-down-command' (which scrolls the viewport up)."
  (interactive)
  (if (vterm-mouse-reporting-p)
      (let ((col (1+ (current-column)))
            (row (1+ (count-screen-lines (window-start) (point)))))
        (vterm-send-string (format "\e[<64;%d;%dM" col row)))
    (scroll-down-command)))

(defun vterm-scroll-down ()
  "Scroll down in vterm.
When the application has enabled mouse reporting, send the SGR
mouse scroll-down escape sequence.  Otherwise, fall back to
`scroll-up-command' (which scrolls the viewport down)."
  (interactive)
  (if (vterm-mouse-reporting-p)
      (let ((col (1+ (current-column)))
            (row (1+ (count-screen-lines (window-start) (point)))))
        (vterm-send-string (format "\e[<65;%d;%dM" col row)))
    (scroll-up-command)))

(defun vterm-scroll-up-mouse ()
  "Handle mouse scroll-up in vterm.
When the application has enabled mouse reporting, send the SGR
mouse scroll-up escape sequence at the mouse position.  Otherwise,
fall back to `scroll-down-command'."
  (interactive)
  (if (vterm-mouse-reporting-p)
      (let* ((pos (mouse-position))
             (col (or (cadr pos) 1))
             (row (or (cddr pos) 1)))
        (vterm-send-string (format "\e[<64;%d;%dM" col row)))
    (scroll-down-command)))

(defun vterm-scroll-down-mouse ()
  "Handle mouse scroll-down in vterm.
When the application has enabled mouse reporting, send the SGR
mouse scroll-down escape sequence at the mouse position.  Otherwise,
fall back to `scroll-up-command'."
  (interactive)
  (if (vterm-mouse-reporting-p)
      (let* ((pos (mouse-position))
             (col (or (cadr pos) 1))
             (row (or (cddr pos) 1)))
        (vterm-send-string (format "\e[<65;%d;%dM" col row)))
    (scroll-up-command)))


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
    (vterm--redraw vterm--term)))

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

  (setq vterm-timer-delay 0.1)

  (setq vterm-environment
        (append alan-bash-fns-env-vars
                (list
                 ;; TODO: append instead
                 "PROMPT_COMMAND=vterm_prompt_command")))

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

  ;; (span-instrument vterm--set-directory)
  (clear-and-backup-keymap vterm-mode-map)
  (clear-and-backup-keymap vterm-copy-mode-map)


  ;; (setq vterm-buffer-name-string "* %s")
  (setq vterm-buffer-name-string nil)

  (evil-set-initial-state 'vterm-mode 'insert)

  (general-def vterm-mode-map
    [remap backward-paragraph] #'vterm-previous-prompt
    [remap forward-paragraph] #'vterm-next-prompt
    [remap alan-completion-at-point] (vterm-with-send-key "<tab>")
    ;; [remap end-of-buffer] #'vterm-reset-cursor-point

    ;; "C-u C-k" sometimes dont work?
    ;; [remap evil-delete-whole-line] (vterm-with-send-key "C-u C-k")
    [remap evil-delete-whole-line] #'alan-vterm-delete-whole-line-without-yank

    [remap evil-append] #'evil-collection-vterm-append
    )


  (general-def vterm-mode-map
    :states 'insert
    [remap self-insert-command] #'vterm--self-insert
    "DEL" #'vterm-send-backspace
    "RET" #'vterm-send-return
    "TAB" #'vterm-send-tab

    "C-k" #'vterm-scroll-up
    "C-J" #'vterm-scroll-down
    "<wheel-up>" #'vterm-scroll-up-mouse
    "<wheel-down>" #'vterm-scroll-down-mouse

    "S-SPC" #'vterm-send-next-key
    ;; "<S-.>" #'vterm-send-next-key

    [remap previous-line] (vterm-with-send-key "<up>")
    [remap next-line] (vterm-with-send-key "<down>")
    [remap left-char] (vterm-with-send-key "<left>")
    [remap right-char] (vterm-with-send-key "<right>")

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

  (add-hook! 'vterm-mode-hook
    (defun alan-vterm-mode-setup ()
      (span-msg "alan-vterm-mode-setup")
      ;; Vterm sets buffer-read-only to t but some commands complains;
      ;; nil seems to do fine?
      (setq-local buffer-read-only nil)

      (setq-local truncate-lines t)

      ;; (setq-local evil-move-cursor-back nil)

      (add-hook 'evil-insert-state-entry-hook
                #'alan-vterm--evil-follow-term-cursor-insert nil t)
      (add-hook 'evil-insert-state-exit-hook
                #'alan-vterm--evil-freeze-term-cursor-normal nil t)

      ;; (alan-vterm--evil-freeze-term-cursor-normal)
      ;; (span-dbg hl-line-mode)
      (setq-local global-hl-line-mode nil)
      (hl-line-mode -1)))


  (evil-collection-require-lazy 'vterm))

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

(defun alan-vterm-kill-process ()
  (interactive)
  (kill-process (get-buffer-process (current-buffer)) 'current-group))

(provide 'alan-vterm)
