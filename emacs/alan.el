;; -*- lexical-binding: t -*-


(setq load-prefer-newer t)
(set-buffer (get-buffer-create " *initialization*"))

(require 'alan-early-init)

(require 'alan-start)

(run-hooks 'alan-start-of-init-hook)

(span-dbgf (frame-parameters))

(require 'alan-base-bindings)
(require 'alan-bindings)

;; (add-hook! 'alan-end-of-init-hook
;;   (menu-bar-mode -1)
;;   (tool-bar-mode -1)
;;   (scroll-bar-mode -1))

(require-noerr 'alan-evil)
(require-noerr 'alan-minibuffer)

;; visual
(require-noerr 'alan-theme)
(require-noerr 'alan-modeline)
(require-noerr 'alan-font)
(require-noerr 'alan-hl-line)
(require-noerr 'alan-terminal)
(pkg! 'rainbow-mode
  (startup-queue-package 'rainbow-mode -10))

;; global stuff
(require-noerr 'alan-commands)
(require-noerr 'alan-iflipb)
(require-noerr 'alan-undo-fu)
(require-noerr 'alan-vundo)
(require-noerr 'alan-persistent-undo)
(require-noerr 'alan-simple)
(require-noerr 'alan-evil-surround)

(pkg! 'better-jumper
  (startup-queue-package 'better-jumper 70))
(eval-after-load! better-jumper
  (better-jumper-mode +1)
  (general-def
    [remap evil-jump-forward] #'better-jumper-jump-forward
    [remap evil-jump-backward] #'better-jumper-jump-backward))


;; completion ui
(require-noerr 'alan-vertico)
(require-noerr 'alan-consult)
(require-noerr 'alan-completion-styles)
(require-noerr 'alan-company)
(require-noerr 'alan-marginalia)

;; tools
(require-noerr 'alan-flycheck)
(require-noerr 'alan-format-all)
(require-noerr 'alan-eldoc)
(require-noerr 'alan-lsp)
(require-noerr 'alan-treesitter)

;; utilities
(require-noerr 'alan-comint)
(require-noerr 'alan-transient)
(require-noerr 'alan-dired)
(require-noerr 'alan-with-editor)
(require-noerr 'alan-magit)
(require-noerr 'alan-gpt)
(require-noerr 'alan-term)

;; langs
(require-noerr 'alan-elisp)
(require-noerr 'alan-rust)
(require-noerr 'alan-python)
(require-noerr 'alan-pdf)
(require-noerr 'alan-nix)
(require-noerr 'alan-markdown)
(require-noerr 'alan-js)
(require-noerr 'alan-lilypond)
(require-noerr 'alan-shell)
(require-noerr 'alan-cxx)
(pkg! 'dockerfile-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-hook! 'yaml-ts-mode-hook
  (setq-local format-all-formatters '(("YAML" prettierd))))


(pkg! '(visual-basic-mode :host github :repo "emacsmirror/visual-basic-mode")
  (autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
  (push '("\\.\\(?:frm\\|\\(?:ba\\|vb\\)s\\)\\'" . visual-basic-mode)
        auto-mode-alist)
  (setq-default visual-basic-mode-indent 4)
  (general-def visual-basic-mode-map
    [remap evil-indent] #'evil-indent))

(eval-after-load! conf-mode
  (add-hook! 'conf-toml-mode-hook
    (defun toml-setup ()
	  (setq-local
       format-all-formatters '(("TOML" taplo-fmt))))))

(eval-after-load! arc-mode
  (general-def archive-mode-map
    [remap evil-ret] #'archive-view))


;; (alan-set-ignore-debug-on-quit #'flyspell-emacs-popup)
;; (alan-set-ignore-debug-on-quit #'flyspell-popup-correct)

;; (alan-set-ignore-debug-on-error #'elisp--local-variables)
(alan-set-ignore-debug-on-error #'completion--some)
(alan-set-ignore-debug-on-error #'company-call-backend-raw)

(defvar-local alan-font-lock-force-specified nil)

(span-wrap font-lock-default-fontify-syntactically (start end &optional loudly)
  (:font-lock-default-fontify-syntactically (buffer-name (current-buffer)) start end)
  (when loudly
    (span-flush)))

;; font lock uses some heruristics to not do any work in some case.
;; sometimes it thinks it doesnt need to do anything when using tree-sitter-hl
;; TODO: do this effect embedded langs?
(defadvice! font-lock-specified-p-override (orig-fun mode)
  :around #'font-lock-specified-p
  (if alan-font-lock-force-specified
      t
    (funcall orig-fun mode)))

(setq vc-follow-symlinks t)






(span-notef "[end-of-init-hook]")
(run-hooks 'alan-end-of-init-hook)


;; (defadvice! alan-require-debug (orig-fun feature &optional filename noerror)
;;   :around #'require
;;   (when (eq feature 'tree-sitter-langs-build)
;;     (debug))
;;   (funcall orig-fun feature filename noerror))


;; tree-sitter-load-path


;; (defadvice! alan-set-message-functions (fn msg)
;;   :around #'set-message-functions
;;   (span-notef "set-message-functions: %s" msg)
;;   (funcall fn msg))

;; (defadvice! alan-clear-minibuffer-message (fn)
;;   :around #'clear-minibuffer-message
;;   (span-notef "clear-minibuffer-message")
;;   (funcall fn))

;; (setq debug-on-message "Loaded version already satisfies requested")
;; TODO:

;; read-multiple-choice--short-answers > lookup-key
;; rebind query-replace-map

;; https://www.reddit.com/r/emacs/comments/4rd44a/how_to_change_cursor_in_evil_mode/
;; (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\033[5 q")))
;; (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\033[2 q")))

;; https://github.com/7696122/evil-terminal-cursor-changer


;; https://www.reddit.com/r/emacs/comments/ntt0v4/copy_from_emacs_in_wsl_to_windows_applicaitons/
;; xclip-mode


;; jit-lock

;; ;; https://github.com/DarthFennec/highlight-indent-guides/issues/61
;; (setq auto-window-vscroll nil)

(add-to-list 'read-only-dir-exclude-list (expand-file-name "elpaca_new/repos/gptel" user-emacs-directory))
(add-to-list 'read-only-dir-exclude-list (expand-file-name "elpaca_new/repos/tree-sitter-langs" user-emacs-directory))





;; (setq debug-allow-recursive-debug nil)


(alan-set-ignore-debug-on-error #'current-kill)

;; ;; https://github.com/DarthFennec/highlight-indent-guides/issues/61
;; (setq auto-window-vscroll nil)

;; (setq jka-compr-verbose nil)


(remove-hook 'xref-backend-functions 'etags--xref-backend)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)


(alan-set-ignore-debug-on-error #'comment-region-default)
(alan-set-ignore-debug-on-error #'revert-buffer)

(defadvice! alan-wrap-tramp-debug-message (orig-fn vec fmt-string &rest arguments)
  :around 'tramp-debug-message
  (span :tramp-debug-message
    (span-note (apply #'format-message fmt-string arguments))
    ;; (span--note-and-flush (apply #'format-message fmt-string arguments))
    (apply orig-fn vec fmt-string arguments)))

(span-wrap tree-sitter-langs--call (&rest args)
  (:tree-sitter-langs--call (:seq args))
  (span-flush))

;; (span (:tramp-send-command command)
;;   :flush-on-err t
;;   (funcall orig-fn vec command neveropen nooutput)
;;   (message "tramp-send-command: done"))
;; )
;; tramp-debug-message

;; (defadvice! alan-wrap-tramp2 (orig-fn vec command &optional neveropen nooutput)
;;   :around #'tramp-send-command
;;   (span (:tramp-send-command command)
;;     :flush-on-err t
;;     (funcall orig-fn vec command neveropen nooutput)
;;     (message "tramp-send-command: done")))

;; (defadvice! alan-wrap-tramp3 (orig-fn vec string)
;;   :around #'tramp-send-string
;;   (span :tramp-send-string
;;     :flush-on-err t
;;     (message "tramp-send-string %s %s" vec string)
;;     (funcall orig-fn vec string)
;;     (message "tramp-send-string: done")))

;; tramp-send-string

;; (defadvice! alan-wrap-tramp4 (orig-fn proc &optional timeout)
;;   :around #'tramp-wait-for-output
;;   (span (:tramp-wait-for-output proc)
;;     :flush-on-err t
;;     (message "tramp-wait-for-output %s %s" proc timeout)
;;     (funcall orig-fn proc timeout)))

;; (defadvice! alan-wrap-tramp5 (orig-fn vec)
;;   :around #'tramp-maybe-open-connection
;;   (span :tramp-maybe-open-connection
;;     :flush-on-err t
;;     (message "tramp-maybe-open-connection %s" vec)
;;     (funcall orig-fn vec)))

;; (defadvice! accept-process-output-adv (orig-fn &optional process seconds millisec just-this-one)
;;   :around #'accept-process-output

;;   (let ((inhibit-quit-old inhibit-quit)
;;         (inhibit-quit t))
;;     (span--unchecked :accept-process-output
;;       (when (and (not inhibit-quit-old) (input-pending-p))
;;         (setq quit-flag t)
;; 	    (eval '(ignore nil) t))
;;       (when inhibit-quit-old
;;         (message "accept-process-output %s %s %s %s" process seconds millisec just-this-one))
;;       (let ((inhibit-quit inhibit-quit-old))
;;         (funcall orig-fn process seconds millisec just-this-one)))))



;; (sleep-for 1)
;; (throw 'quit nil)
;; tramp-maybe-open-connection

(setq tmake-lock-file-nameramp-verbose 4)
;; tramp-send-command

;; (setq print-gensym t)
(setq print-gensym nil)

;; TODO: directory-abbrev-alist

;; (span-wrap gptel-file-handler (&rest args)
;;   :flush t
;;   (:gptel-file-handler (:seq args)))

;; with-silent-modifications




;; (span-wrap redisplay (&optional force)
;;   (:explicit-redisplay force)
;;   ;; (span-flush)
;;   )

(span-wrap jit-lock-function)



(alan-set-ignore-debug-on-error #'xref-backend-definitions)

;; (debug)
;; (setq quit-flag t)
;; (condition-case-unless-debug nil
;;     (throw 'hay nil)
;;   (error t)))

;; (add-hook! 'pre-redisplay-functions #'tmp-do-dbg)


;; (this-command-keys)
;; (read-key-sequence (this-command-keys))
;; (defad

;; (read-key nil)
;; (read-char)

(span-wrap tq-enqueue (&rest args)
  (:tq-enqueue (:seq args))
  ;; (span-flush)
  )

;; (defadvice! alan-on-debug (orig-fn &rest args)
;;   :around #'debug
;;   (span-notef "will be entering debugger!")
;;   (span-notef "inhibit-redisplay: %s" inhibit-redisplay)
;;   (let ((inhibit-eval-during-redisplay t))
;;     (apply orig-fn args)))

;; (span-wrap image-roll-redisplay (&rest args)
;;   :image-roll-redisplay
;;   (span-flush))

(provide 'alan)

;; 1) fontification-functions (hook)
;; 2) jit-lock-function
;; 3) jit-lock-fontify-now
;; 4) jit-lock--run-functions
;; 5) jit-lock-functions (hook)
;; 6) font-lock-fontify-region

;; hint: Consider setting the necessary environment variables by running:

;;      GUIX_PROFILE="/root/.config/guix/current"
;;      . "$GUIX_PROFILE/etc/profile"

;; Alternately, see `guix package --search-paths -p "/root/.config/guix/current"'.


;; hint: After setting `PATH', run `hash guix' to make sure your shell refers to `/root/.config/guix/current/bin/guix'.

;; error after (top-level)

;; Debugger entered--Lisp error: (args-out-of-range nil 2305843009213693951)
;;   line-move-visual(-1 nil)
;;   line-move(-1)
;;   evil-line-move(-1)
;;   evil-previous-visual-line(nil)
;;   funcall-interactively(evil-previous-visual-line nil)
;;   #<subr command-execute>(evil-previous-visual-line)
;;   span--wrap-command-execute(#<subr command-execute> evil-previous-visual-line)
;;   apply(span--wrap-command-execute #<subr command-execute> evil-previous-visual-line)
;;   command-execute(evil-previous-visual-line)

(defun alan-signal (signal-name signal-data)
  (let ((signal-hook-function nil))
    (when
        (eq signal-name 'wrong-type-argument)
      ;; t
      (span-notef "signal %s %s" signal-name (:unsafe signal-data))
      (span--backtrace #'alan-signal))
    (signal signal-name signal-data)))

;; (setq xabc (subr-native-comp-unit (symbol-function #'span--wrap-load)))
;; (setq signal-hook-function nil)
;; (setq signal-hook-function #'alan-signal)


;; (setq signal-hook-function #'alan-signal)
(setq-default compilation-mode-font-lock-keywords nil)


;; evil-textobj-tree-sitter--use-builtin-treesitter

;; (debug-on-entry #'comint--fontify-input-on)


;; #1  0x000000000067aac2 in really_call_select ()
;; #2  0x000000000067b23f in thread_select ()
;; #3  0x00000000006a2063 in xg_select ()
;; #4  0x000000000064f2a4 in wait_reading_process_output ()
;; #5  0x000000000047c58f in sit_for ()
;; #6  0x000000000056da59 in read_char ()
;; #7  0x000000000056e276 in read_key_sequence ()
;; #8  0x0000000000570236 in command_loop_1 ()
;; #9  0x00000000005eba4f in internal_condition_case ()
;; #10 0x000000000055b596 in command_loop_2 ()
;; #11 0x00000000005eb9b3 in internal_catch ()
;; #12 0x000000000055b4e4 in command_loop ()
;; #13 0x000000000055fdeb in recursive_edit_1 ()
;; #14 0x000000000056016f in Frecursive_edit ()


;; (symbol-plist 'ef-night)

;; (read-multiple-choice "a" '((?b "b") (?c "c")))


(alan-set-ignore-debug-on-quit #'read-multiple-choice)


;; TODO: was here, forgot what it fixes?
;; fix tramp
;; (defadvice! read-file-name-default-override-predicate (orig-fun prompt &optional dir default-filename mustmatch initial predicate)
;;   :around #'read-file-name-default
;;   (let ((predicate (or predicate #'identity)))
;;     (funcall orig-fun prompt dir default-filename mustmatch initial predicate)))

(defun alan-throwe ()
  (error "hello"))

;; (add-hook! 'pre-redisplay-functions #'alan-throwe)

;; https://emacs.stackexchange.com/questions/18262/tramp-how-to-add-a-agent-forwarding-to-ssh-connections
(eval-after-load! tramp
  ;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:host@192.168.0.236:")
                     "login-args"
                     '(("-A") ("-l" "%u") ("-p" "%p") ("%c")
                       ("-e" "none") ("%h"))

                     ;; "remote-shell" "/usr/bin/bash"

                     ;; "direct-async-process" t
                     ))
  ;; TODO: this sends a "kill" but that seems to never work?
  (advice-add #'tramp-interrupt-process :override #'ignore)

  ;; https://emacs.stackexchange.com/questions/62919/how-to-disable-magit-on-remote-files-with-tramp
  (setq vc-ignore-dir-regexp
        (rx-to-string
         '(seq bos
               (or (seq (any "/\\") (any "/\\")
                        (one-or-more (not (any "/\\")))
                        (any "/\\"))
                   (seq "/" (or "net" "afs" "...") "/")
                   ;; Ignore all tramp paths.
                   (seq "/"
                        (eval (cons 'or (mapcar #'car tramp-methods)))
                        ":"
                        (zero-or-more anything)))
               eos)))
  )


;; (setq remote-file-name-inhibit-cache 10)
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))
;; (setq tramp-verbose 1)

;; (span-msg "load-file-rep-suffixes: %S" load-file-rep-suffixes)
;; (debug-on-variable-change 'load-file-rep-suffixes)


;; (setq load-file-rep-suffixes '(".gz" ""))

;; TODO
;; This causes err, makes load-file-rep-suffixes nil
;; (let (file-name-handler-alist)
;;   (with-auto-compression-mode nil))
;; temp workaround:
(advice-add 'jka-compr-uninstall :override #'ignore)


;; (setq explicit-bash-args
;;       (delete "--noediting" explicit-bash-args))

;; (advice-add 'comint-term-environment
;;             :filter-return (lambda (env) (cons "INSIDE_EMACS" env)))

;; (setq lsp-clients-clangd-library-directories '("/home/alan/.nix-profile/share/emacs/29.2/src/"))


(defvar alan-shown-time nil)
(defun alan-update-time (&optional donot-redisp)
  ;; (span-msg "alan-update-time")
  (let ((new (format-time-string "%F %r")))
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

(clear-and-backup-keymap tab-bar-map)
(defvar alan-tabbar-count 0)
(defun alan-do-format-tab-bar ()
  (span :alan-do-format-tab-bar
    ;; (span--backtrace)
    (concat
     (let* ((text " %"))
       (put-text-property 0 1 'display '(space :align-to (+ left left-fringe left-margin)) text)
       (put-text-property 1 2 'face 'font-lock-warning-face text)
       ;; (put-text-property 1 2 'help-echo "hello" text)
       text)
     (format-message "%s" (cl-incf alan-tabbar-count))
     "   "
     (alan-update-time t)
     ;; (format-time-string "%F %r")
     )))

;; See (info "(elisp) Defining Menus")
(global-set-key [tab-bar]
                '(keymap "tab-bar-this-str-should-do-nothing"
                         (any-sym menu-item (alan-do-format-tab-bar) nil :help "..")))

(setq tooltip-delay 0.1)
(setq tooltip-short-delay 0.1)
;; (setq use-system-tooltips nil)

(tab-bar-mode)

;; (span-wrap tooltip-show (&rest args)
;;   (:tooltip-show (:seq args))
;;   (span-flush))

;; (span-wrap x-show-tip (&rest args)
;;   (:x-show-tip (:seq args))
;;   (span-flush))

(span-wrap popup-menu (&rest args)
  (:popup-menu (:seq args))
  (span-flush))

(span-wrap x-popup-menu (&rest args)
  (:x-popup-menu (:seq args))
  (span--backtrace)
  (span-flush))

(setf (alist-get 'internal-border-width tooltip-frame-parameters) 0)
(setf (alist-get 'border-width tooltip-frame-parameters) 0)

;; (span-instrument force-mode-line-update)

;; (span-wrap force-mode-line-update (&rest args)
;;   (_)
;;   ;; (span--backtrace)
;;   )

;; (defadvice! byte-compile-let--log (fn &rest args)
;;   :around #'byte-compile-let
;;   (span-msg "%s" args)
;;   (let ((res (apply fn args)))
;;     (span-msg "%s" res)
;;     res))

;; (get 'let 'byte-compile)


;; (time-to-seconds (current-time))

;; (let (current-time-list)
;;   (current-time))

;; (expand-file-name "/ssh:host@192.168.0.238:~/jitsi/config/transcripts/" "/ssh:host@192.168.0.238:~/jitsi/config/transcripts/")

;; (span-instrument tramp-file-name-handler)

;; (span-instrument tramp-handler)
;; ;; (span-instrument rename-file)
;; (span-instrument tramp-handle-write-region)
;; (span-instrument tramp-sh-handle-write-region)

;; (setq print-circle nil)

(setq tramp-inhibit-progress-reporter t)

;; (span-wrap lock-file (file)
;;   (_ "%S" file)
;;   (span--backtrace))
