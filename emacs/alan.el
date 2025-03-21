;; -*- lexical-binding: t -*-


(setq load-prefer-newer t)
(set-buffer (get-buffer-create " *initialization*"))

;; TODO: without this, this file does not native compile correctly:
;; it simply returns here for some reason
(defun alan--require-alan-early-init ()
  (require 'alan-early-init))
(alan--require-alan-early-init)

(require 'alan-start)

(run-hooks 'alan-start-of-init-hook)

(span-dbgf (frame-parameters))

(require 'alan-base-bindings)
(require 'alan-bindings)

(require-noerr 'alan-private)

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
(pkg! '(rainbow-mode :inherit nil :host github :repo "emacsmirror/rainbow-mode")
  (startup-queue-package 'rainbow-mode -10))

;; global stuff
(require-noerr 'alan-commands)
(require-noerr 'alan-iflipb)
(require-noerr 'alan-undo-fu)
(require-noerr 'alan-vundo)
(require-noerr 'alan-persistent-undo)
(require-noerr 'alan-simple)
(require-noerr 'alan-evil-surround)
(require-noerr 'alan-jump)
(require-noerr 'alan-embark)
(require-noerr 'alan-tramp)


;; completion ui
(require-noerr 'alan-vertico)
(require-noerr 'alan-consult)
(require-noerr 'alan-completion-styles)
(require-noerr 'alan-company)
(require-noerr 'alan-marginalia)
(require-noerr 'alan-popup)

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
(require-noerr 'alan-vterm)
(require-noerr 'alan-codeium)
(require-noerr 'alan-info)
(require-noerr 'alan-flyspell)

;; langs
(require-noerr 'alan-csharp)
(require-noerr 'alan-cxx)
(require-noerr 'alan-dafny)
(require-noerr 'alan-docker)
(require-noerr 'alan-elisp)
(require-noerr 'alan-js)
(require-noerr 'alan-latex)
(require-noerr 'alan-lilypond)
(require-noerr 'alan-markdown)
(require-noerr 'alan-nix)
(require-noerr 'alan-pdf)
(require-noerr 'alan-python)
(require-noerr 'alan-rust)
(require-noerr 'alan-scheme)
(require-noerr 'alan-shell)
(pkg! 'lua-mode)
(pkg! 'php-mode)

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-hook! 'yaml-ts-mode-hook
  (setq-local evil-shift-width 2)
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


(require-noerr 'alan-experimental)

;; (alan-set-ignore-debug-on-quit #'flyspell-emacs-popup)
;; (alan-set-ignore-debug-on-quit #'flyspell-popup-correct)

;; (alan-set-ignore-debug-on-error #'elisp--local-variables)
;; (alan-set-ignore-debug-on-error #'completion--some)
;; (alan-set-ignore-debug-on-error #'company-call-backend-raw)

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



;; (span-dbgf elpaca-build-steps)

;; (pkg! '(python-coverage :alan-extra-deps ((xml+ "0"))))


;; (span-wrap elpaca--continue-build (f &rest args)
;;   (_)
;;   (span-dbgf elpaca-build-steps))

;; (span-dbgf elpaca-build-steps)


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


;; (alan-set-ignore-debug-on-error #'current-kill)

;; ;; https://github.com/DarthFennec/highlight-indent-guides/issues/61
;; (setq auto-window-vscroll nil)

;; (setq jka-compr-verbose nil)


(remove-hook 'xref-backend-functions 'etags--xref-backend)
(remove-hook 'xref-after-jump-hook 'xref-pulse-momentarily)
(remove-hook 'xref-after-return-hook 'xref-pulse-momentarily)


;; (alan-set-ignore-debug-on-error #'comment-region-default)
;; (alan-set-ignore-debug-on-error #'revert-buffer)


(span-wrap tree-sitter-langs--call (&rest args)
  (:tree-sitter-langs--call (:seq args))
  (span-flush))

;; (sleep-for 1)
;; (throw 'quit nil)
;; tramp-maybe-open-connection

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



;; (alan-set-ignore-debug-on-error #'xref-backend-definitions)

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


;; (read-multiple-choice "a" '((?b "b") (?c "c")))

(alan-set-ignore-debug-on-quit #'read-multiple-choice)


;; TODO: was here, forgot what it fixes?
;; fix tramp
;; (defadvice! read-file-name-default-override-predicate (orig-fun prompt &optional dir default-filename mustmatch initial predicate)
;;   :around #'read-file-name-default
;;   (let ((predicate (or predicate #'identity)))
;;     (funcall orig-fun prompt dir default-filename mustmatch initial predicate)))


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

(eval-after-load! midnight
  (setq clean-buffer-list-delay-general 1.5))

(setq tab-always-indent 'complete)

;; symlinks
;; (setq find-file-visit-truename nil)
(setq find-file-visit-truename t)


;; (setq flycheck-navigation-minimum-level 'warnings)
;; (setq flycheck-error-list-minimum-level 'warnings)

;; (eval-after-load! woman
;;   (advice-add #'woman-dired-define-keys :override #'ignore))

(defun qemu-stop ()
  (interactive)
  (process-send-string nil "\C-ax"))

;; (span-instrument company-capf)
;; (setq lsp-completion-sort-initial-results nil)
;; (setq lsp-completion-filter-on-incomplete t)

;; (setq lsp-completion--no-reordering t)

;; (span-instrument pgtk-get-selection-internal)
;; (span-instrument TeX-revert-document-buffer)
;; (span-instrument revert-buffer-insert-file-contents--default-function)
;; (setq file-notify-debug t)

;; (span-instrument transient-init-value)
;; (span-instrument transient-history-next)

;; (defun lsp--calculate-root (session file-name)
;;   (let ((debug-on-error t))
;;     (debug)))
;; (defadvice! lsp--calculate-root--bt (fn &rest args)
;;   :around #'lsp--calculate-root
;;   (span--backtrace)
;;   (apply fn args))

;; (disassemble #'read-extended-command-1)
;; (symbol-function #'completing-read)

;; (span-instrument comint-term-environment)

;; (span-wrap eldoc-display-in-echo-area (docs interactive)
;;   (_)
;;   (dolist (doc docs)
;;     (span-notef "%s" (:unsafe (car doc)))
;;     (span-notef "%s" (:ts (cdr doc)))))

;; (span-wrap lsp--render-markdown ()
;;   (_)
;;   (span-notef "%s" (:unsafe (buffer-string))))

;; (span-wrap lsp--setup-markdown (mode)
;;   (_)
;;   (span-notef "%s" (:unsafe (buffer-string))))


;; (oref
;;  (transient-prefix :init-value 'a)
;;  :init-value)

;; (setf (oref (get #'magit-log 'transient--prefix) :init-value)

;;       'a)
;; (require 'tmp)
;; (span-instrument magit-log)
;; (span-instrument transient-init-value :verbose t)

;; (setq byte-compile-warnings '(not docstrings))

;; (elpaca<-main (elpaca-get 'boogie-friends))
;; (elpaca<-dependencies (elpaca-get 'boogie-friends))

;; (alan-set-ignore-debug-on-error 'company-auto-begin)
;; (span-instrument prettify-symbols-mode)

;; (span-instrument hotfuzz-rs-module-filter)
;; (span-instrument hotfuzz-all-completions)
;; (span-instrument all-completions)
;; (span-instrument completion-file-name-table
;;   (span-dbg throw-on-input inhibit-quit))
;; (span-instrument file-name-all-completions
;;   (span-dbg throw-on-input inhibit-quit))


;; (span-instrument self-insert-command
;;   (span-dbgf (this-command-keys)))
;; (span-instrument redisplay--pre-redisplay-functions)

;; (span-instrument company-call-backend-raw
;;   :verbose t
;;   ;; :backtrace t
;;   (span-dbgf company-backend))

;; (span-instrument company-call-frontends
;;   :verbose t
;;   ;; (span-dbgf company-candidates)
;;   )

;; #'company-pseudo-tooltip-frontend
;; (e
;; (e
;; (efe

;; (let ((company-backend 'company-capf)) (company-call-backend-raw 'prefix))

;; (elpaca<-recipe (elpaca-get 'tablist))
;; (elpaca-menu-item 'tablist)
;; (elpaca-recipe 'tablist)
;; (elpaca--lock-file-init-p (elpaca-get 'tablist))
;; (run-hook-with-args-until-failure 'elpaca-lock-file-functions (elpaca-get 'tablist))

;; (advice-add #'elpaca-info--source-buttons :override #'ignore)

;; (elpaca-test
;;   ;; :ref "0c43289"
;;   ;; :keep t
;;   :buffer "*elpaca-test*"
;;   :init
;;   (add-to-list 'load-path "/home/alan/dotfiles_new/emacs/")

;;   (defun ci--redirect-to-stdout (msg)
;;     (message "%s" msg))
;;   (defvar span-log-handler #'ci--redirect-to-stdout)

;;   (require 'span)

;;   (advice-add #'elpaca--log :before #'elpaca--log--redirect)
;;   (defun elpaca--log--redirect (e text &optional _verbosity _replace)
;;     (span-notef "%-20s %-10s %-10s" (elpaca<-id e) (elpaca--status e) text))

;;   (elpaca '(testlib :repo "/home/alan/tmp7/testlib/" :wait t))
;;   ;; (elpaca (general :wait t :vars ((byte-compile-warnings '(not docstrings)))))
;;   (princ (elpaca-log ".*"))
;;   )

;; (alan-set-ignore-debug-on-error #'boogie-friends-backend-index)

;; (setq python-shell-readline-completer-delims t)
;; (advice-add #'python-shell-completion-native-try :override #'always)

;; setsid bash -c bash
