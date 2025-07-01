;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(sclang :repo "https://github.com/supercollider/scel"
               ;; :files ("el/*.el" "sc/scide_scel/*.sc")
               :files ("el/*.el")
               ))

(require-if-is-bytecompile
 arc-mode
 backtrace
 comint
 company-quickhelp
 dafny-mode
 format-all
 lsp-mode
 outline
 python
 tq
 tramp
 tramp-sh
 tree-sitter-langs-build

 alan-simple
 )

(eval-after-load! image-mode
  (clear-and-backup-keymap image-mode-map))

;; set by emacs wrapper from nix; should not propagate to subprocesses
(setenv "GIO_EXTRA_MODULES")
(setenv "GDK_PIXBUF_MODULE_FILE")

(defun alan-donot-debug-foreground-advice (orig-fun &rest args)
  (let ((debugger #'span--debug))
    (apply orig-fun args)))

(defun alan-donot-debug-foreground (fn)
  (advice-add fn :around #'alan-donot-debug-foreground-advice))

(alan-donot-debug-foreground #'current-kill)
(alan-donot-debug-foreground #'revert-buffer)
(alan-donot-debug-foreground #'format-all--prompt-for-formatter)
(alan-donot-debug-foreground #'elisp--local-variables)
(alan-donot-debug-foreground #'dafny-docs-open)
(alan-donot-debug-foreground #'outline-forward-same-level)
(alan-donot-debug-foreground #'outline-backward-same-level)
(alan-donot-debug-foreground #'line-move)
(alan-donot-debug-foreground #'company-quickhelp--show)

;; (alan-donot-debug-foreground #'lsp--on-idle)

(defadvice! lsp--on-idle--quiet (fn &rest args)
  :around (list #'lsp--on-idle #'lsp-request-while-no-input)
  (alan-with-demoted-errors
   (apply fn args)))
;; (advice-add #'lsp--on-idle :around #'with-no-minibuffer-message-advice)

(defadvice! previous-matching-history-element--check-nohist (fn regexp n)
  :around #'previous-matching-history-element
  (let ((history (minibuffer-history-value)))
    (if (eq history t)
        (user-error "no history")
      (funcall fn regexp n))))

(defadvice! archive-get-lineno--fix (fn &rest args)
  :around #'archive-get-lineno
  (if archive-file-list-start
      (apply fn args)
    0))

;; (span-instrument sclang-handle-command-result)
;; (span-instrument sclang-process-filter)
;; (span-instrument sclang-eval-string)

;; (span-wrap sclang-eval-string (string &optional print-p)
;;   (:sclang-eval-string "%s" (if print-p "print" ""))
;;   (span-notef "string:\n%s" (:unsafe  string)))

(add-hook! 'sclang-mode-hook
  (defun alan-setup-sclang ()
    (kill-local-variable 'mode-line-format)))

;; (span-instrument comint-send-string)
;; (span-instrument comint-send-region)
;; (span-instrument process-send-string)
;; (span-instrument comint-redirect-filter)

;; (span-instrument python-shell-completion-native-turn-on-maybe)
;; (span-instrument python-shell-completion-native-setup)
;; (span-instrument python-shell-completion-native-try)
;; (span-instrument python-shell-accept-process-output)

(add-to-list 'read-only-dir-exclude-list (expand-file-name "elpaca_new/repos/gptel" user-emacs-directory))
(add-to-list 'read-only-dir-exclude-list (expand-file-name "elpaca_new/repos/tree-sitter-langs" user-emacs-directory))

(span-wrap tree-sitter-langs--call (&rest args)
  (:tree-sitter-langs--call (:seq args))
  (span-flush))

(advice-add #'tramp-find-executable :override #'tramp-find-executable--override)
(defun tramp-find-executable--override
    (vec progname dirlist &optional ignore-tilde ignore-path)
  "Search for PROGNAME in $PATH and all directories mentioned in DIRLIST.
First arg VEC specifies the connection, PROGNAME is the program
to search for, and DIRLIST gives the list of directories to
search.  If IGNORE-TILDE is non-nil, directory names starting
with \"~\" will be ignored.  If IGNORE-PATH is non-nil, searches
only in DIRLIST.

Returns the absolute file name of PROGNAME, if found, and nil otherwise.

This function expects to be in the right *tramp* buffer."
  (unless ignore-path
    (setq dirlist (cons "$PATH" dirlist)))
  (when ignore-tilde
    ;; Remove all ~/foo directories from dirlist.
    (let (newdl d)
      (while dirlist
	    (setq d (car dirlist)
	          dirlist (cdr dirlist))
	    (unless (char-equal ?~ (aref d 0))
	      (setq newdl (cons d newdl))))
      (setq dirlist (nreverse newdl))))
  (when (tramp-send-command-and-check
         ;; MODIFIED: previously -pv, i think that was a bug
         vec (format "(unalias %s; %s command -v %s)"
                     progname
                     (if dirlist (concat "PATH=" (string-join dirlist ":")) "")
                     progname))
    (string-trim (tramp-get-buffer-string (tramp-get-connection-buffer vec)))))

(span-instrument internal-default-process-sentinel)

(startup-queue-package 'winner 50)
(eval-after-load! winner
  (winner-mode))

(advice-add #'backtrace-print-frame :around #'span--wrap-backtrace-print-frame)
(defun span--wrap-backtrace-print-frame (orig-fun frame view)
  (span :backtrace-print-frame
    (condition-case err
        (funcall orig-fun frame view)
      (error
       (span-notef "backtrace-print-frame: error")
       (insert (format "error (backtrace-print-frame): %S\n" err))))))

(span-instrument tramp-find-executable)
(span-instrument tramp-get-remote-path :verbose t)
(span-instrument tramp-bundle-read-file-names :verbose t)

(provide 'alan-experimental)
