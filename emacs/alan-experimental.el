;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(sclang :repo "https://github.com/supercollider/scel"
               ;; :files ("el/*.el" "sc/scide_scel/*.sc")
               :files ("el/*.el")
               ))

(require-if-is-bytecompile
 arc-mode
 comint
 dafny-mode
 format-all
 lsp-mode
 outline
 python
 tq
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

(provide 'alan-experimental)
