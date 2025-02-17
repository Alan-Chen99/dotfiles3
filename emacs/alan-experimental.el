;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(sclang :repo "https://github.com/supercollider/scel"
               ;; :files ("el/*.el" "sc/scide_scel/*.sc")
               :files ("el/*.el")
               ))

(require-if-is-bytecompile format-all arc-mode sclang)

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

(span-wrap sclang-eval-string (string &optional print-p)
  (:sclang-eval-string "%s" (if print-p "print" ""))
  (span-notef "string:\n%s" (:unsafe  string)))

(add-hook! 'sclang-mode-hook
  (defun alan-setup-sclang ()
    (kill-local-variable 'mode-line-format)))

(provide 'alan-experimental)
