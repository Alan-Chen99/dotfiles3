;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-comint)

(require-if-is-bytecompile comint)

(add-to-list 'major-mode-remap-alist (cons #'sh-mode #'bash-ts-mode))

(add-to-list 'auto-mode-alist (cons (rx "/.env" eos) #'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "/.envrc" eos) #'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "-hook" eos) #'bash-ts-mode))
(add-to-list 'auto-mode-alist (cons (rx "bashrc") #'bash-ts-mode))

(eval-after-load! sh-script
  (add-hook! 'bash-ts-mode-hook
    (defun alan-setup-bash ()
      (setq-local format-all-formatters '(("Shell" shfmt))))))

(defun alan-use-prompt-dirtrack ()
  (or (not (eq system-type 'windows-nt))
      (file-remote-p default-directory)))

(span-instrument alan-load-shell-bash-fns)
(defun alan-load-shell-bash-fns ()
  (cl-destructuring-bind (code stdout stderr)
      (let* ((f (expand-file-name "emacs-shell-bash.sh" alan-dotemacs-dir))
             (content (with-temp-buffer
                        (insert-file-contents f)
                        (buffer-string)))
             (process-environment nil))
        (setenv "__tmp_EMACS" (concat invocation-directory invocation-name))
        (elpaca-process-call "bash" "-c" content))
    (assert (= code 0))
    (when (length> stderr 0)
      (error "error getting shell envs: %s" stderr))
    (--filter
     (string-match-p (rx bos "BASH_FUNC_") it)
     (read stdout))))

(defvar alan-bash-fns-env-vars nil)
(if (eq system-type 'windows-nt)
    (span-msg "TODO")
  (setq alan-bash-fns-env-vars (alan-load-shell-bash-fns)))

(defadvice! comint-term-environment--inject (fn)
  :around #'comint-term-environment
  (if (alan-use-prompt-dirtrack)
      (let ((ans (funcall fn)))
        (append
         ans alan-bash-fns-env-vars
         (list
          ;; TODO: append instead
          "PROMPT_COMMAND=vterm_prompt_command"
          "INSIDE_EMACS=")))
    (funcall fn)))


(eval-after-load! shell
  ;; https://emacs.stackexchange.com/questions/62418/how-to-change-tramp-default-remote-shell-or-any-of-its-descendants
  ;; TODO: tramp?
  (unless (eq system-type 'windows-nt)
    (setq explicit-shell-file-name "bash")
    ;; (setq explicit-shell-file-name "/bin/bash")
    (setq explicit-bash-args '("-i")))

  (setq-default shell-font-lock-keywords nil)
  ;; (setq-default company-global-modes '(not shell-mode))

  ;; (setq-default shell-fontify-input-enable t)
  (setq-default shell-fontify-input-enable nil)

  (defadvice! shell-reapply-ansi-color-inhibit-readonly (fn)
    :around #'shell-reapply-ansi-color
    (let ((inhibit-read-only t))
      (funcall fn)))

  (add-hook! 'shell-mode-hook
    (defun alan-shell-setup ()
      ;; (setq-local font-lock-fontify-syntactically-function nil)
      (kill-local-variable 'paragraph-separate)
      (kill-local-variable 'paragraph-start)
      (when (alan-use-prompt-dirtrack)
        (shell-dirtrack-mode -1))
      )))


(defun alan-osc-handle-custom-message (_ text)
  ;; (message "received: %S" text)
  (let ((subcmd (aref text 0))
        (cmd (substring text 1)))
    ;; (span-dbgf subcmd cmd)
    (when (= subcmd (eval-when-compile (string-to-char "A")))
      (alan-comint-mark-prompt)
      (let ((remote (file-remote-p default-directory)))
        (if remote
            (setq default-directory
                  (concat (file-remote-p default-directory) (file-name-as-directory cmd)))
          (cd-absolute (file-name-as-directory cmd)))))))


(eval-after-load! comint
  (setf (alist-get "51" (default-value 'ansi-osc-handlers) nil nil #'string=)
        #'alan-osc-handle-custom-message))

(provide 'alan-shell)
