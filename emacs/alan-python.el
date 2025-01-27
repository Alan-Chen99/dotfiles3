;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-treesitter)

(require 'alan-format-all)

(pkg! 'lsp-pyright)
;; (pkg! 'python-black)
;; (pkg! '(python-isort :repo "https://github.com/wyuenho/emacs-python-isort"))

(require-if-is-bytecompile lsp-completion)

(defun alan-redef-python-mode ()
  ;; remove uneeded stuff from python
  (define-derived-mode python-mode prog-mode "python"
    (setq-local comment-start "# ")
    (setq-local comment-start-skip "#+\\s-*"))

  (define-derived-mode inferior-python-mode comint-mode "Inferior Python"
    )

  (seq-doseq (p "_")
    (modify-syntax-entry p "w" python-mode-syntax-table)))

;; copied from python.el
(defvar python--treesit-keywords
  '("as" "assert" "async" "await" "break" "case" "class" "continue" "def"
    "del" "elif" "else" "except" "exec" "finally" "for" "from"
    "global" "if" "import" "lambda" "match" "nonlocal" "pass" "print"
    "raise" "return" "try" "while" "with" "yield"
    ;; These are technically operators, but we fontify them as
    ;; keywords.
    "and" "in" "is" "not" "or" "not in" "is not"))
;; for type_alias_statement
(cl-pushnew "type" python--treesit-keywords :test #'string=)

(alan-redef-python-mode)
(eval-after-load! python
  (alan-redef-python-mode)

  (when (executable-find "ipython")
    ;; https://www.emacswiki.org/emacs/PythonProgrammingInEmacs#h5o-41
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args
          (combine-and-quote-strings
           (list
            "-i"
            "--simple-prompt"
            "--InteractiveShell.display_page=True"
            "--profile-dir"
            ;; TODO
            (expand-file-name "../python/ipython_config" alan-dotemacs-dir))))))


(make-lazy treesit-python-lazy 'python
  (lambda ()
    (treesit-parser-create 'python)
    (setq-local treesit-font-lock-feature-list
                '(( comment definition)
                  ( keyword string type)
                  ( assignment builtin constant decorator
                    escape-sequence number string-interpolation )
                  ( bracket delimiter operator
                    ;; function
                    ;; variable property
                    )))
    (setq-local treesit-font-lock-settings python--treesit-settings)
    (setq-local treesit-defun-type-regexp (rx (or "function" "class")
                                              "_definition"))
    (setq-local treesit-defun-name-function
                #'python--treesit-defun-name)

    (treesit-major-mode-setup)
    (font-lock-refresh-defaults)))

(general-def python-mode-map
  :states 'motion
  "SPC SPC" #'run-python)


(add-hook-once! 'python-mode-hook
  (startup-queue-package 'python 100)
  (startup-queue-package 'tree-sitter-langs 100))

(add-hook! 'python-mode-hook
  (defun alan-setup-python ()
    ;; (setq-local treesit-font-lock-settings python--treesit-settings)
    ;; (treesit-major-mode-setup)

    (setq-local
     alan-font-lock-force-specified t
     syntax-propertize-function nil

     lsp-enable-indentation nil
     indent-tabs-mode nil
     ;; tab-always-indent nil
     tab-always-indent 'complete

     electric-indent-inhibit t
     ;; python-indent-trigger-commands nil
     indent-line-function #'python-indent-line
     indent-region-function #'format-all-region
     ;; lsp-completion-enable nil
     ;; (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point))
     )
    (if (string-equal (file-name-extension (buffer-file-name)) "pyi")
        ;; TODO: isort here
        (setq-local format-all-formatters '(("Python" blackd)))
      (setq-local format-all-formatters '(("Python" blackd isort))))

    (alan-lsp-deferred 'lsp-pyright
      (lsp-completion-mode)
      (setq-local completion-at-point-functions (list #'lsp-completion-at-point)))

    ;; (rainbow-delimiters-enable-lazy)

    (treesit-python-lazy)
    ;; (tree-sitter-hl-mode-lazy)
    ))

(setq python-shell-completion-native-output-timeout 0.1)

(defun alan-python-shell-completion-at-point ()
  (let* ((line-start (line-beginning-position))
         (start (save-excursion
                  (if (not (re-search-backward
                            (python-rx
                             (or whitespace open-paren close-paren
                                 string-delimiter simple-operator))
                            line-start
                            t 1))
                      line-start
                    (forward-char (length (match-string-no-properties 0)))
                    (point))))
         (end (point))
         (process (python-shell-get-process)))
    (when process
      (list
       start end
       (python-shell-completion-native-get-completions
        process
        (buffer-substring-no-properties start end))))))

(add-hook! 'inferior-python-mode-hook
  (defun alan-setup-inferior-python ()
    (add-hook 'completion-at-point-functions
              #'alan-python-shell-completion-at-point nil 'local)))


(alan-set-ignore-debug-on-error #'python-indent-line)

(eval-after-load! lsp-pyright
  (add-to-list 'lsp-file-watch-ignored-directories (rx "/site-packages/"))
  (lsp-register-custom-settings '(("basedpyright.typeCheckingMode" "basic"))))

(provide 'alan-python)
