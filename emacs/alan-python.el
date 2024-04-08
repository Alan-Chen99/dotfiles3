;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-treesitter)

(require 'alan-format-all)

(pkg! 'lsp-pyright)
;; (pkg! 'python-black)
;; (pkg! '(python-isort :repo "https://github.com/wyuenho/emacs-python-isort"))

(declare-function lsp-completion-at-point "lsp-completion")
(declare-function lsp-completion-mode "lsp-completion")

(defun alan-redef-python-mode ()
  ;; remove uneeded stuff from python
  (define-derived-mode python-mode prog-mode "python"
    (setq-local comment-start "# ")
    (setq-local comment-start-skip "#+\\s-*"))

  (seq-doseq (p "_")
    (modify-syntax-entry p "w" python-mode-syntax-table))

  )
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
            (expand-file-name "python/ipython_config" "/home/alan/dotfiles"))))))


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
     tab-always-indent nil

     electric-indent-inhibit t
     ;; python-indent-trigger-commands nil
     indent-line-function #'python-indent-line
     indent-region-function #'format-all-region
     ;; lsp-completion-enable nil
     ;; (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point))
     )

    (setq-local format-all-formatters '(("Python" black isort)))

    (alan-lsp-deferred 'lsp-pyright
      (lsp-completion-mode)
      (setq-local completion-at-point-functions (list #'lsp-completion-at-point)))

    ;; (rainbow-delimiters-enable-lazy)

    (treesit-python-lazy)
    ;; (tree-sitter-hl-mode-lazy)
    ))



(provide 'alan-python)
