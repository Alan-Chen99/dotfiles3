;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-flycheck)
(require 'alan-lsp)

(pkg! 'rust-mode)

(make-lazy rust-mode-lazy 'rust-mode 'rust-mode)
(add-to-list 'auto-mode-alist (cons "\\.rs\\'" 'rust-mode-lazy))

(setq-default rust-load-optional-libraries nil)

(add-hook! 'rust-mode-hook :depth -100

  (alan-lsp-deferred 'lsp-rust)

  ;; (setq-local font-lock-defaults '(t nil))

  ;; (add-hook! 'lsp-mode-hook :local
  ;;   (alan-flycheck-deferred (if lsp-mode 1 -1)))

  ;; (my-lsp-deferred 'lsp-rust)
  (tree-sitter-hl-mode-lazy)

  ;; (tree-sitter-indent-mode-lazy)
  )

(eval-after-load! rust-mode
  (startup-queue-package 'rust-cargo 94)
  (startup-queue-package 'rust-compile 93)
  (startup-queue-package 'rust-playpen 92)
  (startup-queue-package 'rust-rustfmt 91)

  (setq
   rust-font-lock-keywords nil
   ;; lsp-rust-analyzer-server-display-inlay-hints nil
   lsp-rust-analyzer-server-display-inlay-hints t

   ;; lsp-rust-analyzer-display-lifetime-elision-hints-enable "always"
   lsp-rust-analyzer-display-lifetime-elision-hints-enable "never"
   lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t

   lsp-rust-analyzer-binding-mode-hints t
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-closing-brace-hints t
   lsp-rust-analyzer-display-closure-return-type-hints t
   lsp-rust-analyzer-display-parameter-hints t
   lsp-rust-analyzer-display-reborrow-hints "never"

   lsp-rust-analyzer-cargo-watch-command "clippy"
   lsp-rust-analyzer-cargo-watch-args ["--tests" "--" "-W" "clippy::pedantic"]
   ;; lsp-rust-analyzer-cargo-watch-args ["--" "-W" "clippy::pedantic"]
   lsp-rust-clippy-preference "on"

   lsp-rust-all-targets nil
   ;; lsp-rust-all-features t
   lsp-rust-all-features nil
   )

  (seq-doseq (p "_")
    (modify-syntax-entry p "w" rust-mode-syntax-table))
  (general-def rust-mode-map
    :states 'motion
    "SPC SPC" 'rust-run
    "SPC c" #'rust-compile
    "SPC a" #'lsp-rust-analyzer-expand-macro
    "SPC t" #'lsp-rust-analyzer-syntax-tree
    "SPC i" #'lsp-rust-analyzer-view-hir
    [remap evil-join] 'lsp-rust-analyzer-join-lines
    )
  (general-def rust-mode-map
    :states 'normal
    "'" #'rust-format-buffer)

  (defun lsp-rust-analyzer-macro-expansion-custom (result)
    "Default method for displaying macro expansion."
    (let* ((root (lsp-workspace-root default-directory))
           (buf (get-buffer-create (get-buffer-create (format "*rust-analyzer macro expansion %s*" root)))))
      (with-current-buffer buf
	    (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (lsp--render-string result "rust"))
          (rust-mode))
        (read-only-mode))
      (pop-to-buffer buf)))

  (setq-default lsp-rust-analyzer-macro-expansion-method #'lsp-rust-analyzer-macro-expansion-custom))

(provide 'alan-rust)
