;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-flycheck)
(require 'alan-lsp)

(require-if-is-bytecompile lsp-rust)
(add-to-list 'auto-mode-alist (cons "\\.rs\\'" #'rust-ts-mode))

;; (pkg! 'rust-mode)
;; (setq-default rust-load-optional-libraries nil)

(add-hook! 'rust-ts-mode-hook :depth -100
  (defun alan-setup-rust ()
    (alan-lsp-deferred 'lsp-rust)

    (treesit-font-lock-recompute-features
     '()
     ;; this marks invalid ast with error face
     ;; disabled since markdown docs often have invalid partial ast
     ;; and we dont want them to be all in error face
     ;; error
     '(error))

    ;; (setq-local format-all-formatters '(("Rust" cargo-fmt)))

    ))

(eval-after-load! rust-ts-mode
  (require 'tree-sitter-langs)
  ;; (startup-queue-package 'rust-cargo 94)
  ;; (startup-queue-package 'rust-compile 93)
  ;; (startup-queue-package 'rust-playpen 92)
  ;; (startup-queue-package 'rust-rustfmt 91)

  (setq
   ;; lsp-rust-analyzer-server-display-inlay-hints nil
   ;; lsp-rust-analyzer-server-display-inlay-hints t

   ;; lsp-rust-analyzer-display-lifetime-elision-hints-enable "always"
   lsp-rust-analyzer-display-lifetime-elision-hints-enable "never"
   lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t

   lsp-rust-analyzer-binding-mode-hints t
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-closing-brace-hints t
   lsp-rust-analyzer-display-closure-return-type-hints t
   lsp-rust-analyzer-display-parameter-hints t
   lsp-rust-analyzer-display-reborrow-hints "never"

   ;; lsp-rust-analyzer-cargo-watch-command "check"
   lsp-rust-analyzer-cargo-watch-command "clippy"
   ;; lsp-rust-analyzer-cargo-watch-args ["--tests" "--" "-W" "clippy::pedantic"]
   ;; lsp-rust-analyzer-cargo-watch-args ["--" "-W" "clippy::pedantic"]
   lsp-rust-analyzer-cargo-watch-args []
   lsp-rust-clippy-preference "on"

   lsp-rust-all-targets nil
   ;; lsp-rust-all-features t
   lsp-rust-all-features nil
   )

  ;; (seq-doseq (p "_")
  ;;   (modify-syntax-entry p "w" rust-mode-syntax-table))
  (general-def rust-ts-mode-map
    :states 'motion
    ;; "SPC SPC" #'rust-run
    ;; "SPC c" #'rust-compile
    "SPC a" #'lsp-rust-analyzer-expand-macro
    "SPC t" #'lsp-rust-analyzer-syntax-tree
    "SPC i" #'lsp-rust-analyzer-view-hir
    [remap evil-join] #'lsp-rust-analyzer-join-lines

    [remap evil-indent] #'lsp-format-buffer
    )
  ;; (general-def rust-mode-map
  ;;   :states 'normal
  ;;   "'" #'rust-format-buffer)

  (setq-default lsp-rust-analyzer-macro-expansion-method #'lsp-rust-analyzer-macro-expansion-custom))

(defun lsp-rust-analyzer-macro-expansion-custom (result)
  "Default method for displaying macro expansion."
  (let* ((root (lsp-workspace-root default-directory))
         (buf (get-buffer-create (get-buffer-create (format "*rust-analyzer macro expansion %s*" root)))))
    (with-current-buffer buf
	  (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (lsp--render-string result "rust"))
        (rust-ts-mode))
      (read-only-mode))
    (pop-to-buffer buf)))

(provide 'alan-rust)
