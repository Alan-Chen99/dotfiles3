;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'nix-mode)

(defun alan-add-to-directory-abbrev-alist (x)
  ;; TODO: this can be faster
  (add-to-list 'directory-abbrev-alist
               (cons (rx-to-string `(seq bos ,(file-truename x)) t) (expand-file-name x))))

(defun alan-add-to-directory-abbrev-alist-rec (path)
  (dolist (x (directory-files-recursively path "" t))
    (when (file-directory-p x)
      (alan-add-to-directory-abbrev-alist x)))  )

;; so that when visiting the source of say nixpkgs,
;; it automatically changes to ~/.nix-profile/repos/nixpkgs/ if possible
(condition-case err
    (let (file-name-handler-alist)
      (dolist (x (directory-files "~/.nix-profile/repos/" t (rx bos (not "."))))
        (alan-add-to-directory-abbrev-alist x))
      (alan-add-to-directory-abbrev-alist-rec "~/.nix-profile")
      (alan-add-to-directory-abbrev-alist-rec "~/aliases/music/"))
  (error
   (message "error adding nix repos to directory-abbrev-alist: %S" err)
   nil))

(eval-after-load! nix-mode
  (modify-syntax-entry (string-to-char "-") "w" nix-mode-syntax-table)
  (modify-syntax-entry (string-to-char "_") "w" nix-mode-syntax-table)

  (eval-after-load! lsp-mode
    ;; TODO: where did i get this?
    ;; https://github.com/nix-community/nixd/blob/main/nixd/docs/editor-setup.md
    ;; ^ claims a simpler way to write this
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () "nixd"))
      :major-modes '(nix-mode)
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                                            (lsp--set-configuration
                                             (lsp-configuration-section "nixd"))))
      :synchronize-sections '("nixd")
      :server-id 'nix-nixd)))

  (add-hook! 'nix-mode-hook
    (defun nix-mode-setup ()
	  (setq-local format-all-formatters '(("Nix" alejandra)))
      (alan-lsp-deferred 'lsp-mode))))



(provide 'alan-nix)
