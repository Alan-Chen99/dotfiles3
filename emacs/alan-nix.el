;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'nix-ts-mode
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode)))

(defun alan-add-to-directory-abbrev-alist (x x-truename)
  ;; TODO: this can be faster
  ;; (setq x (file-name-as-directory (expand-file-name x "/")))
  (setq x (expand-file-name x "/"))
  (add-to-list 'directory-abbrev-alist
               (cons (rx-to-string `(seq bos ,x-truename) t) x))

  ;; (unless (string= (file-chase-links x) (file-truename x))
  ;;   (span-dbgf "not equal!" x (file-chase-links x) (file-truename x)))
  ;; (add-to-list 'directory-abbrev-alist
  ;;              (cons (rx-to-string `(seq bos ,(file-chase-links x)) t) x))
  )

(defvar nix-directory-cache-old
  (force-noerr
   (alan-read-elisp-data-file nix-directory-cache-file)))
(defvar nix-directory-cache nil)

(defun nix-list-directory-rec (path)
  (setq path (file-truename path))
  (cl-assert (or (string-prefix-p "/nix/" path) (string-prefix-p "/gnu/" path)))
  (let ((ans (alist-get path nix-directory-cache-old nil nil #'string=)))
    (unless ans
      (dolist (x (directory-files-recursively path "" t))
        (when (file-directory-p x)
          (push (cons (file-relative-name x path) (file-truename x)) ans))))
    (setf (alist-get path nix-directory-cache) ans)
    ans))


(defun alan-add-to-directory-abbrev-alist-rec (path)
  (alan-add-to-directory-abbrev-alist path (file-truename path))
  (dolist (x (nix-list-directory-rec path))
    (alan-add-to-directory-abbrev-alist (expand-file-name (car x) path) (cdr x)))
  )

;; so that when visiting the source of say nixpkgs,
;; it automatically changes to ~/.nix-profile/repos/nixpkgs/ if possible
(condition-case err
    (let (file-name-handler-alist)
      ;; (dolist (x (directory-files "~/.nix-profile/repos/" t (rx bos (not "."))))
      ;;   (alan-add-to-directory-abbrev-alist x))
      (alan-add-to-directory-abbrev-alist-rec "~/.nix-profile/repos")
      (alan-add-to-directory-abbrev-alist-rec "~/.nix-profile")
      (alan-add-to-directory-abbrev-alist-rec "~/aliases/music")
      (alan-add-to-directory-abbrev-alist-rec "~/aliases/physics"))
  (error
   (message "error adding nix repos to directory-abbrev-alist: %S" err)
   nil))

(condition-case err
    (let (file-name-handler-alist)
      (alan-add-to-directory-abbrev-alist-rec "~/.guix-profile/"))
  (error
   (message "error adding guix to directory-abbrev-alist: %S" err)
   nil))

(alan-startup-schedual-fn
 -1000
 (lambda ()
   (unless (equal nix-directory-cache-old nix-directory-cache)
     (span :update-nix-directory-cache
       (span-notef "update-nix-directory-cache")
       (alan-write-elisp-data-file nix-directory-cache-file nix-directory-cache)))))

(eval-after-load! nix-ts-mode

  (modify-syntax-entry (string-to-char "-") "w" nix-ts-mode--syntax-table)
  (modify-syntax-entry (string-to-char "_") "w" nix-ts-mode--syntax-table)

  (eval-after-load! lsp-mode
    ;; TODO: where did i get this?
    ;; https://github.com/nix-community/nixd/blob/main/nixd/docs/editor-setup.md
    ;; ^ claims a simpler way to write this
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (lambda () "nixd"))
      :major-modes '(nix-ts-mode)
      :initialized-fn (lambda (workspace)
                        (with-lsp-workspace workspace
                                            (lsp--set-configuration
                                             (lsp-configuration-section "nixd"))))
      :synchronize-sections '("nixd")
      :server-id 'nix-nixd)))

  (add-hook! 'nix-ts-mode-hook
    (defun nix-mode-setup ()
	  (setq-local format-all-formatters '(("Nix" alejandra)))
      (alan-lsp-deferred 'lsp-mode))))



(provide 'alan-nix)
