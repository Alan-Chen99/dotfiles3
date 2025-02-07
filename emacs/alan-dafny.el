;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-format-all)

(pkg! 'boogie-friends)

(require-if-is-bytecompile lsp-dafny)

(eval-after-load! dafny-mode
  (add-hook! 'dafny-mode-hook
    (defun alan-setup-dafny ()
      (setq-local format-all-formatters '(("_dafny" dafny)))

      (alan-lsp-deferred 'lsp-dafny
        ;; (lsp-completion-mode)
        ;; (setq-local completion-at-point-functions (list #'lsp-completion-at-point))
        )

      ))

  )

(defadvice! lsp-dafny--server-command--overwrite ()
  :override #'lsp-dafny--server-command
  `("dafny" "server"
    ,(pcase lsp-dafny-server-automatic-verification-policy
       ((and policy (or `never `onchange `onsave))
        (format "--documents:verify=%S" policy))
       (other (user-error "Invalid value %S in \
`lsp-dafny-server-automatic-verification-policy'" other)))
    ,@(pcase lsp-dafny-server-verification-time-limit
        (`nil nil)
        ((and limit (pred integerp))
         (list (format "--verifier:timelimit=%d" limit)))
        (other (user-error "Invalid value %S in \
`lsp-dafny-server-verification-time-limit'" other)))
    ,@lsp-dafny-server-args))

(advice-add #'boogie-friends-setup-prettify :override #'ignore)


;; (defun lsp-dafny--server-command ()
;;   "Compute the command to run Dafny's LSP server."
;;   )

;; (defadvice! lsp-dafny--installed-executable--overwrite (executable)
;;   :override #'lsp-dafny--installed-executable
;;   (pcase executable
;;     ("DafnyLanguageServer" (expand-file-name "../../lib/Dafny/DafnyServer" (executable-find "dafny")))
;;     (_ (error "Unknown executable %S" executable))))


(provide 'alan-dafny)
