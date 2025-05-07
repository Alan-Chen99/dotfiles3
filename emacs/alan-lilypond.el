;; -*- lexical-binding: t -*-

(require 'alan-core)

(eval-and-compile
  (ignore-errors
    (add-to-list 'load-path
                 (expand-file-name "../../share/emacs/site-lisp/"
                                   (file-truename (executable-find "lilypond"))))

    (autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
    (add-to-list 'auto-mode-alist '("\\.ly\\'" . LilyPond-mode))
    (add-to-list 'auto-mode-alist '("\\.ily\\'" . LilyPond-mode))))


;; (autoload 'LilyPond-mode "lilypond-mode")
;; (add-to-list 'auto-mode-alist (cons (rx ".ly" eos) #'LilyPond-mode))

;; (require 'lilypond-mode)

(eval-after-load! lilypond-mode
  ;; (require 'alan-lilypond-ts)
  (setq LilyPond-fancy-comments nil)
  (advice-add #'LilyPond-mode-context-set-syntax-table :override #'ignore)
  (advice-add #'LilyPond-mode-set-syntax-table :override #'ignore)

  (seq-doseq (p ".-")
    (modify-syntax-entry p "w" LilyPond-mode-syntax-table))

  (add-hook! 'LilyPond-mode-hook
    (defun alan-LilyPond-mode-setup ()
      ;; TODO: why isnt it the syntax table by default?
      (set-syntax-table LilyPond-mode-syntax-table)

      ;; (setq-local font-lock-defaults nil)
      ;; (setq-local syntax-propertize-function nil)
      ;; (setq-local alan-font-lock-force-specified t)
      ;; (tree-sitter-hl-mode-lazy)
      ;; (setq-local treesit-font-lock-settings lilypond--treesit-settings)
      ;; (setq-local treesit-font-lock-feature-list
      ;;             '(( custom comment definition)
      ;;               ( keyword string type)
      ;;               ( assignment builtin constant decorator
      ;;                 escape-sequence number string-interpolation )
      ;;               ( bracket delimiter function operator variable property)))

      ;; (setq-local syntax-propertize-function nil)
      ;; (treesit-major-mode-setup-lazy)

      ;; (treesit-parser-create 'lilypond)

      ;; (treesit-major-mode-setup)
      ;; (font-lock-refresh-defaults)
      )

    )


  )


(provide 'alan-lilypond)
