;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(tsc :files ("core/*.el" "core/Cargo.toml" "core/Cargo.lock" "core/src" "core/tsc-dyn.so")))
(pkg! 'tree-sitter)
(pkg! '(tree-sitter-langs
        :depth nil
        :remotes ("alan" :repo "Alan-Chen99/tree-sitter-langs" :branch "master"))
  (startup-queue-package 'tree-sitter-langs 0))

(pkg! 'evil-textobj-tree-sitter
  (startup-queue-package 'evil-textobj-tree-sitter 0))

(pkg! '(ts-movement
        :host github :repo "haritkapadia/ts-movement")
  (startup-queue-package 'ts-movement 0))

;; (eval-after-load! tree-sitter-hl
;;   (add-hook! tree-sitter-hl-mode-hook
;;     (defun alan-tree-sitter-hl-mode-setup ()
;;       (if tree-sitter-hl-mode
;;           (setq-local font-lock-fontify-syntactically-function nil)
;;         (kill-local-variable 'font-lock-fontify-syntactically-function)))))


(setq-default treesit-font-lock-level 4)

(eval-after-load! tree-sitter-langs
  (require 'treesit)

  (setq treesit-extra-load-path
        (list (expand-file-name
               "bin/"
               (file-name-directory (file-truename (locate-library "tree-sitter-langs.el"))))))

  (setq
   treesit-load-name-override-list
   (mapcar
    (lambda (x)
      (list x (symbol-name x) (format "tree_sitter_%s" (symbol-name x))))
    '(
      bash
      cmake
      cpp
      dockerfile
      json
      latex
      lilypond
      make
      nix
      python
      toml
      typescript
      tsx
      yaml
      ))))

(eval-after-load! evil-textobj-tree-sitter
  (defadvice! alan-evil-textobj-tree-sitter--range (&rest _)
    :before #'evil-textobj-tree-sitter--range
    (unless (or (bound-and-true-p treesit-font-lock-settings)
                (bound-and-true-p tree-sitter-mode))
      (user-error "tree sitter not avaiable here")))

  (defadvice! alan-use-builtin-treesitter ()
    :before-until #'evil-textobj-tree-sitter--use-builtin-treesitter
    (bound-and-true-p treesit-font-lock-settings))

  (general-def evil-inner-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (general-def evil-outer-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
    "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  (alan-set-ignore-debug-on-error #'evil-textobj-tree-sitter--message-not-found)

  )

(defun alan-ts-block ()
  (let ((node (treesit-node-on (point) (point))))
    (count-lines
     (treesit-node-start node)
     (treesit-node-end node))))

(eval-after-load! ts-movement
  (require 'alan-tsm))


(provide 'alan-treesitter)
