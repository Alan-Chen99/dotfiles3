;; -*- lexical-binding: t -*-

(require 'alan-core)

(require 'format-all-autoloads)
(startup-queue-package 'format-all 40)


(general-def [remap evil-indent] #'format-all-region-or-buffer)


(eval-after-load! format-all
  ;; (alan-set-ignore-debug-on-error #'format-all--prompt-for-formatter)
  (add-to-list 'debug-ignored-errors 'format-all-executable-not-found)

  (defadvice! alan-format-all--language-id-buffer ()
    :before-until #'format-all--language-id-buffer
    (or
     (and (eq major-mode 'nix-ts-mode) "Nix")

     (and (eq major-mode 'LilyPond-mode) "_lilypond")
     (and (eq major-mode 'dafny-mode) "_dafny")
     (and (eq major-mode 'rst-mode) "_rst")))

  (define-format-all-formatter python-ly
    (:executable "ly")
    (:install)
    (:languages "_lilypond")
    (:features)
    (:format (format-all--buffer-easy executable "reformat")))

  (define-format-all-formatter schemat
    (:executable "schemat")
    (:install)
    (:languages "Scheme")
    (:features)
    (:format (format-all--buffer-easy executable)))

  (define-format-all-formatter scmindent
    (:executable "scmindent")
    (:install)
    (:languages "Scheme")
    (:features)
    (:format (format-all--buffer-easy executable))))

(provide 'alan-format-all)
