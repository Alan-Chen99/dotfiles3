;; -*- lexical-binding: t -*-

(require 'alan-core)

(require 'format-all-autoloads)
(startup-queue-package 'format-all 40)


(general-def [remap evil-indent] #'format-all-region-or-buffer)

(eval-after-load! format-all
  (alan-set-ignore-debug-on-error #'format-all--prompt-for-formatter)


  (defadvice! alan-format-all--language-id-buffer ()
    :before-until #'format-all--language-id-buffer
    (or
     (and (eq major-mode 'LilyPond-mode) "_lilypond")
     (and (eq major-mode 'nix-ts-mode) "Nix")
     (and (eq major-mode 'dafny-mode) "_dafny")))

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
    (:format (format-all--buffer-easy executable)))

  (define-format-all-formatter dafny
    (:executable "dafny")
    (:install)
    (:languages "_dafny")
    (:features)
    (:format
     ;; (with-temp-file
     ;;     (insert (buffer-string)))
     (format-all--buffer-easy
      executable
      "format"
      ;; "--stdin"
      (buffer-file-name)
      "--print"))))

(provide 'alan-format-all)
