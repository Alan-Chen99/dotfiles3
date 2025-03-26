;; -*- lexical-binding: t -*-

(require 'alan-core)

(eval-after-load! rst
  (add-hook! 'rst-mode-hook
    (defun alan-setup-rst ()
      (setq-local format-all-formatters '(("_rst" rstfmt))))))

(eval-after-load! format-all
  (define-format-all-formatter rstfmt
    (:executable)
    (:install "rstfmt")
    (:languages "_rst")
    (:features)
    (:format
     (format-all--buffer-easy "rstfmt"))))

(provide 'alan-rst)
