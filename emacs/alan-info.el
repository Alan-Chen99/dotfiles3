;; -*- lexical-binding: t -*-

(require 'alan-core)

;; https://stackoverflow.com/questions/1921049/how-to-open-info-file-in-emacs-in-info-mode
(add-to-list 'auto-mode-alist `("\\.info\\'" . ,#'Info-on-current-buffer))

(eval-after-load! info
  (general-def Info-mode-map
    [remap evil-jump-backward] #'Info-history-back
    [remap evil-jump-forward] #'Info-history-forward))

(provide 'alan-info)
