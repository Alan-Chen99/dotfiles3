;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'marginalia
  ;; TODO: previously here, do i need this?
  ;; for truncate-string-to-width
  ;; (startup-queue-package 'mule-util 50)
  (startup-queue-package 'marginalia 50))

(eval-after-load! marginalia
  (marginalia-mode))

(provide 'alan-marginalia)
