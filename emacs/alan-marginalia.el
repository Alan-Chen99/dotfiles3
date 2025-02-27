;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'marginalia
  ;; TODO: previously here, do i need this?
  ;; for truncate-string-to-width
  ;; (startup-queue-package 'mule-util 50)
  (startup-queue-package 'marginalia 50))

(defun marginalia--library-doc-ignore (_file)
  (progn ""))
(eval-after-load! marginalia
  (when (eq system-type 'windows-nt)
    (advice-add #'marginalia--library-doc :override #'marginalia--library-doc-ignore))
  (marginalia-mode))

(defadvice! marginalia--library-doc--set-dir (orig-fn &rest args)
  :around #'marginalia--library-doc
  (let ((default-directory "/"))
    (apply orig-fn args)))

(provide 'alan-marginalia)
