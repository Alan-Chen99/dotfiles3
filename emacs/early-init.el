;; -*- lexical-binding: t -*-

(require 'cl-lib)
(cl-assert (version<= "29" emacs-version))

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(set-buffer (get-buffer-create " *initialization*"))

(defvar alan-dotemacs-dir)
(when load-file-name
  (setq alan-dotemacs-dir (file-name-directory (file-chase-links load-file-name))))

(add-to-list 'load-path alan-dotemacs-dir)

(defvar alan-real-early-init nil)

(let ((alan-real-early-init t))
  (require 'alan-early-init))
