;; -*- lexical-binding: t -*-

(ignore-errors
  (redisplay))

(require 'cl-lib)

(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

(set-buffer (get-buffer-create " *initialization*"))

(defvar alan-dotemacs-dir)
(when load-file-name
  (setq alan-dotemacs-dir (file-name-directory (file-chase-links load-file-name))))

(add-to-list 'load-path alan-dotemacs-dir)

(require 'alan)

;; (setq native-comp-verbose 1)
;; (require 'alan-test)
