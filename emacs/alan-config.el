;; -*- lexical-binding: t -*-

(defun ensure-dir (dir)
  (make-directory dir t)
  dir)


(defvar elpaca-directory (expand-file-name "elpaca_new/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name (concat "build-" emacs-version) elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))

(defvar alan-dependency-cache-file (expand-file-name "dependencies-cached.el" elpaca-builds-directory))

;; https://stackoverflow.com/questions/25063073/file-watch-no-such-file-or-directory-for-emacs-autosave-file
(setq-default
 backup-directory-alist
 `(("." . ,(ensure-dir (expand-file-name "backups_new/" user-emacs-directory))))
 auto-save-file-name-transforms
 `((".*" ,(ensure-dir (expand-file-name "auto-save_new/" user-emacs-directory)) t))
 ;; undo-tree-history-directory-alist
 ;; `(("." . ,(ensure-dir (expand-file-name "undo-tree-history/" user-emacs-directory))))
 )

(setq custom-file (expand-file-name "custom-file.el" user-emacs-directory))


;; (setq-default alan-dependency-alist (expand-file-name "elpaca_new/" ry))



(setq-default native-comp-speed 3)

(provide 'alan-config)
