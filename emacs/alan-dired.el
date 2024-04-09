;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)


;; https://emacs.stackexchange.com/questions/24285/how-to-dired-find-file-for-only-directories-in-dired

;; (add-hook! 'dired-mode-hook
;;   (defun alan-dired-setup ()
;;     (setq-local modeline-filename
;;                 '(
;;                   (modeline-computed-filename nil (:eval (ignore (modeline-calc-filename-noexcept))))
;;                   (:propertize modeline-project-name face modeline-project-name)
;;                   (:propertize modeline-file-path face modeline-file-path)))))

(eval-after-load! dired
  (setq dired-deletion-confirmer #'y-or-n-p)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)

  (clear-and-backup-keymap dired-mode-map)
  ;; (add-hook 'dired-mode-hook 'evil-motion-state)
  (add-to-list 'evil-motion-state-modes 'dired-mode)
  (defun dired-find-alternate-directory ()
    (interactive)
    (let ((find-file-run-dired t)
          (file (dired-get-file-for-visit)))
      (when (file-directory-p file)
        (set-buffer-modified-p nil)
        (find-alternate-file file))))

  (general-def dired-mode-map
    :states 'motion
    [remap previous-line] #'dired-previous-line
    [remap next-line] #'dired-next-line

    "o" #'dired-find-file-other-window
    ;; "+" 'dired-display-file ;; todo, readonly and fix buffer
    "RET" #'dired-find-file
    "<right>" 'dired-find-alternate-directory

    "<left>" #'dired-up-directory
    "<up>" #'dired-prev-dirline
    "<down>" #'dired-next-dirline

    ;; "d" 'dired-do-flagged-delete
    "d" #'dired-flag-file-deletion
    "c" #'dired-unmark
    "S-DEL" #'dired-do-delete

    ;; https://stackoverflow.com/questions/30865056/emacs-dired-reverse-date-order
    "SPC d" #'dired-sort-toggle-or-edit)

  (general-def 'visual dired-mode-map
    [remap previous-line] #'evil-previous-visual-line
    [remap next-line] #'evil-next-visual-line)

  ;; (put 'dired-find-alternate-file 'disabled nil)
  (alan-set-ignore-debug-on-error #'dired-next-dirline)

  ;; premission
  ;; TODO: after dired-find-alternate-directory fails for permission, dired stuff dont work
  (alan-set-ignore-debug-on-error #'dired-insert-directory)

  )

(provide 'alan-dired)
