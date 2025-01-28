;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'evil-surround
  (startup-queue-package 'evil-surround 70))


(defun alan--evil-surround-read-char-one ()
  ;; TODO: whatever key binding is active at the moment leaks through this
  ;; should prob set current keymap to nothing but keep translation maps
  (let ((ans (read-key-sequence-vector nil 'continue-echo)))
    (span-notef ans)
    (unless (length= ans 1)
      (user-error "not a single key: %S" ans))
    (setq ans (aref ans 0))
    (unless (characterp ans)
      (user-error "not a character: %S" ans))
    ans))

(eval-after-load! evil-surround
  (add-to-list 'evil-surround-operator-alist '(evil-change-without-yank . change))
  (global-evil-surround-mode))

(defadvice! alan--evil-surround-read-char ()
  :override #'evil-surround-read-char
  (if (evil-operator-state-p)
      (save-restriction (widen) (alan--evil-surround-read-char-one))
    (alan--evil-surround-read-char-one)))

(span-wrap evil-surround-edit (op)
  (:evil-surround-edit op))
(span-wrap evil-Surround-edit (op)
  (:evil-Surround-edit op))

(provide 'alan-evil-surround)
