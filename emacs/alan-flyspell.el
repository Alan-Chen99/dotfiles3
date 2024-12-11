;; -*- lexical-binding: t -*-

(require 'alan-core)

(require-if-is-bytecompile popup)

(eval-after-load! flyspell
  ;; https://www.emacswiki.org/emacs/FlySpell
  (setq flyspell-issue-message-flag nil)
  (defun flyspell-toggle ()
    (interactive)
    (if flyspell-mode
        (flyspell-mode -1)
      (flyspell-mode 1)
      (flyspell-buffer)))
  (defalias 'fs 'flyspell-toggle)

  (clear-and-backup-keymap flyspell-mode-map)

  (general-define-key
   :definer 'minor-mode
   :states 'motion
   :keymaps 'flyspell-mode
   [remap evil-ex-search-next] #'evil-next-flyspell-error
   [remap evil-ex-search-previous] #'evil-prev-flyspell-error)

  (general-define-key
   :definer 'minor-mode
   :states 'normal
   :keymaps 'flyspell-mode
   [remap evil-goto-definition] #'flyspell-correct-word-before-point)

  ;; https://emacs.stackexchange.com/questions/37417/flyspell-in-terminal
  (defun flyspell-emacs-popup-textual (_event poss _word)
    "A textual flyspell popup menu."
    (require 'popup)
    (let* ((corrects (if flyspell-sort-corrections
                         (sort (car (cdr (cdr poss))) 'string<)
                       (car (cdr (cdr poss)))))
           (cor-menu (if (consp corrects)
                         (mapcar (lambda (correct)
                                   (list correct correct))
                                 corrects)
                       '()))
           (affix (car (cdr (cdr (cdr poss)))))
           (show-affix-info nil)
           (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                       (list
                                        (list (concat "Save affix: " (car affix))
                                              'save)
                                        '("Accept (session)" session)
                                        '("Accept (buffer)" buffer))
                                     '(("Save word" save)
                                       ("Accept (session)" session)
                                       ("Accept (buffer)" buffer)))))
                         (if (consp cor-menu)
                             (append cor-menu (cons "" save))
                           save)))
           (menu (mapcar
                  (lambda (arg) (if (consp arg) (car arg) arg))
                  base-menu)))
      (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))

  (fset #'flyspell-emacs-popup 'flyspell-emacs-popup-textual))

(provide 'alan-flyspell)
