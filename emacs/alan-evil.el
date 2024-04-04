;; -*- lexical-binding: t -*-

(require 'alan-core)

(require 'evil)
(require 'general)

(setq
 evil-move-cursor-back nil
 evil-move-beyond-eol t
 evil-mode-line-format nil
 ;; this is extremely slow
 evil-indent-convert-tabs nil)

(add-hook! 'read-only-mode-hook
  (defun read-only-toggle-evil-state ()
    ;; (span-notef "read-only-mode-hook: %S" buffer-read-only)
    (if buffer-read-only
        (unless (or (eq evil-state 'emacs) (eq evil-state 'motion))
          (evil-motion-state))
      (when (eq evil-state 'motion)
        (evil-initialize-state)))))

(defadvice! evil-initial-state-for-buffer-adv (orig-fn &optional buffer)
  :around #'evil-initial-state-for-buffer
  (span (:evil-initial-state-for-buffer (buffer-name buffer))
    ;; (span-flush)
    (let ((res (funcall orig-fn buffer)))
      (when (and
             (not (eq res 'emacs))
             (buffer-local-value 'buffer-read-only (or buffer (current-buffer))))
        (setq res 'motion))
      res)))

(span-wrap evil-initialize-state)
(span-wrap evil-initialize)
(span-wrap evil-mode-enable-in-buffers)
(span-wrap evil-mode)

(span-wrap evil-normalize-keymaps)

(defadvice! evil--preceding-sexp-override (command &rest args)
  :override #'evil--preceding-sexp
  (if (or (evil-normal-state-p) (evil-motion-state-p))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

;; (setq evil-normal-state-modes (append evil-emacs-state-modes evil-normal-state-modes))

(evil-select-search-module 'evil-search-module 'evil-search)
(setq case-replace nil)
(setq evil-ex-search-case 'insensitive)

;; TODO?
(defface my-evil-hl-face '((t :inherit (match evil-ex-lazy-highlight))) "mandatory docstring" :group 'my-faces)
(advice-add 'evil-ex-make-hl :filter-args (lambda (args) (append args '(:face my-evil-hl-face))))

(unless evil-mode
  (evil-mode t))

(add-hook! 'alan-end-of-init-hook :depth 100
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (evil-normalize-keymaps)))
   (buffer-list)))

(advice-add #'evil-refresh-mode-line :override #'ignore)


(define-advice evil-execute-repeat-info
    (:around (orig-fun repeat-info) fix-key-translations)
  ;; TODO: does this actually work
  (let
      (
       (input-decode-map nil)
       (local-function-key-map nil)
       (key-translation-map nil))
    (funcall orig-fun repeat-info)))



(autoload 'ffap-guesser "ffap")
(autoload 'ffap-url-p "ffap")

(startup-queue-package 'ffap 0)
(defun evil-goto-definition-visit-url (_string position)
  (span :evil-goto-definition-visit-url
    ;; (span-flush)
    ;; find-file-at-point
    (save-excursion
      (goto-char position)
      (when-let ((filename (ffap-guesser)))
        ;; should not identify remote file at all but just in case
        ;; (not (file-remote-p filename))
        ;; TODO: make this work with url
        ;; (when (file-exists-p filename)
        ;; TODO: HACK
        (when (or (file-exists-p filename) (ffap-url-p filename))
          (find-file-at-point filename))
        t)
      ;; t))
      ;; (when-let ((url (thing-at-point 'url t)))
      ;;  (browse-url url))
      )))

(setq evil-goto-definition-functions
      (list
       #'evil-goto-definition-visit-url
       #'evil-goto-definition-xref
       ;; evil-goto-definition-imenu
       ;; evil-goto-definition-semantic
       ;; evil-goto-definition-search
       ))

(evil-set-command-property 'evil-jump-item :jump nil)
(evil-set-command-property 'evil-forward-paragraph :jump nil)
(evil-set-command-property 'evil-backward-paragraph :jump nil)

(alan-set-ignore-debug-on-error #'evil-window-delete)

;; TODO: needed since this "rethrows" errors, but maybe not the best idea
(alan-set-ignore-debug-on-error #'evil-motion-range)

(alan-set-ignore-debug-on-quit #'evil-operator-range)
(alan-set-ignore-debug-on-quit #'evil-ex-start-search)

(provide 'alan-evil)
