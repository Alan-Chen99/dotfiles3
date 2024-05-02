;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

;; (pkg! 'mistty)
;; (eval-after-load! mistty
;;   (general-def mistty-mode-map
;;     :states 'motion
;;     "SPC l" 'mistty-clear))

(pkg! '(vterm
        :pre-build
        (progn
          ;; otherwise it will prompt for user interaction
          (setq vterm-always-compile-module t)
          (require 'vterm)
          ;; make it rebuild even if already build
          (vterm-module-compile)
          (require 'vterm-module))
        :files (:defaults "vterm-module.so" "etc")))

;; https://gist.github.com/ram535/a2153fb86f33ecec587d593c1c5e1623

(defmacro vterm-with-send-key (expr)
  `(lambda () (interactive) (vterm-send ,expr)))

(eval-after-load! vterm
  (defadvice! alan-vterm--get-shell ()
    :override #'vterm--get-shell
    (let ((f (expand-file-name "etc/emacs-vterm-bash.sh"
                               (file-name-directory (find-library-name "vterm")))))
      (format
       "%s --init-file <(echo %s)"
       vterm-shell
       (shell-quote-argument
        (concat
         ". \"$HOME/.bashrc\"\n"
         (with-temp-buffer
           (insert-file-contents f)
           (buffer-string)))
        t))))


  (defadvice! alan-vterm--get-directory (path)
    :override #'vterm--get-directory
    (span (:vterm--get-directory path)
      (span-dbgf default-directory)
      (span-dbgf (file-local-name default-directory))
      (span-dbgf (file-remote-p default-directory))

      (when (string-match "^\\(.*?\\)@\\(.*?\\):\\(.*?\\)$" path)
        (progn
          (let (
                ;; (user (match-string 1 path))
                ;; (host (match-string 2 path))
                (dir (match-string 3 path)))
            (concat (file-remote-p default-directory) (file-name-as-directory dir)))))))

  ;; (span-instrument vterm--set-directory)
  (clear-and-backup-keymap vterm-mode-map)
  (clear-and-backup-keymap vterm-copy-mode-map)

  (setq vterm-clear-scrollback-when-clearing t)
  (setq vterm-timer-delay 0.01)

  ;; (setq vterm-buffer-name-string "*vterm %s*")

  (evil-set-initial-state 'vterm-mode 'insert)

  (general-def vterm-mode-map
    [remap backward-paragraph] #'vterm-previous-prompt
    [remap forward-paragraph] #'vterm-next-prompt
    [remap alan-completion-at-point] (vterm-with-send-key "<tab>")
    [remap end-of-buffer] #'vterm-reset-cursor-point
    [remap evil-delete-whole-line] (vterm-with-send-key "C-u C-k")

    )

  (general-def vterm-mode-map
    :states 'insert
    [remap self-insert-command] #'vterm--self-insert
    "DEL" #'vterm-send-backspace
    "RET" #'vterm-send-return
    "TAB" #'vterm-send-tab

    [remap previous-line] (vterm-with-send-key "<up>")
    [remap next-line] (vterm-with-send-key "<down>")
    [remap move-beginning-of-line] (vterm-with-send-key "<start>")
    [remap move-end-of-line] (vterm-with-send-key "<end>")

    [remap yank] #'vterm-yank)

  (general-def vterm-mode-map
    :states 'motion
    "SPC l" #'vterm-clear

    "SPC c" (vterm-with-send-key "C-c")
    "<escape>" #'vterm--self-insert)

  (add-hook! 'vterm-mode-hook
    (defun alan-vterm-mode-setup ()
      (span-msg "alan-vterm-mode-setup")
      (setq-local alan--inhibit-motion-state-on-ro t)
      (evil-normal-state)))

  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)

  (evil-collection-require-lazy 'vterm))


(eval-after-load! evil-collection-vterm
  (span-msg "evil-collection-vterm (eval-after-load!)")
  (let ((evil-collection-state-denylist '(insert))
        (evil-collection-key-blacklist (append '("[[" "]]") evil-collection-key-blacklist)))
    (evil-collection-vterm-setup))

  ;; (general-def vterm-mode-map)
  )


(provide 'alan-term)
