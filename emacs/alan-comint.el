;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)


(defvar-keymap alan-comint-output-map)

(declare-function evil-comint-next-prompt "alan-comint")
(declare-function evil-comint-previous-prompt "alan-comint")

;; https://superuser.com/questions/868680/make-command-line-unmodifiable-in-aquamacs
;; https://snarfed.org/why_i_run_shells_inside_emacs
(eval-after-load! comint
  ;; https://stackoverflow.com/questions/26985772/fast-emacs-shell-mode
  (setq comint-move-point-for-output nil)
  (setq comint-scroll-show-maximum-output nil)

  (setq comint-buffer-maximum-size 10000)
  (add-to-list 'comint-output-filter-functions #'comint-truncate-buffer)

  (setq comint-use-prompt-regexp nil)
  (setq comint-input-ignoredups nil)

  (setq comint-terminfo-terminal "xterm")
  (setq comint-pager "cat")

  ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
  (setq comint-highlight-input nil)
  ;; (face-spec-set 'comint-highlight-prompt '((t)) 'face-defface-spec)

  ;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L26-L48
  ;; https://emacs.stackexchange.com/questions/13592/how-does-comint-mode-override-beginning-of-line-behavior-i-want-same-functional
  ;; Make processes’ outputs read-only. The prompt is easy.

  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Shell-echo.html
  (setq-default comint-prompt-read-only t)

  (general-def alan-comint-output-map
    [remap comint-send-input] #'undefined)

  ;; https://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode

  (advice-add #'comint-output-filter :override 'alan-comint-output-filter)
  (defun alan-comint-output-filter (process string)
    (span :alan-comint-output-filter
      (let ((oprocbuf (process-buffer process)))
        ;; First check for killed buffer or no input.
        (when (and string oprocbuf (buffer-name oprocbuf))
          (with-current-buffer oprocbuf

            ;; (span-notef "alan-comint-output-filter: %s" (length string))

            (setq-local buffer-undo-list nil)

	        ;; Run preoutput filters
	        (let ((functions comint-preoutput-filter-functions))
	          (while (and functions string)
	            (if (eq (car functions) t)
		            (let ((functions
                           (default-value 'comint-preoutput-filter-functions)))
		              (while (and functions string)
		                (setq string (funcall (car functions) string))
		                (setq functions (cdr functions))))
	              (setq string (funcall (car functions) string)))
	            (setq functions (cdr functions))))

	        (let ((inhibit-read-only t)
                  (buffer-undo-list t)
                  (saved-point (copy-marker (point) t)))
	          (save-restriction
	            (widen)

	            (goto-char (process-mark process))
	            (set-marker comint-last-output-start (point))

                (add-text-properties
                 0 (length string)
                 `(
                   field output
                   read-only t
                   front-sticky (read-only)
                   rear-nonsticky (field read-only font-lock-face keymap)
                   keymap ,alan-comint-output-map)
                 string)

	            (insert string)

	            ;; Advance process-mark
	            (set-marker (process-mark process) (point))

	            (unless comint-inhibit-carriage-motion
	              ;; Interpret any carriage motion characters (newline, backspace)
	              (comint-carriage-motion comint-last-output-start (point)))

	            (run-hook-with-args 'comint-output-filter-functions string)

                (goto-char saved-point))))))))

  (defadvice! comint-send-input-advice (&rest _args)
    :after #'comint-send-input
    (add-text-properties
     comint-last-input-start comint-last-input-end
     '(
       read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))))

  (defun comint-clear-buffer-no-undo ()
    (interactive)
    (let ((buffer-undo-list t))
      (comint-clear-buffer)))

  (defadvice! comint-add-to-input-history-no-props (fn cmd)
    :around #'comint-add-to-input-history
    (funcall fn (substring-no-properties cmd)))

  ;; make shell not print control c on kill
  (advice-add #'comint-skip-input :override #'ignore)

  (evil-define-motion evil-comint-next-prompt (count)
    :jump t
    :type exclusive
    (comint-next-prompt (or count 1)))
  (evil-define-motion evil-comint-previous-prompt (count)
    :jump t
    :type exclusive
    (comint-previous-prompt (or count 1)))


  (general-define-key
   :states 'insert
   :keymaps '(comint-mode-map)
   "<down>" 'comint-next-input
   "<up>" 'comint-previous-input
   ;; "<right>" (lambda () (interactive) (if (point-at-eol-p) (completion-at-point) (right-char)))
   "RET" 'comint-send-input
   )
  (general-define-key
   :states 'motion
   :keymaps '(comint-mode-map)

   "<down>" #'evil-comint-next-prompt
   "<up>" 'evil-comint-previous-prompt

   "SPC c" #'comint-interrupt-subjob
   "SPC e" #'comint-send-eof
   "SPC d" #'comint-delete-output
   ;; "SPC r" 'comint-show-output
   ;; "SPC s" 'comint-write-output
   ;; "SPC u" 'comint-kill-input
   "SPC z" #'comint-stop-subjob
   "SPC q" #'comint-quit-subjob
   "SPC l" 'comint-clear-buffer-no-undo
   "SPC t" #'toggle-truncate-lines
   "SPC <up>" #'comint-kill-subjob
   )

  ;; disable wrapping of comint-next-input and comint-previous-input
  (advice-add #'comint-previous-input :before-until
              (lambda (arg)
                (if comint-input-ring-index
                    (and (> arg 0)
                         (eq comint-input-ring-index
                             (1- (ring-length comint-input-ring))))
                  (< arg 0))))

  (modify-syntax-entry (string-to-char "_") "w" comint-mode-syntax-table)
  (modify-syntax-entry (string-to-char "-") "w" comint-mode-syntax-table)

  (add-hook! 'comint-mode-hook
    (defun alan-comint-setup ()
      ;; (setq-local company-minimum-prefix-length 1)
      (ansi-color-for-comint-mode-on)

      (setq-local completion-at-point-functions '(comint-completion-at-point))
      (setq-local company-backends '(company-capf))
      ;; FIXME: race
      (when (fboundp 'company-mode)
        (company-mode +1))
      (setq-local company-idle-delay nil)

      ;; (when (file-remote-p default-directory)
      ;;   ;; (setq-local shell-dirtrackp nil)
      ;;   )

      ))
  )

(eval-after-load! shell
  ;; https://emacs.stackexchange.com/questions/62418/how-to-change-tramp-default-remote-shell-or-any-of-its-descendants
  ;; TODO: windows
  ;; (setq explicit-shell-file-name "/bin/bash")

  (setq-default shell-font-lock-keywords nil)
  ;; (setq-default company-global-modes '(not shell-mode))

  ;; (setq-default shell-fontify-input-enable t)
  (setq-default shell-fontify-input-enable nil)


  (defadvice! shell-reapply-ansi-color-inhibit-readonly (fn)
    :around #'shell-reapply-ansi-color
    (let ((inhibit-read-only t))
      (funcall fn)))

  ;; (add-hook! 'shell-mode-hook
  ;;   (setq-local font-lock-fontify-syntactically-function nil))

  )


(provide 'alan-comint)
