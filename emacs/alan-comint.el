;; -*- lexical-binding: t -*-

(require 'alan-core)

(require 'evil)


;; https://superuser.com/questions/868680/make-command-line-unmodifiable-in-aquamacs
;; https://snarfed.org/why_i_run_shells_inside_emacs
(eval-after-load! comint
  ;; ;; https://stackoverflow.com/questions/26985772/fast-emacs-shell-mode
  ;; (setq
  ;;    comint-move-point-for-output nil
  ;;    comint-scroll-show-maximum-output nil
  ;;    )

  (setq-default comint-use-prompt-regexp nil)

  ;; https://stackoverflow.com/questions/25819034/colors-in-emacs-shell-prompt
  ;; FIXME
  (set-face-attribute 'comint-highlight-prompt nil :inherit 'unspecified)

  ;; https://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode
  ;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L26-L48
  ;; https://emacs.stackexchange.com/questions/13592/how-does-comint-mode-override-beginning-of-line-behavior-i-want-same-functional
  ;; Make processes’ outputs read-only. The prompt is easy.

  ;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Shell-echo.html
  (setq-default comint-prompt-read-only t)

  (defun comint-output-filter-advice (process string)
    (let ((oprocbuf (process-buffer process)))
      ;; repeat check in comint-output-filter
      (when (and string oprocbuf (buffer-name oprocbuf))
        (with-current-buffer oprocbuf
          (let*
              (
               (inhibit-read-only t)
               (saved-point (copy-marker (point) t))
               )
            (save-restriction
              (widen)
              (with-silent-modifications
                (add-text-properties comint-last-output-start (process-mark process)
                                     '(
                                       front-sticky
                                       (read-only)
                                       ;; (read-only field
                                       ;;    ;; inhibit-line-move-field-capture
                                       ;;    )
                                       inhibit-line-move-field-capture t
                                       read-only t
                                       )))))))))
  (advice-add 'comint-output-filter :after #'comint-output-filter-advice)
  (defun comint-send-input-advice (&rest args)
    (add-text-properties comint-last-input-start comint-last-input-end
                         '(
                           read-only t
                           front-sticky (read-only)
                           rear-nonsticky (read-only)
                           ;; front-sticky nil
                           )))
  (advice-add 'comint-send-input :after #'comint-send-input-advice)
  ;; make shell not print control c on kill, --and go to newline on kill when input
  ;; FIXME
  (defun my-comint-skip-input ()
    (cl-letf ;; change to cl-letf since original file use dynamic binding
        (
         ((symbol-value 'comint-input-sender) #'ignore)
         ((symbol-value 'comint-input-filter-functions) nil)
         )
      (comint-send-input t t)
      )
    (end-of-line)
    )
  (advice-add 'comint-skip-input :override 'my-comint-skip-input)

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

   "SPC c" 'comint-interrupt-subjob
   "SPC e" 'comint-send-eof
   "SPC d" 'comint-delete-output
   ;; "SPC r" 'comint-show-output
   ;; "SPC s" 'comint-write-output
   ;; "SPC u" 'comint-kill-input
   "SPC z" 'comint-stop-subjob
   "SPC q" 'comint-quit-subjob
   "SPC l" 'comint-clear-buffer
   "SPC t" 'comint-continue-subjob
   )

  ;; disable wrapping of comint-next-input and comint-previous-input
  (advice-add 'comint-previous-input :before-until
              (lambda (arg)
                (if comint-input-ring-index
                    (and (> arg 0)
                         (eq comint-input-ring-index
                             (1- (ring-length comint-input-ring))))
                  (< arg 0))))

  (modify-syntax-entry (string-to-char "_") "w" comint-mode-syntax-table)
  (modify-syntax-entry (string-to-char "-") "w" comint-mode-syntax-table)
  (add-hook! 'comint-mode-hook
    (defun my-comint-setup ()
      ;; (setq-local company-minimum-prefix-length 1)
      (ansi-color-for-comint-mode-on)

      (setq-local completion-at-point-functions '(comint-completion-at-point))
      (setq-local company-backends '(company-capf))
      ;; FIXME: race + does not work when u exit shell and then sh back
      (company-mode)
      (setq-local company-idle-delay nil)
      (when (file-remote-p default-directory)
        (setq-local modeline-filename
                    '(
                      (:propertize (:eval (file-remote-p default-directory)) face modeline-file-path)
                      (:propertize (:eval (buffer-name)) face modeline-file-or-buffer-name)))
        (setq-local shell-dirtrackp nil)
        ;; (setq-local company-idle-delay nil)
        )
      ))

  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  )

(eval-after-load! shell
  (setq-default shell-font-lock-keywords nil)
  (setq-default company-global-modes '(not shell-mode))

  (setq-default shell-fontify-input-enable t)

  ;; (add-hook! 'shell-mode-hook
  ;;   (setq-local font-lock-fontify-syntactically-function nil))

  )




(provide 'alan-comint)
