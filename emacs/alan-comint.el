;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(defvar-keymap alan-comint-output-map)

;; https://superuser.com/questions/868680/make-command-line-unmodifiable-in-aquamacs
;; https://snarfed.org/why_i_run_shells_inside_emacs
(eval-after-load! comint
  ;; https://stackoverflow.com/questions/26985772/fast-emacs-shell-mode
  (setq comint-move-point-for-output nil)
  (setq comint-scroll-show-maximum-output nil)

  (setq comint-buffer-maximum-size 10000)
  (add-to-list 'comint-output-filter-functions #'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions #'comint-osc-process-output)

  (setf (alist-get "0" (default-value 'ansi-osc-handlers) nil nil #'string=)
        #'ansi-osc-window-title-handler)

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
  (setq comint-prompt-read-only t)

  (general-def alan-comint-output-map
    [remap comint-send-input] #'undefined)

  (general-def comint-mode-map
    [remap forward-paragraph] #'evil-comint-next-prompt
    [remap backward-paragraph] #'evil-comint-previous-prompt

    [remap alan-move-up-screen] #'evil-backward-paragraph
    [remap alan-move-down-screen] #'evil-forward-paragraph

    )

  (general-define-key
   :states 'insert
   :keymaps '(comint-mode-map)
   "<down>" #'comint-next-input
   "<up>" #'comint-previous-input
   ;; "<right>" (lambda () (interactive) (if (point-at-eol-p) (completion-at-point) (right-char)))
   "RET" #'comint-send-input
   )

  (setf (get #'comint-send-input 'company-abort) t)

  (general-define-key
   :states 'motion
   :keymaps '(comint-mode-map)

   "SPC c" #'comint-interrupt-subjob
   "SPC e" #'comint-send-eof
   "SPC d" #'comint-delete-output
   ;; "SPC r" 'comint-show-output
   ;; "SPC s" 'comint-write-output
   ;; "SPC u" 'comint-kill-input
   "SPC z" #'comint-stop-subjob
   "SPC q" #'comint-quit-subjob
   "SPC l" 'comint-clear-buffer-no-undo
   ;; "SPC t" #'toggle-truncate-lines
   "SPC <up>" #'comint-kill-subjob
   )

  (modify-syntax-entry (string-to-char "_") "w" comint-mode-syntax-table)
  (modify-syntax-entry (string-to-char "-") "w" comint-mode-syntax-table)
  )


;; https://emacs.stackexchange.com/questions/2883/any-way-to-make-prompts-and-previous-output-uneditable-in-shell-term-mode
(advice-add #'comint-output-filter :override 'alan-comint-output-filter)
(defun alan-comint-output-filter (process string)
  (span :alan-comint-output-filter
    ;; (span-notef "raw output:\n%s" string)
    (let ((oprocbuf (process-buffer process)))
      ;; First check for killed buffer or no input.
      (when (and string oprocbuf (buffer-name oprocbuf))
        (with-current-buffer oprocbuf

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
                (saved-point
                 (if (<= (process-mark process) (point))
                     (copy-marker (point) t)
                   (point))))

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

              (let ((pm (point-marker)))
                (setq comint-last-prompt (cons pm pm)))

              (goto-char saved-point))))))))

(advice-add #'comint-exec-1 :around #'alan-comint-exec-1)
(defun alan-comint-exec-1 (fn name buffer command switches)
  ;; see `term-exec-1'
  ;; (span-dbgf :alan-comint-exec-1 name buffer command switches)
  (if (eq system-type 'windows-nt)
      (funcall fn name buffer command switches)
    (let* ((proc (funcall
                  fn name buffer
                  "bash"
                  `("-c"
                    "stty echo; exec \"$@\""
                    ;; "stty echo onlcr; exec \"$@\""
                    ".."
                    ,command ,@switches)))
           ;; (code (call-process "stty" nil nil nil "-F" (process-tty-name proc) "echo"))
           )
      ;; (unless (= code 0)
      ;;   (error "stty failed"))
      proc)
    ))

(defun alan-comint-mark-prompt ()
  (with-silent-modifications
    (add-text-properties
     (pos-bol) (point)
     '(field prompt))))

(defvar alan-comint-keep-inputs nil)

(advice-add #'comint-send-input :override #'alan-comint-send-input)
(defun alan-comint-send-input (&optional no-newline artificial)
  (interactive nil comint-mode)
  (ignore artificial)

  (when completion-in-region-mode
    (completion-in-region-mode -1))

  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (intxt (if (>= (point) (marker-position pmark))
                        (progn (if comint-eol-on-send
				                   (goto-char (field-end)))
                               (buffer-substring pmark (point)))
                      (user-error "not in input"))))

        (if alan-comint-keep-inputs
            (let ((inhibit-read-only t))
              ;; see original function for this delete-region
              ;; (delete-region pmark (point))
              ;; (insert intxt)
              (unless no-newline
                (insert ?\n))
              (add-text-properties
               pmark (point)
               '(
                 read-only t
                 front-sticky (read-only)
                 rear-nonsticky (read-only))))

          (let ((inhibit-read-only t))
            (delete-region pmark (point))))

        (comint-add-to-input-history intxt)

        (save-excursion
          (goto-char pmark)
          (alan-comint-mark-prompt))

        (run-hook-with-args 'comint-input-filter-functions
                            (if no-newline intxt
                              (concat intxt "\n")))

        (comint-snapshot-last-prompt)

        (setq comint-save-input-ring-index comint-input-ring-index)
        (setq comint-input-ring-index nil)

        ;; Update the markers before we send the input
        ;; in case we get output amidst sending the input.
        (set-marker comint-last-input-start pmark)
        (set-marker comint-last-input-end (point))
        (set-marker pmark (point))
        ;; clear the "accumulation" marker
        (set-marker comint-accum-marker nil)
        (let ((comint-input-sender-no-newline no-newline))
          (funcall comint-input-sender proc intxt))

        ;; This used to call comint-output-filter-functions,
        ;; but that scrolled the buffer in undesirable ways.
        (set-marker comint-last-output-start pmark)
        (run-hook-with-args 'comint-output-filter-functions "")))))

;; (defadvice! comint-send-input-advice (&rest _args)
;;   :after #'comint-send-input
;;   (add-text-properties
;;    comint-last-input-start comint-last-input-end
;;    '(
;;      read-only t
;;      front-sticky (read-only)
;;      rear-nonsticky (read-only))))

(defun comint-clear-buffer-no-undo ()
  (interactive)
  (let ((buffer-undo-list t))
    (comint-clear-buffer)))

(defadvice! comint-add-to-input-history-no-props (fn cmd)
  :around #'comint-add-to-input-history
  (funcall fn (substring-no-properties cmd)))

;; make shell not print control c on kill
(advice-add #'comint-skip-input :override #'ignore)

(advice-add #'comint-next-prompt :override #'alan-comint-next-prompt)
(defun alan-comint-next-prompt (n)
  (interactive "^p" comint-mode)
  (let ((pos (point))
	    (input-pos nil)
	    prev-pos)
    (while (/= n 0)
	  (setq prev-pos pos)
	  (setq pos
	        (if (> n 0)
		        (next-single-char-property-change pos 'field)
		      (previous-single-char-property-change pos 'field)))
	  (cond ((= pos prev-pos)
	         (when (> n 0)
		       (setq input-pos (point-max)))
	         (setq n 0))
	        ((eq (get-char-property (if (> pos 1) (1- pos) pos) 'field) 'prompt)
	         (setq n (if (< n 0) (1+ n) (1- n)))
	         (setq input-pos pos))))
    (when input-pos
	  (goto-char input-pos))))


(evil-define-motion evil-comint-next-prompt (count)
  :jump t
  :type exclusive
  (comint-next-prompt (or count 1)))
(evil-define-motion evil-comint-previous-prompt (count)
  :jump t
  :type exclusive
  (comint-previous-prompt (or count 1)))


;; disable wrapping of comint-next-input and comint-previous-input
(advice-add #'comint-previous-input :before-until
            (lambda (arg)
              (if comint-input-ring-index
                  (and (> arg 0)
                       (eq comint-input-ring-index
                           (1- (ring-length comint-input-ring))))
                (< arg 0))))


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

    (kill-local-variable 'indent-line-function)

    ;; (when (file-remote-p default-directory)
    ;;   ;; (setq-local shell-dirtrackp nil)
    ;;   )

    ))


(provide 'alan-comint)
