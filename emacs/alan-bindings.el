;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)

(require-if-is-bytecompile
 consult
 vertico
 magit

 alan-theme
 alan-minibuffer
 alan-font
 alan-commands
 alan-consult

 alan-elisp)

(general-def
  "C-," #'help-command
  "<ignore>" #'ignore
  "<x>" #'yank

  ;; TODO: end up using those rarely, should put something more useful here
  ;; "<w>" #'move-beginning-of-line
  ;; "<z>" #'evil-undo
  ;; "<y>" #'evil-redo
  ;; "<q>" #'evil-delete-line

  "<S-return>" #'newline
  "<.> <up>" #'newline
  "M-1" nil
  "<.> 1" #'toggle-input-method

  ;; "TAB" #'self-insert-command

  "<C-wheel-up>" #'alan-font-inc
  "<C-wheel-down>" #'alan-font-dec
  "C-=" #'alan-font-inc
  "C--" #'alan-font-dec
  "C-0" #'alan-font-reset
  "<C-S-wheel-up>" #'text-scale-increase
  "<C-S-wheel-down>" #'text-scale-decrease
  "C-+" #'text-scale-increase
  "C-_" #'text-scale-decrease)


(clear-and-backup-keymap help-map)
                                        ;
(general-def help-map
  "b" #'describe-bindings
  "m" #'describe-mode
  "k" #'describe-key
  "]" #'describe-keymap
  "v" #'describe-variable
  "f" #'describe-function
  "c" #'describe-char

  "x" #'alan-describe-ex-command
  "t" #'alan-describe-font-at-point
  "=" #'customize-face
  "p" #'alan-show-text-properties
  "0" #'debug
  "l" #'find-library)

(clear-and-backup-keymap evil-normal-state-map)
(clear-and-backup-keymap evil-visual-state-map)
;; (clear-and-backup-keymap evil-operator-state-map)
(clear-and-backup-keymap evil-motion-state-map)
(clear-and-backup-keymap evil-window-map)


(general-def
  [remap forward-char] #'evil-forward-char
  [remap backward-char] #'evil-backward-char

  [remap previous-line] #'evil-previous-visual-line
  [remap next-line] #'evil-next-visual-line

  [remap beginning-of-buffer] #'evil-goto-first-line
  [remap end-of-buffer] #'evil-goto-line

  [remap move-beginning-of-line] #'evil-beginning-of-line
  [remap move-end-of-line] #'evil-end-of-line ;;evil-last-non-blank-or-eol

  [remap forward-paragraph] #'evil-forward-paragraph
  [remap backward-paragraph] #'evil-backward-paragraph
  )

(general-def
  :states 'motion

  [remap self-insert-command] #'undefined

  "w" 'evil-window-map
  "N" #'help-command

  "<w> <T>" #'consult-theme
  "w <T>" #'alan-switch-theme
  "g h" #'consult-history
  "g l" #'consult-line
  "g <right>" #'consult-line-multi
  "g m" #'consult-mark
  "g SPC" #'consult-ripgrep
  "g r" (lambda () (interactive) (consult-ripgrep default-directory))

  "s" #'execute-extended-command

  "S-0" #'digit-argument
  "S-1" #'digit-argument
  "S-2" #'digit-argument
  "S-3" #'digit-argument
  "S-4" #'digit-argument
  "S-5" #'digit-argument
  "S-6" #'digit-argument
  "S-7" #'digit-argument
  "S-8" #'digit-argument
  "S-9" #'digit-argument

  ;; arrows
  "h" #'backward-char
  "l" #'forward-char
  "k" #'previous-line
  "j" #'next-line

  "<left>" #'evil-backward-word-begin
  "<right>" #'evil-forward-word-end
  "<down>" #'forward-paragraph
  "<up>" #'backward-paragraph

  "H" #'move-beginning-of-line
  "L" #'move-end-of-line
  "J" #'evil-jump-forward
  "K" #'evil-jump-backward

  ;; mode changing

  "v" #'evil-visual-char
  "\\" #'evil-visual-line

  "%" #'pp-eval-expression
  "<.> {" #'copy-pp-eval-expression

  ;; essential
  "y" #'evil-yank

  ;; search
  "1" #'evil-ex-search-word-forward

  "f" #'evil-ex-search-forward
  "=" #'evil-ex-search-backward

  "n" #'evil-ex-search-next
  "[" #'evil-ex-search-previous

  "t" #'evil-find-char-to
  ;; "<t>" 'evil-find-char-backward

  "e" #'evil-find-char
  "{" #'evil-find-char-backward
  "m" #'evil-repeat-find-char
  "]" #'evil-repeat-find-char-reverse

  ;; leader

  ;; TODO: wtf is this?
  ;; "<.> m" #'evil-lookup

  "<.> n" #'evil-ex-nohighlight
  "$" #'evil-visual-restore
  "\"" #'evil-jump-item ;; go to other parenthesis

  "<.> j" #'evil-goto-definition

  "0" #'display-local-help

  "<.> }" #'revert-buffer

  ;; not needed or now have different key, can be bound to something else
  ;; "M" 'ignore ;;evil-window-middle
  ;; "_" 'ignore ;;evil-next-line-1-first-non-blank
  ;; "B" 'ignore ;;evil-backward-WORD-begin ;; now <left>
  ;; "W" 'ignore ;;evil-forward-WORD-begin ;; now <right>
  ;; "/" 'ignore ;;evil-ex-search-forward ;; now S

  "g g" #'beginning-of-buffer
  "&" #'end-of-buffer

  "g u" #'evil-scroll-line-to-top
  "g i" #'evil-scroll-line-to-bottom
  "g s" #'evil-scroll-line-to-center
  ;; TODO: dont work in visual state?
  "g k" (lambda () (interactive) (evil-previous-visual-line (- (window-height) 5)))
  "g j" (lambda () (interactive) (evil-next-visual-line (- (window-height) 5)))

  "g c" #'copy-cur-filename
  "g x" #'copy-cur-filename-last

  "z s" #'shell

  "z j" #'magit-status
  "z l" #'magit-log
  "z <right>" #'magit-log-all
  "z b" #'magit-branch
  "z f" #'magit-find-file-other-window
  "z c" #'magit-checkout
  "z r" #'magit-remote
  )


(general-def
  :states 'normal

  ;; essential
  ;; moved from motion
  ;; https://emacs.stackexchange.com/questions/46371/how-can-i-get-ret-to-follow-org-mode-links-when-using-evil-mode
  ;; ~removed now, revisit if found use~
  ;; I do need this, prevents ret from entering newline in normal state
  "RET" #'evil-ret

  "a" #'evil-append
  "<end>" #'evil-append-line ;; A

  "i" #'evil-insert
  ")" #'evil-insert-line ;; I

  "p" #'evil-paste-after
  "-" #'evil-paste-before;; P
  "P" #'consult-yank-from-kill-ring

  "u" #'evil-undo
  "(" #'evil-redo ;; U

  "DEL" #'evil-delete-char
  "S-DEL" #'evil-delete-whole-line

  "r" #'evil-replace
  "}" #'evil-replace-state ;; R

  "o" #'evil-join ;;J
  "+" #'split-line

  "c" #'evil-change
  "d" #'evil-delete

  "q" #'evil-use-register


  "'" #'evil-indent
  "<" #'evil-shift-left
  ">" #'evil-shift-right

  "#" #'comment-region
  ":" #'uncomment-region

  ;; needs to be bound to (maybe) something different
  "=" 'nil ;;evil-indent

  "\"" 'nil ;;evil-use-register
  "." #'evil-repeat

  ;; "q" 'nil ;;evil-record-macro
  "@" 'nil ;;evil-execute-macro

  "`" #'evil-record-macro
  "@" #'evil-execute-macro

  "g a" #'save-buffer

  ;; todo: which of those do i need
  ;; "C" 'nil ;;evil-change-line
  ;; "D" 'nil ;;evil-delete-line

  ;; "J" 'nil ;;evil-join
  ;; "S" 'nil ;;evil-change-whole-line
  ;; "X" 'nil ;;evil-delete-backward-char
  ;; "Y" 'nil ;;evil-yank-line
  ;; "Z" 'nil ;;Prefix Command
  ;; "[" 'nil ;;Prefix Command
  ;; "]" 'nil ;;Prefix Command
  ;; "g" 'nil ;;Prefix Command
  ;; "m" 'nil ;;evil-set-marker
  ;; "y" 'nil ;;evil-yank
  ;; "z" 'nil ;;Prefix Command
  ;; "~" 'nil ;;evil-invert-char
  )

(general-def
  :states 'visual

  "<escape>" #'evil-exit-visual-state
  "i" evil-inner-text-objects-map
  "a" evil-outer-text-objects-map

  ;; ")" 'evil-insert
  ;; "A" 'evil-append

  "o" #'exchange-point-and-mark
  ;; "+" 'evil-visual-exchange-corners

  "u" #'evil-downcase
  "(" #'evil-upcase

  "p" #'alan-evil-visual-paste-without-yank
  "-" #'evil-visual-paste
  ;; TODO: this isnt what i want
  ;; "P" #'consult-yank-replace

  "<" #'evil-shift-left
  ">" #'evil-shift-right

  "1" #'alan-evil-visual-star-nomove

  ;; "g u" #'undo
  )

(general-def evil-window-map
  "k" #'evil-window-up
  "j" #'evil-window-down
  "h" #'evil-window-left
  "l" #'evil-window-right

  "u" #'previous-buffer
  "i" #'next-buffer
  "y" #'alan-iflipb-pop

  "o" #'delete-other-windows
  "a" #'quit-window
  "q" #'evil-window-delete
  "<up>" #'alan-kill-current-buffer
  "K" (lambda ()
        (interactive)
        (let (kill-buffer-hook kill-buffer-query-functions)
          (kill-buffer)))

  "s" #'evil-window-split
  "v" #'evil-window-vsplit
  "f" #'window-swap-states

  "t"
  (lambda ()
    (interactive)
    (let ((wconfig (current-window-configuration)))
      (add-hook-once! 'post-command-hook :depth 100
        (call-interactively
         (lambda ()
           (interactive)
           (set-window-configuration wconfig nil 'dont-set-miniwindow)))))
    (top-level))

  "w" #'find-file
  "b" #'switch-to-buffer
  "n" #'switch-to-buffer-other-window
  "p" #'consult-project-buffer

  "SPC" #'consult-find

  "m" #'alan-minibuffer-resize-count
  ;; "n" #'evil-buffer

  ;; "<T>" #'load-theme

  "(" #'alan-find-url

  ;; "z j" #'magit-status
  ;; "z l" #'magit-log
  ;; "z b" #'magit-branch
  ;; "z f" #'magit-find-file-other-window
  )

(general-def
  :states 'insert
  ;; "TAB" 'self-insert-command
  ;; "TAB" (lambda () (interactive) (insert-tab))
  "<.> <down>" #'alan-completion-at-point
  "C-<.> J" #'alan-consult-complete)

;; makes number keys replace in replace mode
(general-def evil-read-key-map
  "S-1" "1"
  "S-2" "2"
  "S-3" "3"
  "S-4" "4"
  "S-5" "5"
  "S-6" "6"
  "S-7" "7"
  "S-8" "8"
  "S-9" "9"
  "S-0" "0")

(general-def evil-outer-text-objects-map
  "l" #'evil-a-line
  "e" #'evil-entire-entire-buffer)

(general-def evil-inner-text-objects-map
  "l" #'evil-inner-line
  "e" #'evil-entire-entire-buffer)

(clear-and-backup-keymap minibuffer-local-map)
(clear-and-backup-keymap minibuffer-local-must-match-map)
(clear-and-backup-keymap minibuffer-local-completion-map)

;; evil-ex-start-search actually sets the global value of minibuffer-local-map to evil-ex-search-keymap
;; which i think is a bug?
;; this do not usually manifest though, since you are supposed to
;; read-from-minibuffer with a map with minibuffer-local-map as parent
;; which still works even if minibuffer-local-map has been changed by evil
;; i dont need evil-ex-search-keymap though, so just do this...
(setq evil-ex-search-keymap minibuffer-local-map)

(general-def minibuffer-local-map
  :states '(motion insert)
  "<escape>" #'abort-minibuffers
  "<up>" #'previous-complete-history-element
  "<down>" #'next-complete-history-element)

(general-def minibuffer-local-map
  :states 'insert
  "RET" #'exit-minibuffer
  "<.> <right>" #'evil-force-normal-state)

(general-def minibuffer-local-completion-map
  :states 'insert
  "TAB" #'minibuffer-complete
  "<.> j" #'minibuffer-complete
  "RET" #'minibuffer-complete-and-exit

  "<right>"
  (lambda ()
    (interactive)
    (if (alan-point-at-eol-p)
        (call-interactively-with-remap #'minibuffer-complete)
      (call-interactively-with-remap #'right-char)))

  "<up>" #'minibuffer-previous-completion
  "<down>" #'minibuffer-next-completion

  "C-k" #'vertico-previous-group
  "C-j" #'vertico-next-group

  ;; minibuffer-complete-word ?
  )

;; dont need this, we do this in minibuffer-local-map already
;; (general-def minibuffer-local-completion-map
;;   :states 'motion
;;   "<up>" #'previous-complete-history-element
;;   "<down>" #'next-complete-history-element)

(eval-after-load! crm
  (clear-and-backup-keymap crm-local-completion-map)
  (clear-and-backup-keymap crm-local-must-match-map)
  ;; TODO: currently keybindings for crm dont work
  ;; need to add crm cmds for vertico probably
  )



(provide 'alan-bindings)
