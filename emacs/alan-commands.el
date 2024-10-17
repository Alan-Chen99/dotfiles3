;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'evil)
(require 'evil-visualstar)

(defun alan-kill-current-buffer ()
  (interactive)
  (kill-buffer))


(defun alan-describe-ex-command (command)
  (interactive
   (list
    (completing-read
     "Describe Ex Command: "
     (evil-ex-completion-table)
     nil t)))
  (describe-function (evil-ex-binding command)))

(defun alan-customize-face ()
  (interactive)
  (global-hl-line-mode 0)
  (show-paren-mode 0)
  (unwind-protect
      (call-interactively 'customize-face)
    (global-hl-line-mode 1)
    (show-paren-mode 1)))
(general-def [remap customize-face] #'alan-customize-face)

(evil-define-command alan-show-text-properties ()
  (if (evil-visual-state-p)
      (prin1 (buffer-substring (mark) (point)))
    (prin1 (text-properties-at (point)))))

(evil-define-operator alan-evil-visual-star-nomove (beg end)
  :jump nil
  :repeat nil
  :move-point nil
  (interactive "<r>")
  (save-excursion
    (let ((evil-search-wrap t) (inhibit-message t))
      (evil-visualstar/begin-search-forward beg end)
      (let ((evil-ex-search-direction 'backward))
        (evil-ex-search 1)))))

(evil-define-command alan-evil-visual-paste-without-yank (count &optional register yank-handler)
  :suppress-operator t
  (interactive "*P<x>")
  (let ((evil-kill-on-visual-paste nil))
    (setq count (prefix-numeric-value count))
    (evil-visual-paste count register)))


;; https://github.com/emacsorphanage/evil-textobj-line/blob/master/evil-textobj-line.el
(defun evil-line-range (_count _beg _end _type &optional inclusive)
  (if inclusive
      (evil-range (line-beginning-position) (line-end-position))
    (let ((start (save-excursion
                   (back-to-indentation)
                   (point)))
          (end (save-excursion
                 (goto-char (line-end-position))
                 (skip-syntax-backward " " (line-beginning-position))
                 (point))))
      (evil-range start end))))
(evil-define-text-object evil-a-line (count &optional beg end type)
  "Select range between a character by which the command is followed."
  (evil-line-range count beg end type t))
(evil-define-text-object evil-inner-line (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  (evil-line-range count beg end type))
;; https://github.com/syohex/evil-textobj-entire/blob/master/evil-textobj-entire.el
(evil-define-text-object evil-entire-entire-buffer (count &optional beg end type)
  "Select entire buffer"
  (evil-range (point-min) (point-max)))



;; ;; TODO: why the inclusive/exclusive
;; ;; https://github.com/emacs-evil/evil/blob/6fde982d731e2cc4e5f6bded6f8955ab2daee3b7/evil-commands.el#L229
;; (evil-define-motion evil-last-non-blank-or-eol (count)
;;  :type inclusive
;;  (if (> (save-excursion (evil-last-non-blank count) (point)) (point))
;;      (evil-last-non-blank count)
;;      (evil-end-of-line count)
;;      ))
;; TODO: why does this not involve count
;; https://github.com/emacs-evil/evil/blob/6fde982d731e2cc4e5f6bded6f8955ab2daee3b7/evil-commands.el#L224
(evil-define-motion evil-first-non-blank-or-beginning ()
  :type exclusive
  (if (< (save-excursion (evil-first-non-blank) (point)) (point))
      (evil-first-non-blank)
    (evil-beginning-of-line)))
(general-def [remap move-beginning-of-line] #'evil-first-non-blank-or-beginning)


;; TODO: maybe use something more like this
;; (defun evil-goto-definition-search (string _position)
;;  "Find definition for STRING with evil-search."
;;  (evil-search (format "\\_<%s\\_>" (regexp-quote string)) t t (point-min))
;;  t)
(evil-define-motion evil-ex-search-word-forward-nomove (count &optional symbol)
  :jump nil
  :repeat nil
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     evil-symbol-word-search))
  (save-excursion
    (let ((evil-search-wrap t) (inhibit-message t))
      ;; give up on matching word boundaries if buffer is too large
      (evil-ex-start-word-search (> (buffer-size) 1000000) 'forward count symbol)
      (let ((evil-ex-search-direction 'backward))
        (evil-ex-search 1)))))
(general-def [remap evil-ex-search-word-forward] #'evil-ex-search-word-forward-nomove)

(evil-define-motion evil-ex-search-always-next (count)
  "Goes to the next occurrence."
  ;; :jump t
  :jump nil
  :type exclusive
  (save-excursion
    (let
        ((evil-search-wrap t) (inhibit-message t))
      (evil-ex-search count)))
  (let
      (
       (evil-ex-search-direction 'forward)
       (evil-search-wrap nil))
    (evil-ex-search count)))
(general-def [remap evil-ex-search-next] #'evil-ex-search-always-next)
(evil-define-motion evil-ex-search-always-previous (count)
  "Goes the the previous occurrence."
  ;; :jump t
  :jump nil
  :type exclusive
  (save-excursion
    (let
        ((evil-search-wrap t) (inhibit-message t))
      (evil-ex-search count)))
  (let
      (
       (evil-ex-search-direction 'backward)
       (evil-search-wrap nil))
    (evil-ex-search count)))
(general-def [remap evil-ex-search-previous] #'evil-ex-search-always-previous)
(eval-after-load! eldoc
  (eldoc-remove-command #'evil-ex-search-always-next)
  (eldoc-remove-command #'evil-ex-search-always-previous))


(evil-define-motion evil-repeat-find-char-always-forward (count)
  :type inclusive
  (if (and evil-last-find (nth 2 evil-last-find))
      (evil-repeat-find-char count)
    (evil-repeat-find-char-reverse count)))
(general-def [remap evil-repeat-find-char] #'evil-repeat-find-char-always-forward)

(evil-define-motion evil-repeat-find-char-reverse-always-backward (count)
  :type inclusive
  (if (and evil-last-find (nth 2 evil-last-find))
      (evil-repeat-find-char-reverse count)
    (evil-repeat-find-char count)))
(general-def [remap evil-repeat-find-char-reverse] #'evil-repeat-find-char-reverse-always-backward)



(evil-define-operator evil-delete-char-without-yank (beg end type register)
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-delete beg end type ?_))
(general-def [remap evil-delete-char] #'evil-delete-char-without-yank)


(evil-define-operator evil-delete-whole-line-without-yank (beg end type register)
  :motion evil-line-or-visual-line
  :type line
  (interactive "<R><x>")
  (evil-delete beg end type ?_))
(general-def [remap evil-delete-whole-line] #'evil-delete-whole-line-without-yank)


(evil-define-operator evil-change-without-yank
  (beg end type register yank-handler delete-func)
  (interactive "<R><x><y>")
  (evil-change beg end type ?_ yank-handler delete-func))
(general-def [remap evil-change] #'evil-change-without-yank)


;; replaced with format-all
(evil-define-operator evil-indent-all (beg end)
  :move-point nil
  :type line
  :motion evil-entire-entire-buffer
  (save-excursion
    (evil-indent beg end)))
;; (general-def [remap evil-indent] #'evil-indent-all)

(defun evil-with-restored-visual-impl (fn)
  (let ((isvisual (evil-visual-state-p)))
    (unwind-protect
        (funcall fn)
      (when isvisual
        (evil-normal-state)
        (evil-visual-restore)))))


(defmacro evil-with-restored-visual (&rest forms)
  `(evil-with-restored-visual-impl (lambda () ,@forms)))

;; https://emacs.stackexchange.com/questions/48719/keep-text-selection-after-indenting-with-evil
(evil-define-operator evil-shift-left-keep-visual (beg end &optional count preserve-empty)
  :type line
  (interactive "<r><vc>")
  (evil-with-restored-visual
   (evil-shift-left beg end count preserve-empty)))
(general-def [remap evil-shift-left] #'evil-shift-left-keep-visual)

(evil-define-operator evil-shift-right-keep-visual (beg end &optional count preserve-empty)
  :type line
  (interactive "<r><vc>")
  (evil-with-restored-visual
   (evil-shift-right beg end count preserve-empty)))
(general-def [remap evil-shift-right] #'evil-shift-right-keep-visual)

;; https://github.com/linktohack/evil-commentary
(evil-define-operator my-comment-region (beg end type)
  :type line
  :move-point nil
  :motion evil-line-or-visual-line
  (interactive "<R>")
  (let ((inhibit-message t))
    (evil-with-restored-visual
     (comment-region beg end))))
(general-def [remap comment-region] #'my-comment-region)

(evil-define-operator my-uncomment-region (beg end type)
  :type line
  :move-point nil
  :motion evil-line-or-visual-line
  (interactive "<R>")
  (let ((inhibit-message t))
    (evil-with-restored-visual
     (uncomment-region beg end))))
(general-def [remap uncomment-region] #'my-uncomment-region)


(defun my-display-local-help ()
  (interactive)
  (if-let ((help (help-at-pt-kbd-string)))
      (message "%s" (substitute-command-keys help))
    (save-excursion
      (goto-char (1+ (point)))
      (display-local-help))))
(general-def [remap display-local-help] #'my-display-local-help)


(defun q ()
  (interactive)
  (save-buffers-kill-terminal))


(defun copy-cur-filename ()
  (interactive)
  (let ((v
         (if buffer-file-name
             buffer-file-name
           default-directory)))
    (message "%s" v)
    (kill-new v)))


(defun copy-wsl-windows-filename ()
  (interactive)
  (let ((v (process-lines
            "wslpath" "-w"
            (file-truename
             (or buffer-file-name default-directory)))))
    (cl-assert (length= v 1))
    (setq v (nth 0 v))
    (message "%s" v)
    (kill-new v)))

(defun copy-cur-filename-last ()
  (interactive)
  (let ((v (if buffer-file-name
               (file-name-nondirectory buffer-file-name)
             (file-name-nondirectory (directory-file-name default-directory)))))
    ;; (message "%s\n%s" (buffer-name) v)
    (message "%s" v)
    (kill-new v)))


;; (defalias 'sh 'shell)
(cl-defun sh ()
  (interactive)
  (let ((remote (file-remote-p default-directory)))
    (dolist (b (append (bound-and-true-p my-iflipb-buffer-list) (buffer-list)))
      (when (and (eq (buffer-local-value 'major-mode b) 'shell-mode)
                 (equal (file-remote-p (buffer-local-value 'default-directory b)) remote))
        (shell b)
        (cl-return-from sh))))
  (shell (generate-new-buffer-name "*shell*")))

(defun shn ()
  (interactive)
  (setq current-prefix-arg '(1))
  (call-interactively 'shell))

(defun alan-describe-font-at-point ()
  (interactive)
  (let ((font (font-xlfd-name (font-at (point)) t))
        (sz (window-text-pixel-size
             nil (point) (1+ (point)))))
    (message "size: %S\n%s" sz font)))


;; TODO: maybe make resizing commands

(defun alan-frame-text-width (&optional frame)
  (frame-text-width frame))
(gv-define-setter alan-frame-text-width (val &optional frame)
  `(set-frame-width ,frame ,val nil 'pixelwise))

;; (setf (alan-frame-text-width) (alan-frame-text-width))

(defun alan-frame-native-width (&optional frame)
  (frame-native-width frame))
(defun alan-set-frame-native-width (val &optional frame)
  (setf (alan-frame-text-width)
        (- val
           (- (alan-frame-native-width frame)
              (alan-frame-text-width frame)))))
(gv-define-simple-setter alan-frame-native-width alan-set-frame-native-width)

;; (set-frame-parameter nil 'fullscreen 'maximized)
;; (frame-parameter nil 'fullscreen)
;; (frame-parameter nil 'height)
;; (frame-parameter nil 'width)

;; (set-frame-parameter nil 'width 1.0)

;; (window-old-pixel-width)
;; (toggle-frame-maximized)

;; (setf (frame-parameter nil 'left) 0)
;; (gv-define-setter alan-frame-native-width (val &optional frame)
;;   `(set-frame-width
;;     (- (alan-frame-native-width ,frame)
;;        (alan-frame-text-width ,frame))
;;     ,val nil 'pixelwise))




;; set-frame-width sets (frame-text-width)
;; (set-frame-width (selected-frame)
;;                  1189
;;                  ;; (- (frame-pixel-width) (frame-text-width))
;;                  nil 'pixelwise)

;; (frame-border-width)

;; 1189
;; (frame-native-width) 1226
;; (frame-inner-width) 1226
;; (frame-outer-width) 1244
;; (frame-text-width)

;; (x-display-pixel-height)


(provide 'alan-commands)
