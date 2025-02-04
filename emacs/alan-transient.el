;; -*- lexical-binding: t -*-

(require 'alan-core)
(pkg! 'transient)

(defun alan-normalize-key (key)
  (concat " " (key-description (key-parse key)) " "))

(defun alan-transient-tranlate-key (key)
  (setq key (vconcat key [dummy dummy]))
  (let (n subs cur ans)
    (while (length> key 2)
      (setq n (lookup-key key-translation-map key))
      (setq subs (substring key 0 n))
      (setq cur (lookup-key key-translation-map subs))
      (if cur
          (progn
            (push cur ans)
            (setq key (substring key n)))
        (push (list (aref key 0)) ans)
        (setq key (substring key 1))))
    (setq ans (nreverse ans))
    (apply #'vconcat ans)))

;; (my-transient-substitude2 "-a")
;; (my-transient-substitude2 "-A")
;; (my-transient-substitude2 "=a")
;; (my-transient-substitude2 "=A")

;; (my-transient-substitude2 "--")
;; (my-transient-substitude2 "-S")
;; (my-transient-substitude2 "=S")
;; (my-transient-substitude2 "C-x s")
;; (my-transient-substitude2 "RET")

;; (my-transient-substitude2 "-w")
;; (my-transient-substitude2 "C-h")
;; (my-transient-substitude2 "?")

(defun my-transient-substitude2 (key)
  ;; TODO: transient seem to assume subsitude fixes its ret
  ;; is this a bug?
  (if (get-text-property 0 'alan-transient-did-sub key)
      key
    (let ((ans (my-transient-substitude2-impl key)))
      (span-dbg ans)
      (propertize ans 'alan-transient-did-sub t))))


(eval-and-compile
  (defun alan-make-key-desc-rx (keys)
    `(seq
      " "
      ,@(-interleave keys (make-list (length keys) " "))))

  (rx-define alan-keys (&rest keys) (eval (alan-make-key-desc-rx '(keys)))))

(defun alan-keys (&rest keys)
  (apply #'concat " "
         (-interleave keys (make-list (length keys) " "))))

(defmacro alan-key-rep (rx val)
  (declare (indent 1))
  `(while (string-match (rx (alan-keys ,@rx)) key)
     (setq key (replace-match (alan-keys ,@val) t t key))))

(defun my-transient-substitude2-impl (key)
  (span-dbg key)
  (setq key (alan-normalize-key key))
  (let ((case-fold-search))

    (when (string-match-p (rx bos (alan-keys "SPC") eos) key)
      (setq key (alan-keys "S-SPC S-SPC")))

    ;; TODO: these can cause conflicts
    ;; (alan-key-rep ("-" (group (any "a-z")))
    ;;   ("<.>" (match-string 1 key)))

    ;; (alan-key-rep ("-" (group (any "A-Z")))
    ;;   ("SPC" (downcase (match-string 1 key))))

    ;; (alan-key-rep ("=" (group (any "a-z")))
    ;;   ("<.>" (upcase (match-string 1 key))))

    ;; (alan-key-rep ("=" (group (any "A-Z")))
    ;;   ("SPC" (match-string 1 key)))

    (alan-key-rep ("-") ("<.>"))
    (alan-key-rep ("=") ("SPC"))

    (alan-key-rep ("RET") ("<return>"))

    (setq key
          (concat " "
                  (key-description
                   (alan-transient-tranlate-key (key-parse key)))
                  " "))

    (when (string-match-p (rx (alan-keys (or "<up>" "<down>" "J" "K"))) key)
      (setq key (concat " S-SPC" key)))

    (key-description (key-parse key))))

(defvar transient-values nil)

(eval-after-load! transient

  (setq transient-detect-key-conflicts t)
  ;; (setq transient-detect-key-conflicts nil)
  (setq transient-default-level 5)
  (setq transient-show-common-commands nil)
  ;; (setq transient-show-common-commands t)
  (setq transient-semantic-coloring t)
  (setq transient-save-history nil)
  (setq transient--buffer-name "*transient*")

  (setq transient-show-during-minibuffer-read t)

  (general-def transient-map
    ;; "s" #'execute-extended-command
    "M-x" #'execute-extended-command
    ;; "w j" #'evil-window-down
    ;; "w k" #'evil-window-up
    ;; "%" #'pp-eval-expression

    ;; "w" evil-window-map
    "C-h" help-map
    "<escape>" #'transient-quit-one
    "<up>" #'transient-history-prev
    "<down>" #'transient-history-next
    "K" #'transient-scroll-down
    "J" #'transient-scroll-up

    "?" nil
    )

  (general-def transient-predicate-map
    [execute-extended-command] #'transient--do-stay
    ;; [evil-window-down] #'transient--do-stay
    ;; [evil-window-up] #'transient--do-stay
    [pp-eval-expression] #'transient--do-stay
    )

  (setq transient-substitute-key-function
        (lambda (obj)
          (span-dbg (oref obj key))
          ;; (span-notef "%s" (:ts (record-to-list obj)))
          (my-transient-substitude2 (oref obj key))))
  ;; (setq transient-substitute-key-function nil)

  (setq transient-display-buffer-action
        '(display-buffer-in-side-window
          (side . bottom)
          (dedicated . t)
          (inhibit-same-window . t)
          ;; (window-parameters (no-other-window . t))
          )
        )
  ;; (setq-default transient-mode-line-format 'line
  ;; (setq-default transient-enable-popup-navigation nil)
  )

(span-wrap transient--make-transient-map)

;; TODO: transient--make-redisplay-map seems to alwasy fail?
(defadvice! transient--make-redisplay-map-advice (&rest _args)
  :override #'transient--make-redisplay-map
  (make-sparse-keymap))

(defadvice! transient--show-after (&rest _)
  :after 'transient--show
  (with-current-buffer transient--buffer
    (setq window-size-fixed nil))
  (when (window-live-p transient--window)
    (with-selected-window transient--window
      ;; disable cursor in transient window
      (setq-local cursor-in-non-selected-windows nil)
      ;; allow windmove-up to move to the transient window
      (set-window-parameter nil 'no-other-window nil))))


(provide 'alan-transient)
