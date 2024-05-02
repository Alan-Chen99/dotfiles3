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

(defun my-transient-substitude2 (key)
  ;; TODO: transient seem to assume subsitude fixes its ret
  ;; is this a bug?
  (if (get-text-property 0 'alan-transient-did-sub key)
      key
    (let ((ans (my-transient-substitude2-impl key)))
      (propertize ans 'alan-transient-did-sub t))))


(eval-when-compile
  (defun alan-make-key-desc-rx (keys)
    `(seq
      " "
      ,@(-interleave keys (-cycle '(" ")))))

  (rx-define alan-keys (&rest keys) (eval (alan-make-key-desc-rx '(keys)))))

(defun alan-keys (&rest keys)
  (apply #'concat " "
         (-interleave keys (-cycle '(" ")))))

(defmacro alan-key-rep (rx val)
  (declare (indent 1))
  `(while (string-match (rx (alan-keys ,@rx)) key)
     (setq key (replace-match (alan-keys ,@val) t t key))))

(defun my-transient-substitude2-impl (key)
  (span-dbgf key)
  (setq key (alan-normalize-key key))
  (let ((case-fold-search))

    (when (string-match-p (rx bos (alan-keys "SPC") eos) key)
      (setq key (alan-keys "S-SPC S-SPC")))

    (alan-key-rep ("-" (group (any "a-z")))
      ("<.>" (match-string 1 key)))

    (alan-key-rep ("-" (group (any "A-Z")))
      ("SPC" (downcase (match-string 1 key))))

    (alan-key-rep ("=" (group (any "a-z")))
      ("<.>" (upcase (match-string 1 key))))

    (alan-key-rep ("=" (group (any "A-Z")))
      ("SPC" (match-string 1 key)))

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

  (general-def transient-map
    ;; "s" #'execute-extended-command
    "M-x" #'execute-extended-command

    ;; "w" evil-window-map
    "C-h" help-map
    "<escape>" #'transient-quit-one
    "<up>" #'transient-history-prev
    "<down>" #'transient-history-next
    "K" #'transient-scroll-down
    "J" #'transient-scroll-up

    )

  (general-def transient-predicate-map
    [execute-extended-command] #'transient--do-stay)

  (setq transient-substitute-key-function
        (lambda (obj)
          (span-dbgf (oref obj key))
          (my-transient-substitude2 (oref obj key))))
  ;; (setq transient-substitute-key-function nil)


  ;; TODO: transient--make-redisplay-map seems to alwasy fail?
  (defadvice! transient--make-redisplay-map-advice (&rest _args)
    :override #'transient--make-redisplay-map
    (make-sparse-keymap))

  (setq transient-display-buffer-action
        '(display-buffer-in-side-window
          (side . bottom)
          (dedicated . t)
          ;; (inhibit-same-window . t)
          ;; (window-parameters (no-other-window . t))
          )
        )
  ;; (setq-default transient-mode-line-format 'line
  ;; (setq-default transient-enable-popup-navigation nil)
  (defadvice! transient--show-inhibit-cursor (&rest _)
    :after 'transient--show
    (when (window-live-p transient--window)
      (with-selected-window transient--window
        (setq-local cursor-in-non-selected-windows nil))))

  )



(provide 'alan-transient)
