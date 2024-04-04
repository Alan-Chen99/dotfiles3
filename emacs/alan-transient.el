;; -*- lexical-binding: t -*-

(require 'alan-core)
(pkg! 'transient)

(defun transient-maybe-tranlate-key (key)
  (when-let ((translate (keymap-lookup key-translation-map (key-description key))))
    (when (and (not (numberp translate)) (not (equal translate (kbd "<escape>"))))
      translate)))
(defun my-transient-substitude (key)
  (let (did-downcase translate (key (kbd key)))
    (setq key
          (cl-map 'vector
                  (lambda (char)
                    (elt
                     (pcase (key-description (vector char))
                       ("-" (kbd "<leader>"))
                       ("=" (kbd "SPC"))
                       ("RET" (kbd "<return>"))
                       (_ (vector char)))
                     0))
                  key))
    (setq translate (transient-maybe-tranlate-key key))
    (if translate
        (setq key translate)
      (setq key
            (cl-map 'vector
                    (lambda (char)
                      (if (characterp char)
                          (progn
                            (setq did-downcase
                                  (or did-downcase
                                      (not (eq char (downcase char)))))
                            (downcase char))
                        char))
                    key))
      (when did-downcase
        (setq key
              (cl-map 'vector
                      (lambda (char)
                        (if (eq char 'leader)
                            (elt (kbd "SPC") 0)
                          char))
                      key)))
      (setq key (or (transient-maybe-tranlate-key key) key)))
    (key-description key)))


(eval-after-load! transient
  (general-def transient-map
    "s" #'execute-extended-command
    "M-x" #'execute-extended-command
    "C-h" help-map
    "<escape>" #'transient-quit-one
    "<up>" #'transient-history-prev
    "<down>" #'transient-history-next)

  (general-def transient-predicate-map
    [execute-extended-command] #'transient--do-stay)

  (setq-default transient-substitute-key-function
                (lambda (obj)
                  (my-transient-substitude (oref obj key))))
  ;; (setq transient-substitute-key-function nil)

  (defadvice! transient--make-redisplay-map-advice (&rest _args)
    :override 'transient--make-redisplay-map
    (make-sparse-keymap))
  ;; (key-description
  ;;    (cl-map 'vector
  ;;        (lambda (char)
  ;;            (elt
  ;;                (pcase (key-description (vector char))
  ;;                    ("-" (kbd "SPC"))
  ;;                    ("=" (kbd "<leader>"))
  ;;                    (_ (vector (downcase char))))
  ;;                0))
  ;;        (kbd "= 1 A")))
  ;; ;; (elt (kbd "<leader>") 0)
  ;; (kbd "SPC 1 <leader>")

  ;; (lookup-key key-translation-map "A")
  ;; (kbd "-")
  ;; (replace-regexp-in-string (rx "=") " SPC " "==efw=")
  ;; (kbd "SPC")

  (setq-default transient-save-history nil)
  ;; (setq-default transient-detect-key-conflicts t)
  (setq-default transient-show-common-commands nil)
  ;; transient-history-prev
  ;; (setq-default transient-semantic-coloring t)
  ;; transient-substitute-key-function
  (setq-default transient-display-buffer-action
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
