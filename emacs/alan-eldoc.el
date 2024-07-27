(require 'alan-core)
(require 'evil)

(startup-queue-package 'eldoc 50)

(defun resolve-display-text-prop (str)
  (let ((ans nil)
        (p 0)
        (nx nil)
        (l (length str)))
    (while p
      (setq nx (next-single-property-change p 'display str))
      (let* ((part (substring str p (or nx l)))
             (prop (text-properties-at 0 part))
             (disp (plist-get prop 'display)))
        (if (and disp (stringp disp))
            (push (apply #'propertize disp prop) ans)
          (push part ans)))
      (setq p nx))
    (apply #'concat (nreverse ans))))

(defun my-eldoc--echo-area-substring (available)
  (let ((ans (resolve-display-text-prop (buffer-string))))
    (setq ans (replace-regexp-in-string (rx "\n\n") "\n" ans))
    ;; (setq ans (string-join (string-split ans "\n" t) "\n"))
    ans))


(eval-after-load! eldoc
  (setq
   eldoc-idle-delay 0.05
   eldoc-echo-area-display-truncation-message nil)
  (advice-add #'eldoc--message-command-p :before-while
              (lambda (&rest args) (not prefix-arg)))
  (advice-add #'eldoc--echo-area-substring :override #'my-eldoc--echo-area-substring)
  (global-eldoc-mode)

  (eldoc-add-command #'delete-backward-char)
  (eldoc-add-command #'evil-delete-backward-char-and-join)
  (eldoc-remove-command #'evil-window-delete))



(provide 'alan-eldoc)
