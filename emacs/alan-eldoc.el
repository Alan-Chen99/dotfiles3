(require 'alan-core)
(require 'evil)

(startup-queue-package 'eldoc 50)

(eval-after-load! eldoc
  (setq
   eldoc-idle-delay 0.05
   eldoc-echo-area-display-truncation-message nil)
  (advice-add #'eldoc--message-command-p :before-while
              (lambda (&rest args) (not prefix-arg)))
  (global-eldoc-mode)

  (eldoc-add-command #'delete-backward-char)
  (eldoc-add-command #'evil-delete-backward-char-and-join)
  (eldoc-remove-command #'evil-window-delete))



(provide 'alan-eldoc)
