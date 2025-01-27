;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-transient)

(require 'evil)

(pkg! 'magit
  (startup-queue-package 'magit -50))

(eval-after-load! smerge-mode
  (setq smerge-command-prefix (kbd "SPC"))

  (clear-and-backup-keymap smerge-basic-map)
  (general-def smerge-basic-map
    ;; :states 'motion
    "j" #'smerge-next
    "k" #'smerge-prev
    "RET" #'smerge-keep-current
    )

  (clear-and-backup-keymap smerge-mode-map)
  (general-def smerge-mode-map
    :states 'motion
    "SPC" smerge-basic-map)

  (add-hook! smerge-mode-hook #'evil-normalize-keymaps)

  ;; (setq smerge-mode-map (key-description smerge-command-prefix) smerge-basic-map)

  )

(eval-after-load! magit

  (clear-and-backup-keymap magit-section-mode-map)
  (clear-and-backup-keymap magit-mode-map)

  (general-def magit-diff-section-map
    ;; by defualt, magit-do-async-shell-command
    ;; this takes precedence over evil since its "local"
    ;; which is why this is in effect
    "&" nil
    )

  (general-def magit-section-mode-map
    :states 'motion
    "SPC SPC" #'magit-section-toggle
    ;; #'magit-section-show-level-1
    [remap backward-paragraph] #'magit-section-backward-sibling
    [remap forward-paragraph] #'magit-section-forward-sibling)


  (general-def magit-mode-map
    :states 'motion
    "RET" #'magit-visit-thing
    "S-DEL" #'magit-discard

    "#" #'magit-unstage
    ":" #'magit-stage

    "+" #'magit-diff-more-context
    "-" #'magit-diff-less-context
    "0" #'magit-diff-default-context

    "d" #'magit-diff
    "c" #'magit-commit
    "b" #'magit-branch
    "p" #'magit-push

    ;; TODO: when can i use these?
    "SPC e" #'magit-edit-thing
    "SPC v" #'magit-browse-thing
    "SPC c" #'magit-copy-thing
    )

  ;; TODO: doesnt work, why?
  (setq-default magit-buffer-log-args '("--graph" "--color" "--decorate" "-n256"))
  (setf (alist-get 'magit-log:magit-log-mode transient-values)
        (default-value 'magit-buffer-log-args))

  (add-to-list 'debug-ignored-errors 'magit-outside-git-repo)
  )



(provide 'alan-magit)
