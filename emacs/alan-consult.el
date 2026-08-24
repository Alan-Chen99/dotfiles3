;; -*- lexical-binding: t -*-

(require 'alan-core)


(pkg! 'consult
  (startup-queue-package 'consult 50))

(eval-after-load! consult
  (setq-default
   consult-narrow-key "<"
   consult-line-numbers-widen t
   consult-async-min-input 2
   consult-async-refresh-delay  0.15
   consult-async-input-throttle 0.2
   ;; TODO: maybe 0.2 for tramp and 0.1 local?
   consult-async-input-debounce 0.5)

  (setq consult-fd-args '("fd" "--full-path --color=never --hidden"))

  ;; (string-split consult-ripgrep-args " ")
  (setq consult-ripgrep-args
        '("rg" "--null" "--line-buffered" "--color=never" "--max-columns=1000" "--path-separator" "/"
          "--smart-case" "--no-heading" "--with-filename" "--line-number" "--search-zip"
          ;; added
          "--hidden"
          ;; disable .git/ which is hidden, but its not ignored by gitignore
          "--glob" "!.git/*"
          ))

  ;; (setq consult-ripgrep-args
  ;;       "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
  ;;  --smart-case --no-heading --with-filename --line-number --search-zip")

  (general-def
    [remap switch-to-buffer] #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window)

  (setq completion-in-region-function #'consult-completion-in-region)

  ;; Match against filenames/directories in consult-buffer, not just
  ;; buffer names.  Appended as invisible text so orderless matches it
  ;; but display is unchanged (marginalia still shows the annotation).
  ;;
  ;; This runs over every buffer on each `consult-buffer' call, so the
  ;; path has to be abbreviated without touching the file system.
  ;; `abbreviate-file-name' dispatches to Tramp for a remote name, which
  ;; asks the host for its home directory and case sensitivity; on a
  ;; connection that is live but unresponsive that wait has no timeout.
  (plist-put consult--source-buffer :items
             (lambda ()
               (consult--buffer-query
                :sort 'visibility
                :as (lambda (buf)
                      (let* ((name (buffer-name buf))
                             (extra (or (buffer-file-name buf)
                                        (buffer-local-value 'default-directory buf))))
                        (if extra
                            (cons (concat name (propertize
                                                (concat " " (consult--fast-abbreviate-file-name extra))
                                                'invisible t))
                                  buf)
                          (cons name buf))))))))

(defun alan-consult-complete ()
  (interactive)
  (let ((completion-in-region-function #'consult-completion-in-region))
    (completion-at-point)))

(provide 'alan-consult)
