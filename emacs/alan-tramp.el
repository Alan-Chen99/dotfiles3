;; -*- lexical-binding: t -*-

(require 'alan-core)

;; https://emacs.stackexchange.com/questions/18262/tramp-how-to-add-a-agent-forwarding-to-ssh-connections
(eval-after-load! tramp

  ;; (setq tramp-inhibit-progress-reporter t)
  (setq tramp-connection-timeout 5)
  (setq tramp-histfile-override nil)

  ;; (add-to-list 'tramp-connection-properties
  ;;              (list (regexp-quote "/ssh:host@192.168.0.238:")
  ;;                    "login-args"
  ;;                    '(("-A") ("-l" "%u") ("-p" "%p") ("%c")
  ;;                      ("-e" "none") ("%h"))

  ;;                    ;; "remote-shell" "/usr/bin/bash"

  ;;                    ;; "direct-async-process" t
  ;;                    ))

  ;; ;; https://stackoverflow.com/questions/26630640/tramp-ignores-tramp-remote-path
  ;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; ;; TODO: this sends a "kill" but that seems to never work?
  ;; (advice-add #'tramp-interrupt-process :override #'ignore)

  ;; use by rundocker.py script in this repo
  (add-to-list 'tramp-remote-path "/docker_host_prof/bin")
  ;; newer, easy to run stuff in prior generations
  (add-to-list 'tramp-remote-path "/nix-host-profiles/profile/bin")
  ;; apptainer containers: nix_store bind-mounted at /nix
  (add-to-list 'tramp-remote-path "/nix/state/profile/bin")
  (add-to-list 'tramp-remote-path "/nix/nix_path/bin")

  (connection-local-set-profile-variables
   'alan-custom-vars
   '(
     (tramp-direct-async-process . t)
     (shell-file-name . "/bin/bash")
     (shell-command-switch . "-c")))
  (connection-local-set-profiles nil 'alan-custom-vars)

  ;; tramp-search-regexp searches backward from point-max within the
  ;; last 256 chars.  bos anchors are unreachable when SSH banner text
  ;; pushes the prompt beyond that window.
  (setq tramp-password-prompt-regexp
        (rx-to-string
         ;; this "group" is shown as prompt
         `(group
           (or
            ;; 2025/11/17
            ;; MIT engaging: after this message requires a enter to continue
            (: (* anything) "maintenance reservation midnight Tuesday morning." (* anything))
            (: (* anything)
               (| . ,password-word-equivalents)
               (* nonl)
               (any . ,(or (bound-and-true-p tramp-compat-password-colon-equivalents)
                           '(?\N{COLON}
                             ?\N{FULLWIDTH COLON}
                             ?\N{SMALL COLON}
                             ?\N{PRESENTATION FORM FOR VERTICAL COLON}
                             ?\N{KHMER SIGN CAMNUC PII KUUH})))
               (? "\^@") (* blank)))
           )))

  ;; https://emacs.stackexchange.com/questions/62919/how-to-disable-magit-on-remote-files-with-tramp
  (setq vc-ignore-dir-regexp
        (rx-to-string
         '(seq bos
               (or (seq (any "/\\") (any "/\\")
                        (one-or-more (not (any "/\\")))
                        (any "/\\"))
                   (seq "/" (or "net" "afs" "...") "/")
                   ;; Ignore all tramp paths.
                   (seq "/"
                        (eval (cons 'or (mapcar #'car tramp-methods)))
                        ":"
                        (zero-or-more anything)))
               eos)))

  (tramp-enable-flatpak-method)
  (tramp-enable-apptainer-method)
  )


;; `file-chase-links' resolves a symlink target against the link's own
;; directory using `files--splice-dirname-file'.  That helper looks up a
;; file-name handler for the target and, finding none for a bare absolute
;; path, quotes it into the local namespace: chasing
;; /ssh:host:/dir/link -> /abs/target yields "/:/abs/target".  Its own
;; docstring calls the quoting "dubious if DIRNAME is magic".
;;
;; Only absolute targets are affected.  A relative target is concatenated
;; onto the remote directory and keeps its /ssh: prefix, so two links to
;; the same file behave differently according to how the link was written.
;;
;; The elisp manual (node "Truenames") specifies the return value as the
;; name of the file at the end of the chain, and callers rely on that.
;; `backup-buffer' copies from it before every save: where /abs/target is
;; absent locally the save aborts with `file-missing', and where it happens
;; to exist locally the backup silently captures that unrelated local file.
(defun alan-file-chase-links-remote (orig filename &optional limit)
  "Chase links in FILENAME, resolving targets in FILENAME's remote namespace.
An absolute symlink target of a remote FILENAME names a remote file;
`file-chase-links' hands it back as a local name instead."
  (let ((remote (file-remote-p filename)))
    (if (null remote)
        (funcall orig filename limit)
      (let ((newname filename)
            (count 0)
            target)
        (while (and (or (null limit) (< count limit))
                    (setq target (file-symlink-p newname)))
          (save-match-data
            (when (and (null limit) (= count 100))
              (error "Apparent cycle of symbolic links for %s" filename))
            ;; In the context of a link, `//' is a plain separator, not the
            ;; `expand-file-name' escape back to the filesystem root.
            (setq target (replace-regexp-in-string "//+" "/" target))
            (setq newname
                  (expand-file-name
                   (if (file-name-absolute-p target) (concat remote target) target)
                   (file-name-directory newname)))
            (setq count (1+ count))))
        newname))))

(advice-add #'file-chase-links :around #'alan-file-chase-links-remote)

(require-if-is-bytecompile
 tramp tramp-sh)

(setq tramp-verbose 4)

(advice-add #'tramp-debug-message :override #'span--wrap-tramp-debug-message)
(defun span--wrap-tramp-debug-message (_vec fmt-string &rest arguments)
  (let* ((signal-hook-function #'span--signal-hook-function)
         (msg (apply #'format-message fmt-string arguments)))
    (span-notef "%s" msg)))

(advice-add #'tramp-send-command :around #'span--wrap-tramp-send-command)
(defun span--wrap-tramp-send-command (orig-fn vec command &optional neveropen nooutput)
  (span (:tramp-send-command "%s%s" (if neveropen "neveropen " "") (if nooutput "nooutput " ""))
    (span-flush)
    (span-note ">>>>>>>>>>\n%s\n<<<<<<<<<<" command)
    (let ((ans (funcall orig-fn vec command neveropen nooutput)))
      ;; (span-note "result:\n %s" ans)
      ans)))

(advice-add #'tramp-wait-for-output :around #'span--wrap-tramp-wait-for-output)
(defun span--wrap-tramp-wait-for-output (orig-fn proc &optional timeout)
  (span :tramp-wait-for-output
    (let* ((time-start (span--time)))
      (unwind-protect
          (let ((ans (funcall orig-fn proc timeout)))
            (with-current-buffer (process-buffer proc)
              (span-notef "%s" (buffer-string)))
            ans)
        (let ((time (float-time (time-subtract (span--time) time-start))))
          (span-notef "took: %.3f" time))))))

(advice-add #'tramp-process-one-action :around #'span--wrap--tramp-process-one-action)
(defun span--wrap--tramp-process-one-action (orig-fn proc vec actions)
  (let (res normal-exit)
    (span :tramp-process-one-action
      (span-flush)
      (span-note "%s" (span-fmt-to-string actions))
      (setq res (catch 'tramp-action
                  (funcall orig-fn proc vec actions)
                  (setq normal-exit t)))
      (span-note "tramp-process-one-action -> %S" (span-fmt-to-string res)))
    (if normal-exit
        res
      (throw 'tramp-action res))))

;; (advice-add #'tramp-send-string :around #'span--wrap-tramp-send-string)
;; (defun span--wrap-tramp-send-string (orig-fn vec string)
;;   ;; (span-note "tramp-send-string: %S" (tramp-get-connection-name vec))
;;   (span-note "%s" string)
;;   (span-flush)
;;   (funcall orig-fn vec string)
;;   ;; (unwind-protect
;;   ;;     (funcall orig-fn vec string)
;;   ;;   (span-note "----------"))
;;   )

;; (advice-add #'tramp-search-regexp :around #'span--wrap-tramp-search-regexp)
;; (defun span--wrap-tramp-search-regexp (orig-fn regexp)
;;   (let ((ans (funcall orig-fn regexp)))
;;     (when (and ans (not (string= regexp (rx (group (regexp tramp-process-alive-regexp)) eos))))
;;       (span :span--wrap-tramp-search-regexp
;;         (span-notef "%s" (buffer-string))
;;         (span-notef "%s" (buffer-substring (point) (point-max)))))
;;     ans))

(span-wrap tramp-maybe-open-connection)
;; (span-wrap tramp-sh-handle-make-process)
(span-instrument tramp-sh-handle-make-process :verbose t)
(span-instrument tramp-process-actions :verbose t)
(span-instrument tramp-action-password)
(span-instrument tramp-open-shell)
(span-instrument tramp-handle-make-process)

(provide 'alan-tramp)
