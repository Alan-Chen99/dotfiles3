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
  (add-to-list 'tramp-remote-path "/docker_host_bin")

  (connection-local-set-profile-variables
   'alan-custom-vars
   '(
     (tramp-direct-async-process . t)
     (shell-file-name . "/bin/bash")
     (shell-command-switch . "-c")))
  (connection-local-set-profiles nil 'alan-custom-vars)

  (setq tramp-password-prompt-regexp
        (rx-to-string
         ;; this "group" is shown as prompt
         `(group
           (: bos (* anything)
              (| . ,password-word-equivalents)
              (* nonl) (any . ,(or (bound-and-true-p tramp-compat-password-colon-equivalents)
                                   '(?\N{COLON}
                                     ?\N{FULLWIDTH COLON}
                                     ?\N{SMALL COLON}
                                     ?\N{PRESENTATION FORM FOR VERTICAL COLON}
                                     ?\N{KHMER SIGN CAMNUC PII KUUH})))
              (? "\^@") (* blank)))))

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
               eos))))


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
(span-wrap tramp-sh-handle-make-process)
(span-instrument tramp-process-actions :verbose t)
(span-instrument tramp-action-password)
(span-instrument tramp-open-shell)
(span-instrument tramp-handle-make-process)

(provide 'alan-tramp)
