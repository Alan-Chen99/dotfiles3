;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)

(pkg! 'with-editor
  (startup-queue-package 'with-editor 0))

(eval-after-load! server
  (setq server-log t))


(eval-after-load! with-editor
  (add-hook! 'with-editor-mode-hook #'evil-normalize-keymaps)

  (run-with-idle-timer
   ;; delaying with timer makes it less likely to throw
   ;; (file-error "Cannot bind server socket" "Interrupted system call")
   0.5 nil
   #'alan-setup-with-editor)

  (clear-and-backup-keymap with-editor-mode-map)

  (general-def with-editor-mode-map
    :states 'motion
    "SPC SPC" #'with-editor-finish
    [remap alan-kill-current-buffer] #'with-editor-cancel)

  ;; (general-def with-editor-mode-map
  ;;   "SPC SPC" #'with-editor-finish
  ;;   [remap alan-kill-current-buffer] #'with-editor-cancel)
  )

(defvar alan-did-setup-with-editor nil)

(defun alan-setup-with-editor ()
  (require 'with-editor)
  (unless alan-did-setup-with-editor
    (span :with-editor-setup
      (with-temp-buffer
        (span-dbgf (getenv "EDITOR"))
        (setq alan-did-setup-with-editor t)
        (unwind-protect
            (setq process-environment (with-editor process-environment))
          (span-dbgf (getenv "EDITOR")))))))

;; A remote host cannot reach this Emacs's server socket, so emacsclient is
;; out.  with-editor's own sleeping editor prints its request as plain text,
;; which vterm draws instead of handing to a filter, and its $EDITOR is a
;; quoted "sh -c" string that breaks when a shell word-splits "$EDITOR file".
;; remote-editor.sh sends the request as an OSC 51;E sequence instead, and
;; $EDITOR is just its path.  The copy's name carries the content hash, so
;; an existing copy is current.
(defun alan-with-editor-remote-env ()
  "Environment entries for a shell started in `default-directory'.
On a remote host, $EDITOR becomes a copy of remote-editor.sh there.
Locally there are none: the global $EDITOR already runs emacsclient."
  (when-let* ((remote (file-remote-p default-directory)))
    (let* ((src (expand-file-name "remote-editor.sh" alan-dotemacs-dir))
           (hash (with-temp-buffer
                   (insert-file-contents-literally src)
                   (secure-hash 'sha256 (current-buffer))))
           ;; Under $HOME, not /tmp, where another user could plant the file.
           (dest (expand-file-name
                  (format "%s~/.cache/alan-emacs/remote-editor-%s" remote (substring hash 0 16)))))
      (unless (file-exists-p dest)
        (make-directory (file-name-directory dest) t)
        ;; Renamed into place so a copy cut short never passes the check above.
        (let ((tmp (format "%s.tmp%d" dest (emacs-pid))))
          (copy-file src tmp t)
          (set-file-modes tmp #o700)
          (rename-file tmp dest t)))
      (list (concat "EDITOR=" (file-local-name dest))))))

(defun alan-with-editor-open (pid hex)
  "Visit the file that remote-editor.sh process PID asks for.
HEX is its working directory and arguments, NUL-terminated and hex-encoded.
The file is on `default-directory''s host.  `with-editor-finish' and
`with-editor-cancel' signal PID there, ending it with status 0 or 1.  If
the file cannot be visited, PID is ended with status 1."
  (require 'with-editor)
  (condition-case err
      (let* ((bytes (apply #'unibyte-string
                           (mapcar (lambda (i) (string-to-number (substring hex i (+ i 2)) 16))
                                   (number-sequence 0 (- (length hex) 2) 2))))
             (fields (butlast (split-string (decode-coding-string bytes 'utf-8-unix) "\0")))
             (dir (car fields))
             (args (cdr fields)))
        (unless args
          (error "Remote $EDITOR was called without a file"))
        ;; The request is rebuilt in with-editor's own format, so +LINE:COL
        ;; arguments and the `with-editor-mode' setup match its sleeping editor.
        (let ((request (format "WITH-EDITOR: %s OPEN %s\037%s\037 IN %s"
                               pid (nth 0 args) (or (nth 1 args) "") dir)))
          (when (with-editor-sleeping-editor-filter nil request)
            (error "with-editor did not parse remote $EDITOR request %S" request))))
    (error
     (process-file "kill" nil nil nil "-s" "USR2" pid)
     (signal (car err) (cdr err)))))


(provide 'alan-with-editor)
