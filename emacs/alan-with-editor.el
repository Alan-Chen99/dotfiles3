;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)

;; with-editor's, declared so that compiling this file without with-editor
;; loaded still binds the hook in `alan-with-editor-open' dynamically.
(defvar with-editor-filter-visit-hook)
(defvar with-editor-usage-message)
(defvar with-editor--pid)
(defvar with-editor-mode)

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
;; $EDITOR is just the path of its copy on the remote host.
(defun alan-with-editor-install (name)
  "Copy script NAME from `alan-dotemacs-dir' to `default-directory''s host.
Return the copy's file name on that host, without the remote prefix.
The copy's name carries the content hash, so an existing copy is current."
  (let* ((src (expand-file-name name alan-dotemacs-dir))
         (hash (with-temp-buffer
                 (insert-file-contents-literally src)
                 (secure-hash 'sha256 (current-buffer))))
         ;; Under $HOME, not /tmp, where another user could plant the file.
         (dest (expand-file-name
                (format "%s~/.cache/alan-emacs/%s-%s"
                        (file-remote-p default-directory)
                        (file-name-sans-extension name)
                        (substring hash 0 16)))))
    (unless (file-exists-p dest)
      (make-directory (file-name-directory dest) t)
      ;; Renamed into place so a copy cut short never passes the check above.
      (let ((tmp (format "%s.tmp%d" dest (emacs-pid))))
        (copy-file src tmp t)
        (set-file-modes tmp #o700)
        (rename-file tmp dest t)))
    (file-local-name dest)))

(defun alan-with-editor-remote-env ()
  "Environment entries for a shell started in `default-directory'.
On a remote host, $EDITOR becomes a copy of remote-editor.sh there.
Locally there are none: the global $EDITOR already runs emacsclient."
  (when (file-remote-p default-directory)
    (list (concat "EDITOR=" (alan-with-editor-install "remote-editor.sh")))))

;; Every $EDITOR this Emacs answers -- emacsclient, tmux-editor.sh, and
;; remote-editor.sh on a remote host -- opens a file the same way, since the
;; calling program keeps or drops the edit by the editor's exit status.  A
;; plain file is a plain buffer: killing it ends the editor with status 0,
;; after kill-buffer's usual prompt if it has unsaved changes, so saving and
;; then killing keeps the edit.  Only buffers that git-commit or git-rebase
;; put in `with-editor-mode' differ: finishing ends the editor with status 0,
;; and cancelling, which killing does there, with 1.  No way of ending deletes
;; the file.

(defvar-local alan-with-editor--pids nil
  "remote-editor.sh processes waiting on this buffer, as (PID . DIRECTORY).
The counterpart of `server-buffer-clients' for a plain file; `kill' runs
in DIRECTORY, on the host of PID.")
(put 'alan-with-editor--pids 'permanent-local t)

(defvar alan-with-editor--end-signal "USR1"
  "Signal `alan-with-editor--end' sends.
remote-editor.sh exits with status 0 on USR1 and 1 on USR2.")

(defun alan-with-editor--end ()
  "End the remote-editor.sh processes waiting on the current buffer.
Killing the buffer runs this, as it runs `server-kill-buffer' for
emacsclient."
  (dolist (p alan-with-editor--pids)
    (let* ((default-directory (cdr p))
           (rc (process-file "kill" nil nil nil
                             "-s" alan-with-editor--end-signal (car p))))
      ;; A warning, not an error: an error would leave the buffer unkillable
      ;; once the process is gone, as after C-c in its shell.
      (unless (eq rc 0)
        (display-warning 'alan-with-editor
                         (format "kill -s %s %s in %s exited %s"
                                 alan-with-editor--end-signal (car p)
                                 default-directory rc))))))

(add-hook 'kill-buffer-hook #'alan-with-editor--end)

(defadvice! alan-with-editor-return-keep-file (fn cancel)
  "Finish or cancel for a sleeping editor as for emacsclient.
With no emacsclient waiting, `with-editor-return' deletes the file on
cancel, a fallback for Emacs run directly as $EDITOR, whose exit status
cannot say the edit was cancelled.  A sleeping editor's can, so its file
is kept, and remote-editor.sh processes waiting on a plain buffer end with
the status with-editor would give."
  :around #'with-editor-return
  (if (not (or with-editor--pid alan-with-editor--pids))
      (funcall fn cancel)
    (let ((file buffer-file-name)
          (delete (symbol-function 'delete-file))
          (alan-with-editor--end-signal (if cancel "USR2" "USR1")))
      (cl-letf (((symbol-function 'delete-file)
                 (lambda (name &rest args)
                   (unless (equal name file)
                     (apply delete name args)))))
        (funcall fn cancel)))))

;; Emacs's own backup copies a file before its first save, so for a commit
;; message it holds git's template, and the auto-save file is deleted on save
;; and reused by the next edit of the same file.  The caller then overwrites
;; or deletes the file: git on the next commit, and callers that edit a
;; temporary file once they read it.  So an edit that was cancelled, killed
;; unsaved or rejected by the caller survives only in the copy written here
;; when its buffer is killed.  A buffer that emacsclient found already open is
;; buried instead, and keeps the edit itself.

(defvar alan-with-editor-backup-directory
  (expand-file-name "with-editor-backups/" user-emacs-directory)
  "Where a buffer that an $EDITOR waits on is copied when killed.
Each copy is named after the file, with / as !, and the time of the kill.")

(defun alan-with-editor--backup ()
  "Copy the current buffer to `alan-with-editor-backup-directory'."
  (make-directory alan-with-editor-backup-directory t)
  (let ((name (decode-coding-string
               (string-limit
                (concat (subst-char-in-string
                         ?/ ?! (or buffer-file-name (buffer-name)))
                        (format-time-string "~%Y%m%dT%H%M%S.%3N"))
                ;; NAME_MAX bytes; the end, with the file and time, is kept.
                255 t 'utf-8)
               'utf-8))
        ;; Encodes any buffer text without asking, which a kill cannot.
        (coding-system-for-write 'utf-8-emacs-unix))
    (save-restriction
      (widen)
      (write-region nil nil
                    (expand-file-name name alan-with-editor-backup-directory)
                    nil 'quiet nil 'excl))))

(put 'alan-with-editor--backup 'permanent-local-hook t)

(defun alan-with-editor--mark ()
  "Copy the current buffer when it is killed.
Buffer-local and first: the local hook runs before the global one, and
server.el's own entry, which releases emacsclient, is local too.  A copy
that fails then leaves the buffer alive and its $EDITOR still waiting."
  (add-hook 'kill-buffer-hook #'alan-with-editor--backup -90 t))

(add-hook 'with-editor-filter-visit-hook #'alan-with-editor--mark)

(defadvice! alan-with-editor-mark-client-buffers (fn files proc &optional nowait)
  "Copy the buffers that emacsclient waits on when they are killed.
An emacsclient -n does not wait, and is not an $EDITOR."
  :around #'server-visit-files
  (let ((buffers (funcall fn files proc nowait)))
    (unless nowait
      (dolist (b buffers)
        (with-current-buffer b
          (alan-with-editor--mark))))
    buffers))

(defun alan-with-editor-open (pid hex)
  "Visit the file that remote-editor.sh process PID asks for.
HEX is its working directory and arguments, NUL-terminated and hex-encoded.
The file is on `default-directory''s host.  The buffer ends PID as an
emacsclient buffer ends its client: killing a plain file ends PID with
status 0, and a buffer that git-commit or git-rebase puts in
`with-editor-mode' ends it with status 0 on finishing and 1 on cancelling.
If the file cannot be visited, PID is ended with status 1."
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
        ;; arguments and commit messages match its sleeping editor.
        (let* ((request (format "WITH-EDITOR: %s OPEN %s\037%s\037 IN %s"
                                pid (nth 0 args) (or (nth 1 args) "") dir))
               (visit (symbol-function 'find-file-noselect))
               (enabled nil)
               ;; The sleeping editor turns on `with-editor-mode' for every
               ;; file once visited.  A file the visit left without it is one
               ;; emacsclient would open as a plain buffer.
               (with-editor-filter-visit-hook
                (cons (lambda ()
                        (unless enabled
                          ;; The usage message the mode shows on a timer is
                          ;; read when the timer runs.
                          (setq with-editor-usage-message
                                "When done with this buffer, kill it")
                          (with-editor-mode -1)
                          ;; The mode adds this when turned off too.
                          (remove-hook 'kill-buffer-query-functions
                                       #'with-editor-kill-buffer-noop t)
                          (push (cons with-editor--pid default-directory)
                                alan-with-editor--pids)
                          (setq with-editor--pid nil)))
                      with-editor-filter-visit-hook)))
          (cl-letf (((symbol-function 'find-file-noselect)
                     (lambda (&rest visit-args)
                       (let ((buf (apply visit visit-args)))
                         ;; Set last by the outermost visit, the filter's own.
                         (setq enabled (buffer-local-value 'with-editor-mode buf))
                         buf))))
            (when (with-editor-sleeping-editor-filter nil request)
              (error "with-editor did not parse remote $EDITOR request %S" request)))))
    (error
     (process-file "kill" nil nil nil "-s" "USR2" pid)
     (signal (car err) (cdr err)))))


(provide 'alan-with-editor)
