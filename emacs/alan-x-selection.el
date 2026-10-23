;; -*- lexical-binding: t -*-

(require 'alan-core)

(require-if-is-bytecompile xcb xcb-sync)

(pkg! 'xelb)

;; Emacs claims an X selection with the time of the last input event it saw,
;; and records ownership whether or not the X server took the claim
;; (bug#82091).  The server ignores a claim older than the selection's last
;; change, even after that owner has gone, and Emacs, never having been the
;; owner, gets no SelectionClear: its yanks return its own text while other
;; programs paste the previous owner's.  Claims from timers, process filters
;; (vterm's OSC 52) and unfocused frames carry an input time that another
;; client's claim can postdate.  Asking the owner for its claim time does not
;; help: cosmic-comp's Xwayland bridge refuses every request from X while no
;; X window has keyboard focus, and an owner that has gone cannot be asked.
;;
;; So every claim is checked as ICCCM 2.1 requires: ask the server who owns
;; the selection, and if the claim was ignored, claim again at the server's
;; current time, which no earlier claim can postdate.  Emacs Lisp can ask
;; neither question on Emacs's own connection; a second one, through xelb,
;; asks both.

(defvar alan-x-selection--connections nil
  "Alist from X display name to (CONNECTION . SERVERTIME-COUNTER).
A nil value records a display xelb cannot reach.")

(defun alan-x-selection--reply (conn request)
  (or (xcb:+request-unchecked+reply conn request)
      (error "X server sent no reply to %s" (eieio-object-class request))))

(defun alan-x-selection--connect (display)
  (require 'xcb)
  (require 'xcb-sync)
  ;; xelb takes the xauth cookie for $DISPLAY.
  (let ((conn (with-environment-variables (("DISPLAY" display))
                (xcb:connect display))))
    (set-process-query-on-exit-flag (slot-value conn 'process) nil)
    ;; xelb sends an extension's requests with major opcode 0 until it has
    ;; looked the extension up.
    (xcb:get-extension-data conn 'xcb:sync)
    (alan-x-selection--reply conn (make-instance 'xcb:sync:Initialize
                                                 :desired-major-version 3
                                                 :desired-minor-version 1))
    (cons conn
          (slot-value
           (or (seq-find (lambda (counter)
                           (equal (slot-value counter 'name) "SERVERTIME"))
                         (slot-value (alan-x-selection--reply
                                      conn (make-instance 'xcb:sync:ListSystemCounters))
                                     'counters))
               (error "X display %s has no SERVERTIME counter" display))
           'counter))))

(defun alan-x-selection--connection (display)
  (let ((entry (assoc display alan-x-selection--connections)))
    (unless (and entry (or (null (cdr entry))
                           (process-live-p (slot-value (cadr entry) 'process))))
      (setf (alist-get display alan-x-selection--connections nil nil #'equal)
            (condition-case err
                (alan-x-selection--connect display)
              ;; Emacs cannot connect to an abstract socket, which is all an
              ;; X server listens on when /tmp/.X11-unix is not root's, as for
              ;; the Xvfb displays of agent sessions in containers.
              (file-error
               (display-warning
                'alan-x-selection
                (format "xelb cannot reach X display %s (%s); the server can ignore Emacs's selection claims there, and Emacs then yanks its own text"
                        display (error-message-string err))
                :warning)
               nil))))
    (alist-get display alan-x-selection--connections nil nil #'equal)))

(defun alan-x-selection--owner-p (conn selection frame)
  ;; A round trip on Emacs's own connection flushes its claim, still in
  ;; Xlib's output buffer, to the server before CONN asks about it.
  (x-window-property "_ALAN_X_SELECTION_SYNC" frame)
  (let ((name (symbol-name selection)))
    (= (slot-value
        (alan-x-selection--reply
         conn (make-instance 'xcb:GetSelectionOwner
                             :selection (slot-value
                                         (alan-x-selection--reply
                                          conn (make-instance 'xcb:InternAtom
                                                              :only-if-exists 0
                                                              :name-len (string-bytes name)
                                                              :name name))
                                         'atom)))
        'owner)
       (string-to-number (frame-parameter frame 'window-id)))))

(when (fboundp 'x-own-selection-internal)
  (defadvice! alan-x-selection-verify-claim (fn selection value &optional frame)
    :around #'x-own-selection-internal
    (prog1 (funcall fn selection value frame)
      (let* ((frame (or frame (selected-frame)))
             (clock (alan-x-selection--connection (frame-parameter frame 'display))))
        (when (and clock (not (alan-x-selection--owner-p (car clock) selection frame)))
          (x-display-set-last-user-time
           (slot-value (slot-value (alan-x-selection--reply
                                    (car clock) (make-instance 'xcb:sync:QueryCounter
                                                               :counter (cdr clock)))
                                   'counter-value)
                       'lo)
           frame)
          (funcall fn selection value frame)
          (unless (alan-x-selection--owner-p (car clock) selection frame)
            (error "X server ignored Emacs's claim of %s even at the server's current time"
                   selection)))))))

(provide 'alan-x-selection)
