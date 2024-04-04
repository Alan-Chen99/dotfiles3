;; -*- lexical-binding: t -*-

(require 'alan-utils2)

(cl-assert (= (minibuffer-depth) 0))

;; simple async loop
(defvar async-queue nil)
(defvar process-queue-thread-exist nil)
(defun alan-startup-schedual-fn (priority func)
  ;; same priority => "last in first out"
  (push func (alist-get priority async-queue nil nil #'=))
  (when alan-finished-early-init
    (unless process-queue-thread-exist
      (setq process-queue-thread-exist t)
      (alan-startup-process-queue))))

(defun alan-startup-process-queue ()
  (if nil
      (run-with-idle-timer 0.01 nil
                           #'alan-startup-process-queue)
    (if-let
        (
         (async-queue)
         (func (pop (cdr (--max-by (and (cdr it) (or (not (cdr other)) (> (car it) (car other)))) async-queue)))))
        (let ((ans))
          (unwind-protect
              (setq ans
                    (with-local-quit
                      (with-demoted-errors "error in alan-startup-process-queue: %S"
                        (funcall func))))
            (if (eq ans 'immediate)
                (alan-startup-process-queue)
              (run-with-timer 0.001 nil
                              #'alan-startup-process-queue))))
      (setq process-queue-thread-exist nil))))



(defvar alan-dependency-alist nil)
(defvar alan-write-deps-schedualed nil)

(force-noerr
 (setq alan-dependency-alist
       (when (file-exists-p alan-dependency-cache-file)
         (with-temp-buffer
           (insert-file-contents alan-dependency-cache-file)
           (goto-char (point-min))
           (read (current-buffer))))))

(defun alan-write-dependency-alist ()
  (span :alan-write-dependency-alist
    (span-notef "writing deps to: %s" alan-dependency-cache-file)
    (with-local-quit
      (with-temp-file alan-dependency-cache-file
        (require 'pp)
        (let (pp-use-max-width print-circle print-level print-length)
          (pp alan-dependency-alist (current-buffer))))))
  (setq alan-write-deps-schedualed nil))

(defun alan-record-dep (feature deps)
  (let ((prev (alist-get feature alan-dependency-alist)))
    (unless (equal prev deps)
      (span-notef "updating deps of %S from %S to %S" feature (:seq prev) (:seq deps))
      (setf (alist-get feature alan-dependency-alist) deps)
      (unless alan-write-deps-schedualed
        (setq alan-write-deps-schedualed t)
        (alan-startup-schedual-fn -1000 #'alan-write-dependency-alist)))))

(setq alan-record-deps-func #'alan-record-dep)


(defun alan-require-one (feature)
  (unless (get feature 'alan-has-required)
    (let (f ignored)
      (while (and feature (not (memq feature ignored)))
        (setq f feature)
        (setq feature (--first (not (or (get it 'alan-has-required) (featurep it))) (alist-get f alan-dependency-alist)))
        (push f ignored))
      (setf (get f 'alan-has-required) t)
      (let ((alan-ignored-require ignored)
            (alan-ignored-require-for f))
        (require f nil t)))))

;; priority guideline to self
;; 100 = asap for queueing after command
;; 80 = load on first pre-command-hook
;; 50 = often used
;; 0 = standard
;; < 0 = wont do anything until timer
(defun startup-queue-package (package priority)
  (alan-startup-schedual-fn
   priority
   (lambda ()
     (alan-require-one package)
     (unless (featurep package)
       (startup-queue-package package priority)))))


;; (setq-default completion-at-point-functions nil)
(defvar alan-completion-at-point-hook nil)
(defun alan-completion-at-point ()
  (interactive)
  (run-hook-with-args-until-success 'alan-completion-at-point-hook))

(add-hook! 'alan-completion-at-point-hook :depth 90
  (completion-at-point))


;; allow hiding cursor in some windows
(defvar alan-enable-cursor t)
(defadvice! alan-internal-show-cursor-maybe-disable (args)
  :filter-args #'internal-show-cursor
  (cl-destructuring-bind (window show) args
    (when-let ((buf (window-buffer window)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (list window (and alan-enable-cursor show)))))))
(defun update-cursor-visibility ()
  (ignore-errors
    (internal-show-cursor nil alan-enable-cursor)))
;; so that we update it in both the previous and current window
(add-hook 'pre-command-hook #'update-cursor-visibility)
(add-hook 'post-command-hook #'update-cursor-visibility)


(add-to-list 'debug-ignored-errors 'minibuffer-quit)
(add-to-list 'debug-ignored-errors 'search-failed)




(make-lazy lsp-deferred-lazy 'lsp-mode 'lsp-deferred)

(defun alan--lsp-deferred (client &optional callback)
  (unless (local-variable-p 'header-line-format)
    (setq-local header-line-format ""))
  (startup-queue-package client 100)
  (let ((buf (current-buffer)))
    (alan-eval-after-load client
      (lambda ()
        (run-with-idle-timer
         0.05 nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (lsp-deferred-lazy)
               (when callback
                 (funcall callback))))))))))

(defmacro alan-lsp-deferred (client &rest body)
  (declare (indent 1))
  `(alan--lsp-deferred ,client (lambda () ,@body)))

(autoload 'lsp-completion-at-point "lsp-completion")
(autoload 'lsp-completion-mode "lsp-completion")



(make-lazy tree-sitter-hl-mode-lazy 'tree-sitter 'tree-sitter-hl-mode)

(autoload 'treesit-major-mode-setup "treesit")

(provide 'alan-core)
