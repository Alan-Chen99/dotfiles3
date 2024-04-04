;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! '(vertico :files (:defaults "extensions/*"))
  (startup-queue-package 'vertico 80))

(eval-after-load! vertico
  (span-wrap vertico--exhibit (&rest args)
    (_)
    (span-dbg
     inhibit-quit
     throw-on-input))

  (defadvice! span--wrap-vertico--update (fn &optional interruptible)
    :around #'vertico--update
    (span :vertico--update
      ;; vertico will force non-interruptible for tramp
      (if interruptible
          (while-no-input
            (funcall fn interruptible))
        (funcall fn))))

  ;; (span-wrap vertico--update (&rest args)
  ;;   (_ (:seq args))
  ;;   (span-dbgf
  ;;    inhibit-quit
  ;;    throw-on-input))

  (defadvice! span--wrap-vertico--affixate (fn cands)
    :around #'vertico--affixate
    (span :vertico--affixate
      (span-dbg
       inhibit-quit
       throw-on-input
       (vertico--metadata-get 'affixation-function))
      (let ((inhibit-quit nil)
            (ans nil)
            (suc nil))
        (catch 'input
          (let
              ((throw-on-input 'input))
            (setq ans (funcall fn cands))
            (setq suc t)))
        (if suc
            ans
          (cl-loop for cand in cands collect (list cand "" ""))))))

  ;; (defadvice! force-debug (func &rest args)
  ;;   :around #'vertico--exhibit
  ;;   (condition-case e
  ;;       (apply func args)
  ;;     ((debug error) (signal (car e) (cdr e)))))


  (general-def vertico-map
    [remap minibuffer-complete-and-exit] #'vertico-exit
    [remap minibuffer-complete] #'vertico-insert

    [remap minibuffer-previous-completion] #'vertico-previous
    [remap minibuffer-next-completion] #'vertico-next)

  (defadvice! vertico--setup-compose-map (orig-fun &rest args)
    :around #'vertico--setup
    (let ((vertico-map (make-composed-keymap (current-local-map) vertico-map)))
      (apply orig-fun args))
    (overlay-put vertico--candidates-ov 'priority 1)
    (overlay-put vertico--count-ov 'priority 50))



  ;; (vertico-buffer-mode)
  ;; (setq-default
  ;;    vertico-buffer-display-action
  ;;    '(display-buffer-at-bottom
  ;;        (window-height . 10)))
  ;; (vertico-reverse-mode)

  (setq
   vertico-count 10
   vertico-resize nil)
  (add-hook! 'resize-minibuffer-hook
    (setq vertico-count (max (1- (window-height (minibuffer-window) 'floor)) 10)))

  (vertico-mode))

(eval-after-load! crm

  ;; one ex of crm is customize-face


  ;; ;; Add prompt indicator to `completing-read-multiple'.
  ;; ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  ;; (defun crm-indicator (args)
  ;;   (cons (format "[CRM%s] %s"
  ;;                 (replace-regexp-in-string
  ;;                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
  ;;                  crm-separator)
  ;;                 (car args))
  ;;         (cdr args)))
  ;; (advice-add #'completing-read-multiple :filter-args 'crm-indicator)
  )

(provide 'alan-vertico)
