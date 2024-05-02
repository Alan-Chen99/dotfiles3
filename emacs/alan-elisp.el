;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-flycheck)
(require 'evil)

;; (setq print-circle t)
(setq print-circle nil)
;; (setq print-level 10)
(setq print-level nil)
(setq print-length nil)

;; (setq eval-expression-print-level nil)

(autoload 'pp-last-sexp "pp")

(defun pp-macroexpand-all-last-sexp ()
  (interactive)
  (let ((print-circle nil))
    (pp-display-expression (macroexpand-all (pp-last-sexp)) "*Pp Macroexpand Output*")))

(defun copy-eval-last-sexp ()
  (interactive)
  (let ((ans (call-interactively 'eval-last-sexp)))
    (kill-new
     (if (stringp ans)
         ans
       (prin1-to-string ans)))))

(defun copy-pp-eval-expression (expression)
  (interactive
   (list (read--expression "Eval: ")))
  (message "Evaluating...")
  (let ((result (pp-to-string (eval expression lexical-binding))))
    (message "%s" result)
    (kill-new result)))


(general-def lisp-mode-shared-map
  :states 'motion
  "SPC e" #'pp-eval-last-sexp
  "SPC SPC" #'eval-buffer
  "SPC c" #'copy-eval-last-sexp
  "SPC o" #'pp-macroexpand-last-sexp ;; Only outmost layer
  "SPC a" #'pp-macroexpand-all-last-sexp ;; All
  )

;; TODO: do i still do this?
;; dont seems to do anything other than leaking mem
(advice-add #'values--store-value :override #'ignore)

;; (defadvice! pp-display-expression-set-prints (func &rest args)
;;   :around 'pp-display-expression
;;   (let ((print-circle t)
;;         (print-length 10))
;;     (apply func args)))

(setq
 eval-expression-print-length nil
 eval-expression-print-level nil)

(defun elisp-set-syntax (syntax-table)
  (seq-doseq (p "_+-*/:=.!<>")
    (modify-syntax-entry p "w" syntax-table)))

(elisp-set-syntax lisp-mode-syntax-table)
(elisp-set-syntax lisp-data-mode-syntax-table)
(elisp-set-syntax emacs-lisp-mode-syntax-table)
(elisp-set-syntax messages-buffer-mode-syntax-table)

;; (defadvice! elisp--xref-infer-namespace-advice (namespace)
;;   :filter-return #'elisp--xref-infer-namespace
;;   namespace)


(eval-after-load! company
  ;; so that company quickhelp dont overwrite the *help* buffer
  ;; FIXME: this can do with less buffer
  (defadvice! elisp--company-doc-buffer-seperate-buffer (orig-fun &rest args)
    :around #'elisp--company-doc-buffer
    (with-temp-buffer
      (cl-letf (((symbol-function 'help-buffer) (lambda () (current-buffer))))
        (apply orig-fun args)
        (company-doc-buffer (buffer-string))))))

(add-hook! 'emacs-lisp-mode-hook
  (defun alan-emacs-lisp-setup ()
    (setq-local my-orderless-component-separator (rx "*"))
    ;; (setq-local completion-at-point-functions '(elisp-completion-at-point codeium-completion-at-point))
    (setq-local completion-at-point-functions (list #'elisp-completion-at-point))
    ;; (highlight-indent-guides-deferred)
    ;; (rainbow-delimiters-enable-lazy)
    ;; (my-flycheck-deferred)
    (alan-flycheck-deferred)
    (rainbow-mode-lazy)
    ;; (dash-fontify-mode)
    ))

(add-hook! 'messages-buffer-mode-hook
  (setq-local xref-backend-functions '(elisp--xref-backend)))
(let ((buf (get-buffer messages-buffer-name)))
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (run-hooks 'messages-buffer-mode-hook))))

(eval-after-load! pp
  (setq pp-use-max-width nil))


(eval-after-load! edebug
  (clear-and-backup-keymap edebug-mode-map)
  (general-def edebug-mode-map
    :states 'motion
    ;; "SPC" edebug-mode-map-origional
    [remap eval-buffer] #'edebug-safe-eval
    "i" #'edebug-step-in
    "o" #'edebug-step-out
    "d" #'edebug-step-mode
    "a" #'edebug-goto-here
    [remap pp-eval-expression] #'edebug-eval-expression
    [remap pp-eval-last-sexp] #'edebug-eval-last-sexp)
  (add-hook 'edebug-mode-hook #'evil-normalize-keymaps))

(provide 'alan-elisp)
