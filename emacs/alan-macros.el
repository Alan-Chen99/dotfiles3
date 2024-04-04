;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; from doom https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-lib.el
(eval-and-compile
  (defun doom-unquote (exp)
    "Return EXP unquoted."
    (declare (pure t) (side-effect-free t))
    (while (memq (car-safe exp) '(quote function))
      (setq exp (cadr exp)))
    exp))

(defmacro defbackport! (type symbol &rest body)
  "Backport a function/macro/alias from later versions of Emacs."
  (declare (indent defun) (doc-string 4))
  (unless (fboundp (doom-unquote symbol))
    `(,type ,symbol ,@body)))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.
    If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
    list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))
(defmacro add-hook-once! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
    FORMS are evaluated once, when that function/hook is first invoked, then never
    again.
    HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
    advised)."
  (declare (indent defun))
  (let (append-p depth local-p
                 (fn (gensym "doom-transient-hook")))
    (while (keywordp (car forms))
      (pcase (pop forms)
        (:append (setq append-p t))
        (:depth  (setq depth (pop forms)))
        (:local  (setq local-p t))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append-p :after :before) #',fn))
             ((symbolp sym)
              ;; TODO: why this?
              ;; (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,(or depth append-p) ,local-p))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.
    This macro accepts, in order:
    1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
        unquoted list of modes, a quoted hook variable or a quoted list of hook
        variables.
    2. Optional properties :local, :append, and/or :depth [N], which will make the
        hook buffer-local or append to the list of hooks (respectively),
    3. The function(s) to be added: this can be a quoted function, a quoted list
        thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
        implicitly be wrapped in a lambda).
    \(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent defun) (debug t))
  ;; (declare (indent (lambda (indent-point state)
  ;;                    (goto-char indent-point)
  ;;                    (when (looking-at-p "\\s-*(")
  ;;                      (lisp-indent-defform state indent-point))))
  ;;          (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'quote (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))
(defmacro remove-hook! (hooks &rest rest)
  "A convenience macro for removing N functions from M hooks.
    Takes the same arguments as `add-hook!'.
    If N and M = 1, there's no benefit to using this macro over `remove-hook'.
    \(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent defun) (debug t))
  `(add-hook! ,hooks :remove ,@rest))

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.
    ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
    PLACE is the function to which to add the advice, like in `advice-add'.
    DOCSTRING and BODY are as in `defun'.
    \(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) ',symbol))))))
(defmacro undefadvice! (symbol _arglist &optional docstring &rest body)
  "Undefine an advice called SYMBOL.
    This has the same signature as `defadvice!' an exists as an easy undefiner when
    testing advice (when combined with `rotate-text').
    \(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))


(provide 'alan-macros)
