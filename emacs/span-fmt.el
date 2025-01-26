;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl-print)

(defun span-fmt--immutable-or-replace (obj)
  (declare (pure t) (side-effect-free t))
  (let ((tp (type-of obj)))
    (pcase tp
      ('symbol obj)
      ('symbol-with-pos obj)
      ('integer obj)
      ('float obj)
      ('string (substring-no-properties obj))

      ('subr obj)
      (_ `[,tp]))))

(defsubst span--ensure-str (obj)
  (declare (pure t) (side-effect-free t))
  (if (stringp obj)
      obj
    (prin1-to-string obj)))

(defsubst span-fmt--immutable-or-replace-seq (obj)
  (cl-map (type-of obj) #'span-fmt--immutable-or-replace obj))

;; state is (count . (arglist . args))
(defun span-fmt--push (form state)
  (push form (cddr state))
  (let* ((n (cl-incf (car state)))
         (sym (intern (concat "span-fmt--arg" (int-to-string n)))))
    (push sym (cadr state))
    sym))

(defun span-fmt-parse (form)
  (let* ((state (cons 0 (cons nil nil)))
         (body (span-fmt--parse-form form state))
         (args (nreverse (cadr state)))
         (compute (nreverse (cddr state)))
         (n (car state)))
    (cond
     ((= n 0)
      (cons `(lambda (_span-fmt--obj) ,body) nil))

     ((= n 1)
      (cons `(lambda (,(car args)) ,body) (car compute)))

     (t
      (setq body
            (macroexp-let*
             (mapcar
              (lambda (x)
                (list
                 x
                 '(prog1
                      (car span-fmt--obj)
                    (setq span-fmt--obj (cdr span-fmt--obj)))))
              args)
             body))
      (cons `(lambda (span-fmt--obj) ,body) `(list ,@compute))))))

(defun span-fmt--parse-backquote (form state)
  (cond
   ((eq (car-safe form) '\,)
    (cl-assert (length= form 2))
    (span-fmt--parse-obj (cadr form) state))

   ((listp form)
    (mapcar (lambda (x) (span-fmt--parse-backquote x state)) form))

   (t
    form)))


(defun span-fmt--parse-obj (form state)
  (cond
   ((eq (car-safe form) :unsafe)
    (cl-assert (length= form 2))
    (span-fmt--push (cadr form) state))
   ((eq (car-safe form) :seq)
    (cl-assert (length= form 2))
    (let ((val (span-fmt--push `(copy-sequence ,(cadr form)) state)))
      `(span-fmt--immutable-or-replace-seq ,val)))

   ((eq (car-safe form) '\`)
    (cl-assert (length= form 2))
    (span-fmt--parse-backquote (cadr form) state))

   ;; ((vectorp form)
   ;;  (cl-assert (length= form 1))

   ;;  )

   ;; ((keywordp (car-safe form))
   ;;  (error "todo"))

   ((macroexp-const-p form)
    form)
   (t
    (let ((val (span-fmt--push form state)))
      `(span-fmt--immutable-or-replace ,val)))))

(defun span-fmt--parse-form (form state)
  (cond

   ((and (listp form) (length> form 1))
    (let ((args (mapcar (lambda (x) (span-fmt--parse-obj x state)) form)))
      `(format-message ,@args)))

   ((and (listp form) (length= form 1))
    `(span--ensure-str ,(span-fmt--parse-obj (car form) state)))

   (t (error "invalid form: %s" form))))



(provide 'span-fmt)
