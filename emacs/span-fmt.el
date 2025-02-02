;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'cl-print)
(eval-when-compile
  (require 'backtrace))
(autoload 'backtrace-print-to-string "backtrace")

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

(defun span-fmt-to-string (obj)
  (backtrace-print-to-string obj 100))

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
    (span-fmt--push (nth 1 form) state))
   ((eq (car-safe form) :seq)
    (cl-assert (length= form 2))
    (let ((val (span-fmt--push `(copy-sequence ,(nth 1 form)) state)))
      `(span-fmt--immutable-or-replace-seq ,val)))
   ((eq (car-safe form) :ts)
    (cl-assert (length= form 2))
    (span-fmt--push `(span-fmt-to-string ,(nth 1 form)) state))
   ((eq (car-safe form) :unsafe-ts)
    (cl-assert (length= form 2))
    (let ((val (span-fmt--push (nth 1 form) state)))
      `(span-fmt-to-string ,val)))

   ((eq (car-safe form) '\`)
    (cl-assert (length= form 2))
    (span-fmt--parse-backquote (nth 1 form) state))

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



(defun span-fmt--record-to-list (r)
  (mapcar (lambda (x) (aref r x))
          (number-sequence 0 (1- (length r)))))

(advice-add #'cl-print-object :around #'span-fmt--cl-print-object-show-record)
(defun span-fmt--cl-print-object-show-record (orig-fn object stream)
  (funcall orig-fn object stream)
  (when (recordp object)
    (princ "[#s" stream)
    (cl-print-insert-ellipsis (span-fmt--record-to-list object) nil stream)
    (princ "]" stream)))


(provide 'span-fmt)
