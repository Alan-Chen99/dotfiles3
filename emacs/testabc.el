;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'span)

;; (require 'span)

(eval-when-compile
  (defun unsafe-bind (val)
    (error "unsafe-bind %s" val))
  (setf (get 'unsafe-bind 'byte-compile) #'unsafe-bind--byte-compile)
  (defun unsafe-bind--byte-compile (form)
    (byte-compile-push-binding-init `(placehold-1 ,(nth 1 form)))
    (byte-compile-out 'byte-unbind 0)
    nil))

(defvar some-var1)
(defvar some-var2)
(defvar some-var3)
(defvar some-var4)

;; (symbol-function 'testf1)
;; (symbol-plist 'let)

(defvar placehold-1)
(defun testf1 ()
  (let* ((some-var1 1))
    (unsafe-bind (atewwtg)))
  ;; (let* ((some-var1 1)
  ;;        (some-var2 2)
  ;;        (some-var3 3))
  ;;   (funcall x))
  )


(provide 'testabc)
