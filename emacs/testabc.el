;; -*- lexical-binding: t -*-

(require 'cl-lib)
;; (require 'span)

(eval-and-compile

  (defun span--special-maybe-bind (cd var value cb)
    (ignore cd var value cb)
    (error "span--special-maybe-bind should never be called directly"))

  (defun span--compile-special-maybe-bind (form)
    (cl-destructuring-bind (_ cd-form var value-form body-form) form
      (cl-assert (symbolp var))

      (let ((depth byte-compile-depth))
        ;; stores the result
        (byte-compile-push-constant nil)

        (byte-compile-form cd-form)

        (cl-assert (= (+ depth 2) byte-compile-depth))

        (let ((body-start-tag (byte-compile-make-tag)))
          (byte-compile-goto-if nil nil body-start-tag)
          (byte-compile-form value-form)
          (byte-compile-dynamic-variable-bind var)
          (byte-compile-push-constant t)
          (byte-compile-out-tag body-start-tag))

        (cl-assert (= (+ depth 2) byte-compile-depth))

        (byte-compile-form body-form)
        (byte-compile-stack-set depth)

        (cl-assert (= (+ depth 2) byte-compile-depth))

        (let ((done-tag (byte-compile-make-tag)))
          (byte-compile-goto-if nil 'discard done-tag)
          (byte-compile-out 'byte-unbind 1)
          (byte-compile-out-tag done-tag))

        (cl-assert (= (+ depth 1) byte-compile-depth))

        nil)))

  (setf (get #'span--special-maybe-bind 'byte-compile) #'span--compile-special-maybe-bind))

(defvar test-glob-var 0)

(defun testf1 (v)
  (span--special-maybe-bind
   v test-glob-var 1
   (progn
     (debug)
     (message "test-glob-var: %s" test-glob-var)
     (cons 'af test-glob-var))))

;; (defun testf2 (fn x)
;;   (funcall fn x x))

;; (debug)


(provide 'testabc)
