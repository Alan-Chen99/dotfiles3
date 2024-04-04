;; -*- lexical-binding: t -*-

(require 'cl-lib)

(require 'span)

(defvar testabc-x (cons (cons 0 0) 0))
;; (defvar testabc-x nil)

(defvar span--tmp-sucess2 nil)

(defun testx2 ()
  (let ((inhibit-quit t))
    (while t
      (span--unchecked :test
        (cl-incf (caar testabc-x))))))

(defun testx ()
  (let ((inhibit-quit t))
    (while t
      (unwind-protect
          (cl-incf x)
        (cl-incf x)))))


(defun testchecked ()
  (span :test
    (cl-incf (caar testabc-x))))

(defun testchecked2 ()
  (let* ((tmp inhibit-quit)
         (inhibit-quit t))
    (span--unchecked :test
      (let ((inhibit-quit tmp))
        (cl-incf (caar testabc-x))))))

(defun testunchecked ()
  (span--unchecked :test
    (cl-incf (caar testabc-x))))

;; (benchmark-run-compiled 1000000
;;   ;; (cl-incf (caar testabc-x))
;;   (testunchecked))

;; (benchmark-run-compiled 1000000
;;   (let
;;       ((span--stack
;;         (cons
;;          (cons '#s(span-s :test
;;                           (closure
;;                               (t)
;;                               (_span-fmt--obj)
;;                             (span--ensure-str ""))
;;                           nil nil)
;;                (span--time))
;;          span--stack))
;;        (span--tmp-sucess2 nil)
;;        (inhibit-quit t))
;;     (unwind-protect
;;         (prog1
;;             (let*
;;                 ((v testabc-x))
;;               (setcar
;;                (car v)
;;                (+
;;                 (car
;;                  (car v))
;;                 1)))
;;           (setq span--tmp-sucess2 t))
;;       (if span--tmp-sucess2 nil
;;         (span--unsafe-on-err t)))))

;; (benchmark-run-compiled 1000000
;;   (span :hello
;;     2))

;; (disassemble (native-compile
;;               '(lambda nil
;;                  (span :hello 2))))

;; (funcall (native-compile
;;               '(lambda nil
;;                  (span :hello 2))))

(defun testfn3 (x)
  (span-notef "hello3: %s" x x))

(defun testfn ()
  (let ((x 1))
    (unwind-protect
        (setq x nil)
      (when nil
        (ignore x)
        (testfn3)))))

(defun testfn2 (x)
  (lambda (x)
    (+ x 5)))
;; (sleep-for 12)


;; (with-temp-file "~/.emacs.d/elpaca_new/tmp-deps.el"
;;   (pp alan-dependency-alist-new (current-buffer)))


;; (with-temp-buffer
;;   (insert-file-contents "~/.emacs.d/elpaca_new/tmp-deps.el")
;;   (goto-char (point-min))
;;   (read (current-buffer)))


;; (force-noerr
;;  1)

;; (read "~/.emacs.d/elpaca_new/tmp-deps.el")

;; (write-file

;; (use-package evil
;;   :config
;;   (evil-ret))

;; (eval-when-compile (require 'evil))

;; (wrap-maybe-toplevel
;;  (eval-after-load! evil
;;    (evil-ret)
;;    (at-top-level
;;     (hello-world))))


;; (run-with-timer 1 nil (lambda () (sleep-for 10)))
;; (run-with-timer 1 nil (lambda () (debug)))


;; (yes-or-no-p "hello")
;; (add-hook! 'pre-command-hook
;;   (debug))

;; (setq-default mode-line-format
;;               '("%e" mode-line-front-space
;;                 (:propertize
;;                  ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
;;                  display
;;                  (min-width
;;                   (5.0)))
;;                 mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
;;                 (vc-mode vc-mode)
;;                 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(provide 'testabc)
