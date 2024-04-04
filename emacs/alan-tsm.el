;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'ts-movement)
(require 'hydra)

(defun alan-tsm (cb point)
  (interactive "d")
  (let* ((overlay (tsm/-overlay-at-point point))
         (node (overlay-get overlay 'node))
         (new (funcall cb node)))
    (when new
      (overlay-put overlay 'node new)
      (move-overlay overlay (treesit-node-start new) (treesit-node-end new))
      (goto-char (treesit-node-start new)))))

(defmacro alan-with-tsm (sym &rest rest)
  (declare (indent defun))
  (let ((int-sym (intern (concat "tsm/" (symbol-name sym)))))
    `(progn
       (defun ,sym ,@rest)

       (defun ,int-sym (point)
         (interactive "d")
         (alan-tsm #',sym point)))))

(alan-with-tsm alan-ts-prev-nostop (node)
  (let* ((ans (treesit-node-prev-sibling node)))
    (while (and node (not ans))
      (setq node (treesit-node-parent node))
      (setq ans (treesit-node-prev-sibling node)))
    ans))

(alan-with-tsm alan-ts-next-nostop (node)
  (let* ((ans (treesit-node-next-sibling node)))
    (while (and node (not ans))
      (setq node (treesit-node-parent node))
      (setq ans (treesit-node-next-sibling node)))
    ans))


(defhydra tsm/hydra (:body-pre (lambda () (tsm/-overlay-at-point (point))) :post tsm/clear-overlays)
  "TS Movement"

  ("<up>" #'tsm/alan-ts-prev-nostop)
  ("<down>" #'tsm/alan-ts-next-nostop)

  ("k" #'tsm/node-prev)
  ("j" #'tsm/node-next)
  ("h" #'tsm/node-parent)
  ("l" #'tsm/node-child)

  ("d" #'tsm/delete-overlay-at-point)
  ("<escape>" #'ignore :exit t)

  ;; ("D" #'tsm/clear-overlays-of-type)
  ;; ("C-b" #'tsm/backward-overlay)
  ;; ("C-f" #'tsm/forward-overlay)
  ;; ("b" #'tsm/node-prev)
  ;; ("f" #'tsm/node-next)
  ;; ("n" #'tsm/node-child)
  ;; ("N" #'tsm/node-children)
  ;; ("s" #'tsm/node-children-of-type)
  ;; ("a" #'tsm/node-start)
  ;; ("e" #'tsm/node-end)
  ;; ("m" #'tsm/node-mark)
  ;; ("c" #'tsm/mc/mark-all-overlays)
  )

(general-def
  :states 'motion
  "b" #'tsm/hydra/body)

(provide 'alan-tsm)
