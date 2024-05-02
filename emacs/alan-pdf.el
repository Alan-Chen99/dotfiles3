;; -*- lexical-binding: t -*-

(require 'alan-core)

;; (autoload 'pdf-view-mode "pdf-tools")
;; (pkg! 'pdf-tools
;;   (add-to-list 'auto-mode-alist (cons "\\.pdf\\'" #'pdf-view-mode)))

(pkg! '(image-roll
        :host github
        :repo "dalanicolai/image-roll.el"))
(pkg! '(pdf-tools
        :host github
        :repo "dalanicolai/pdf-tools"
        :branch "pdf-roll"
        :alan-extra-deps ((image-roll "0"))
        :files ("lisp/*.el"
                "README"
                ("build" "Makefile")
                ("build" "server")
                (:exclude "lisp/tablist.el" "lisp/tablist-filter.el")))
  (add-to-list 'auto-mode-alist (cons "\\.pdf\\'" #'pdf-view-mode)))

(autoload 'pdf-view-mode "pdf-tools")
(autoload 'pdf-history-before-change-page-hook "pdf-history")

(eval-when-compile
  (defvar revert-buffer-preserve-modes))

(eval-after-load! pdf-tools

  (general-def pdf-view-mode-map
    [remap next-line] (lambda () (interactive) (pdf-view-next-line-or-next-page 4))
    [remap previous-line] (lambda () (interactive) (pdf-view-previous-line-or-previous-page 4))

    [remap forward-paragraph] #'pdf-view-next-page
    [remap backward-paragraph] #'pdf-view-previous-page
    )

  (general-def pdf-view-mode-map
    :states 'motion
    "o" #'pdf-view-enlarge
    "p" #'pdf-view-shrink
    "d" #'pdf-view-fit-width-to-window
    "r" #'pdf-view-rotate
    )

  (defun alan-pdf-view-revert-buffer (&rest _)
    ;; (fundamental-mode)
    ;; (pdf-view-mode)
    (let ((revert-buffer-preserve-modes nil))
      (pdf-info-close)
      (revert-buffer--default t t)
      (pdf-view-mode)
      ;; (pdf-info-open)
      ))

  ;; (defadvice! pdf-view-revert-buffer-adv (&rest _)
  ;;   :after #'pdf-view-revert-buffer
  ;;   (pdf-view-roll-minor-mode -1)
  ;;   (pdf-view-roll-minor-mode +1)
  ;;   )

  (add-hook! 'pdf-view-mode-hook
    (defun alan-pdf-view-mode-setup ()
      (setq-local revert-buffer-function 'alan-pdf-view-revert-buffer)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local alan-enable-cursor nil)
      (pdf-view-roll-minor-mode)
      (setq-local mwheel-scroll-up-function #'pdf-view-next-line-or-next-page)
      (setq-local mwheel-scroll-down-function #'pdf-view-previous-line-or-previous-page)))

  (alan-set-ignore-debug-on-error #'pdf-view-goto-page)
  ;; (alan-set-ignore-debug-on-error #'image-roll-goto-page)
  ;; pdf-view-scroll-down-or-previous-page

  (span-wrap pdf-info-query (&rest args)
    (:pdf-info-query (:seq args))
    ;; (error "no")
    ;; (span-flush)
    )


  )


(provide 'alan-pdf)
