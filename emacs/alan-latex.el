;; -*- lexical-binding: t -*-

(require 'alan-core)
(require 'alan-evil)
(require 'alan-lsp)

(require-if-is-bytecompile outline)
(pkg! '(auctex :repo "https://github.com/emacsmirror/auctex.git" :branch "master"))

(add-to-list 'auto-mode-alist `(,(rx ".xtx" eos) . LaTeX-mode))

(eval-after-load! format-all
  (define-format-all-formatter latexindent
    (:executable "latexindent.pl")
    (:install)
    (:languages "LaTeX")
    (:features)
    (:format (format-all--buffer-easy executable "-l")))

  (define-format-all-formatter prettier-latex
    (:executable "prettier")
    (:install)
    (:languages "LaTeX")
    (:features)
    (:format (format-all--buffer-easy executable "--plugin=prettier-plugin-latex" "--parser=latex-parser"))))

(evil-define-motion outline-backward-same-level-or-paragraph (count)
  :jump t
  :type exclusive
  (let
      (
       (count (or count 1))
       (prevpoint (point)))
    (if (outline-on-heading-p)
        (progn
          (outline-backward-same-level count)
          (when (not (outline-on-heading-p))
            (goto-char prevpoint)))
      (evil-backward-paragraph count))))

;; (alan-set-ignore-debug-on-error #'outline-forward-same-level)
;; (alan-set-ignore-debug-on-error #'outline-backward-same-level)

(evil-define-motion outline-forward-same-level-or-paragraph (count)
  :jump t
  :type exclusive
  (let
      (
       (count (or count 1))
       (prevpoint (point)))
    (if (outline-on-heading-p)
        (progn
          (outline-forward-same-level count)
          (when (not (outline-on-heading-p))
            (goto-char prevpoint)))
      (evil-forward-paragraph count))))

(add-hook! 'TeX-mode-hook
  (defun alan-setup-tex ()

    ;; (setq-local format-all-formatters '(("LaTeX" prettier-latex)))
    (setq-local format-all-formatters '(("LaTeX" latexindent)))
    (setq-local TeX-command-extra-options "-shell-escape")

    (alan-lsp-deferred 'lsp-tex)))

(eval-after-load! latex
  (clear-and-backup-keymap LaTeX-mode-map)
  (clear-and-backup-keymap TeX-mode-map)

  (general-def LaTeX-mode-map
    :states 'motion
    "SPC a" #'TeX-command-run-all
    "SPC c" #'TeX-command-master
    "SPC v" #'TeX-view
    "SPC j" #'TeX-next-error
    "SPC k" #'TeX-previous-error
    "g f" #'TeX-error-overview

    "<down>" #'outline-forward-same-level-or-paragraph
    "<up>" #'outline-backward-same-level-or-paragraph)

  (setq TeX-debug-warnings t)
  ;; if nil, and first error is bad box, then previous error on first error crashes
  (setq TeX-debug-bad-boxes t)
  ;; (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-view-program-selection '((output-pdf "Sumatra PDF")))

  ;; sumatrapdf inverse search command:
  ;; C:\Windows\System32\wsl.exe /home/alan/emacs29/bin/emacsclient -n +%l "$(wslpath '%f')"
  ;; "\\wsl.localhost\ubuntu\nix\store\g9273bqby6yb7jy90bskc43068qyldl1-emacs-pgtk-30.1\bin\emacsclient" -n +%l "%f"
  (setq TeX-view-program-list
        '(("Sumatra PDF"
           ("/mnt/c/SumatraPDF/SumatraPDF.exe -reuse-instance"
            (mode-io-correlate " -forward-search %b %n ")
            ;; (mode-io-correlate " -forward-search \"%b\" %n ")
            " %o"))))


  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; (alan-set-ignore-debug-on-error #'TeX-error-overview)
  )

(eval-after-load! lsp-tex
  (let ((clients (lsp--filter-clients (lambda (client)
                                        (-contains? (lsp--client-major-modes client) 'latex-mode)))))
    (dolist (client clients)
      (eval `(push 'LaTeX-mode (lsp--client-major-modes (quote ,client)))))))

(provide 'alan-latex)
