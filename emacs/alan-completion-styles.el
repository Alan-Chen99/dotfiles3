;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'orderless
  (startup-queue-package 'orderless 75))

(pkg! 'hotfuzz
  (startup-queue-package 'hotfuzz 76))

(pkg! '(hotfuzz-rs
        :host github :repo "Alan-Chen99/hotfuzz-rs" :protocol ssh
	    :pre-build
	    (let ((target-file (if (eq system-type 'windows-nt) "hotfuzz-rs-module.dll" "hotfuzz-rs-module.so"))
              (cargo-res (if (eq system-type 'windows-nt) "./target/release/hotfuzz_rs_module.dll" "./target/release/libhotfuzz_rs_module.so")))
	      (cl-assert (zerop (shell-command "cargo build --release")))
	      (delete-file target-file)
          (rename-file cargo-res target-file))
	    :files (:defaults "hotfuzz-rs-module.so" "hotfuzz-rs-module.dll"))
  (startup-queue-package 'hotfuzz-rs 76))



(defvar alan-completion-styles)
(setq alan-completion-styles
      '(
        orderless
        ;; basic
        hotfuzz
        basic
        ;; flex
        abgbadc
        ))

(defun alan-update-completion-styes ()
  (setq completion-styles (--filter (alist-get it completion-styles-alist) alan-completion-styles)))
(alan-update-completion-styes)


(setq completion-category-defaults nil)
(setq
 ;; goes before the defualt completion-styles
 completion-category-overrides
 '(
   (file (styles partial-completion))
   ;; (command (styles initials basic))
   ;; (command (styles basic))
   )
 )

(eval-after-load! orderless
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))

  ;; https://github.com/oantolin/orderless#company
  ;; (defun just-one-face (fn &rest args)
  ;;   (let ((orderless-match-faces [completions-common-part]))
  ;;     (apply fn args)))
  ;; (advice-add 'company-capf--candidates :around #'just-one-face)

  (defvar my-orderless-component-separator #'orderless-escapable-split-on-space)
  (defun my-orderless-split (string)
    (let
        ((ans
          (if (functionp my-orderless-component-separator)
              (funcall my-orderless-component-separator string)
            (split-string string my-orderless-component-separator t))))
      (if (> (length ans) 1)
          ans
        (throw 'orderless-quit nil))))
  (defun orderless-quit-advice (orig-fun &rest args)
    (catch 'orderless-quit (apply orig-fun args)))
  (advice-add #'orderless-all-completions :around 'orderless-quit-advice)
  (advice-add #'orderless-try-completion :around 'orderless-quit-advice)

  ;; (defadvice! orderless-all-completions-catch (orig-fun &rest args)
  ;;    :around 'orderless-all-completions
  ;;    (catch 'orderless-quit (apply orig-fun args)))
  ;; orderless-try-completion
  (setq orderless-component-separator 'my-orderless-split)

  (alan-update-completion-styes))

(eval-after-load! hotfuzz
  (eval-after-load! hotfuzz-rs
    (hotfuzz-rs-mode)
    ;; (hotfuzz-vertico-mode)
    (alan-update-completion-styes)))


;; (use-package hotfuzz
;;    :elpaca nil
;;    :autoload (hotfuzz--adjust-metadata hotfuzz-vertico-mode hotfuzz-all-completions)
;;    :functions (hotfuzz--filter-c-simple-string hotfuzz-filter-always-copy)
;;    :init
;;    ;; (setf (alist-get 'hotfuzz my-require-deps-alist-overrides) '(hotfuzz-module))

;;    (startup-queue-package 'hotfuzz 76)
;;    ;; (startup-queue-package 'hotfuzz-configs 75)

;;    :config
;;    ;; (require 'hotfuzz-configs)
;;    (hotfuzz-vertico-mode))


(provide 'alan-completion-styles)
