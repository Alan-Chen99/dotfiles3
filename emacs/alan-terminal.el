;; -*- lexical-binding: t -*-

(require 'alan-core)


(pkg! 'evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (startup-queue-package 'evil-terminal-cursor-changer 100)))

(pkg! 'clipetty
  (unless (display-graphic-p)
    (startup-queue-package 'clipetty 70)))


(eval-after-load! evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    ;; (setq evil-motion-state-cursor 'box)  ; █
    ;; (setq evil-visual-state-cursor 'box)  ; █
    ;; (setq evil-normal-state-cursor 'box)  ; █
    ;; (setq evil-insert-state-cursor 'bar)  ; ⎸
    ;; (setq evil-emacs-state-cursor  'hbar) ; _

    ;; TODO: find out why (getenv "TERM") returns "dumb"
    (setq etcc-term-type-override 'xterm)

    ;; TODO: maybe i should do this after each evil cursor change?
    (add-hook! 'after-load-theme-hook
      (etcc--evil-set-cursor-color (face-attribute 'cursor :background)))

    ;; TODO: to reset cursor after exit emacs,
    ;; maybe i should add a cursor setting to prompt as someone said here
    ;; https://github.com/7696122/evil-terminal-cursor-changer/issues/12
    ))

(eval-after-load! clipetty
  (unless (display-graphic-p)
    (global-clipetty-mode)))


(provide 'alan-terminal)
