;; -*- lexical-binding: t -*-

(require 'alan-core)

(pkg! 'dockerfile-mode)

(eval-after-load! dockerfile-mode
  (add-hook! 'dockerfile-mode-hook
    (defun alan-setup-dockerfile ()
      (setq-local format-all-formatters '(("Dockerfile" dockerfile-native))))))

(eval-after-load! format-all
  (define-format-all-formatter dockerfile-native
    (:executable)
    (:install)
    (:languages "Dockerfile")
    (:features region)
    (:format
     (format-all--buffer-native
      'dockerfile-mode
      (if region
          (lambda () (indent-region (car region) (cdr region)))
        (lambda () (indent-region (point-min) (point-max))))))))

(provide 'alan-docker)
