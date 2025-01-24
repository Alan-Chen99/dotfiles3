;; -*- lexical-binding: t -*-

(require 'alan-utils)

(require 'general)
(require 'dash)


(defvar input-decode-map-original (copy-keymap input-decode-map))
;; local-function-key-map is terminal local, dont change it, use function-key-map
(defvar function-key-map-original (copy-keymap function-key-map))
(defvar key-translation-map-original (copy-keymap key-translation-map))

;; https://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
;; https://emacs.stackexchange.com/questions/220/how-to-bind-c-i-as-different-from-tab

;; (key-description [9]) -> "TAB"
;; (key-description [tab]) -> "<tab>"

;; gui defualt behavior:
;; [tab] -> input-decode-map -> [tab] -> function-key-map -> [9] -> key-translation-map -> [9]
;; [9] -> input-decode-map -> [9] -> function-key-map -> [9] -> key-translation-map -> [9]

;; we defer this translation to key-translation-map
(general-def function-key-map
  "<backspace>" nil
  "<tab>" nil
  "<linefeed>" nil
  "<return>" nil)
(general-def key-translation-map
  "<backspace>" "DEL"
  "<tab>" "TAB"
  "<linefeed>" "LFD"
  "<return>" "RET")

(defun alan-base-bindings-per-frame (frame)
  (with-selected-frame frame
    (general-def input-decode-map
      ";" "<.>"
      ":" "S-<.>"
      "C-;" "C-<.>"
      "C-:" "C-S-<.>")))

(alan-run-per-frame #'alan-base-bindings-per-frame)

(defvar translation-leader-map (make-sparse-keymap))
(general-def translation-leader-map
  ;; this will bind "; q" to type "@"
  ;; "q" "@"
  ;; this will bind "; w" to type "?"
  ;; "w" "?"
  ;; this will bind "; l" to escape
  ;; "l" "<escape>"
  ;; etc

  ;; since we used ";" as a prefix key, you should bind something to type ";"
  ;; here you would type ";" by pressing "/"
  ;; "/" ";"
  )

(general-def key-translation-map
  ;; you rebind keys here
  ;; this would make pressing "'" turn into a "DEL"
  ;; you will then need to make something else to "'"
  ;; "'" "DEL"
  ;; this will make control-h translate to the left arrow
  ;; "C-h" "<left>"

  "<.>" translation-leader-map)

(provide 'alan-base-bindings)
