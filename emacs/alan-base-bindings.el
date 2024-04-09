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
    ;; disable default C-x bindings, else C-x dont type capital X
    (general-def local-function-key-map
      "C-x" nil)

    (general-def input-decode-map
      ";" "<.>"
      ":" "S-<.>"
      "C-;" "C-<.>"
      "C-:" "C-S-<.>"
      ;; (kbd "M-;") [134217787]
      ;; (read-kbd-macro "M-;") [134217787]
      ;; (key-description [134217787])
      ;; (keymap-lookup local-function-key-map "M-;")
      ;; (keymap-lookup function-key-map "M-;")


      ;; otherwise 0 dont work in terminal
      ;; TODO: why?
      "M-;" "M-;"
      )))

(alan-run-per-frame #'alan-base-bindings-per-frame)


;; https://stackoverflow.com/questions/63306647/various-forms-of-looping-and-iteration-in-elisp
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Conversion.html
(defconst lowercase-letters "abcdefghijklmnopqrstuvwxyz")

(mapc
 (lambda (it)
   (let ((p (char-to-string it)))
     (general-define-key
      :keymaps 'key-translation-map
      (concat "C-" p) (upcase p)
      ;; for read-key, so that c-s-g reads c-g
      (concat "C-S-" p) (concat "C-" p))))
 lowercase-letters)


;; using shift translation
(general-def key-translation-map
  "1" "S-1"
  "2" "S-2"
  "3" "S-3"
  "4" "S-4"
  "5" "S-5"
  "6" "S-6"
  "7" "S-7"
  "8" "S-8"
  "9" "S-9"
  "0" "S-0"

  "M-a" "S-1"
  "M-s" "S-2"
  "M-d" "S-3"
  "M-f" "S-4"
  "M-g" "S-5"
  "M-h" "S-6"
  "M-j" "S-7"
  "M-k" "S-8"
  "M-l" "S-9"
  "M-;" "S-0")

(defvar translation-leader-map (make-sparse-keymap))
(general-def translation-leader-map
  "q" "@"
  "w" "?"
  "e" "%"
  "r" "`"

  "a" "#"
  "s" ":"
  "d" "\""
  "f" "'"

  "z" "~"
  "x" "^"
  "c" "!"
  "v" "$"

  "l" "<escape>"
  "k" "RET"

  "'" "DEL"
  "\"" "DEL"
  "C-'" "DEL"
  "C-\"" "DEL"
  "M-'" "DEL"
  "M-\"" "DEL")

(general-def key-translation-map
  "Q" "<q>"
  "W" "<w>"
  "E" "{"
  "R" "}"
  "T" "<T>"
  "Y" "<y>"
  "U" "("
  "I" ")"
  "O" "+"
  "P" "-"

  "[" "_"
  "{" "/"

  "A" "<end>"
  "S" "1"
  "D" "0"
  "F" "="
  "G" "&"
  "H" "<left>"
  "J" "<down>"
  "K" "<up>"
  "L" "<right>"

  "'" "DEL"
  "\"" "S-DEL"
  "C-'" "DEL"
  "C-\"" "S-DEL"
  "M-'" "DEL"
  "M-\"" "DEL"

  "Z" "<z>"
  "X" "<x>"
  "C" "2"
  "V" "\\"
  "B" "|"
  "N" "["
  "M" "]"

  "/" ";"
  "?" "*"

  "=" "S-="

  "C-," "C-h"
  "C-/" "C-h"
  "<f1>" "C-h"

  "<.>" translation-leader-map)

(provide 'alan-base-bindings)
