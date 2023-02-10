;;; sketch-themes.el --- Sketch color themes -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daw-Ran Liou

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/sketch-themes/
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; A collection of (almost) grayscale Emacs themes.  A lot of the code and
;; concepts were inspired by [@cryon](https://github.com/cryon)'s [Almost Mono
;; Themes](https://github.com/cryon/almost-mono-themes).  Mostly what I did is
;; tweaking the color slightly to fit my tastes.  Huge thanks to him!

;;; Code:

(defconst sketch-themes-colors
  '((white . ((fg        . "#212121")
              (bg        . "#FAFAFA")
              (bg-alt    . "#efefef")
              (weak      . "#888888")
              (weaker    . "#dddddd")
              (weakest   . "#efefef")
              (highlight . "#fee761")
              (success   . "#63c74d")
              (warning   . "#e43b44")))

    (black . ((fg        . "#f0f6f0")
              (bg        . "#222323")
              (bg-alt    . "#2F302F")
              (weak      . "#6E706E")
              (weaker    . "#555755")
              (weakest   . "#2F302F")
              (highlight . "#7D5DC1")
              ;; (highlight . "#CC7F22")   ; An alternative highlighting color
              (success   . "#63c74d")
              (warning   . "#e43b44")))))

(defmacro sketch-themes--variant-with-colors (variant &rest body)
  "Execute BODY in a scope where the different colors for given
VARIANT is bound."
  `(let* ((colors (or (cdr (assoc ,variant sketch-themes-colors))
                      (error "No such theme variant")))
          (bg         (cdr (assoc 'bg colors)))
          (fg         (cdr (assoc 'fg colors)))
          (weak	      (cdr (assoc 'weak colors)))
          (weaker     (cdr (assoc 'weaker colors)))
          (weakest    (cdr (assoc 'weakest colors)))
          (bg-alt     (cdr (assoc 'bg-alt colors)))
          (highlight  (cdr (assoc 'highlight colors)))
          (warning    (cdr (assoc 'warning colors)))
          (success    (cdr (assoc 'success colors)))
          (string     (cdr (assoc 'string colors))))
     ,@body))

(defmacro sketch-themes--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(
      ;; default
      (default (:background ,bg :foreground ,fg))
      (fringe (:background ,bg :foreground ,weak))
      (shadow (:background ,weakest))
      (highlight (:foreground ,fg :background ,highlight))
      (region (:foreground ,fg :background ,highlight))
      (show-paren-match (:background ,highlight :bold t))
      (show-paren-mismatch (:background ,warning :bold t))
      (minibuffer-prompt (:bold t :foreground ,fg))
      (isearch (:bold t :foreground ,fg :background ,highlight :bold t))
      (lazy-highlight (:foreground ,fg :background ,weaker))
      (link (:underline t))
      (parenthesis (:foreground ,weak))
      (trailing-whitespace (:foreground nil :background ,warning))
      (cursor (:background ,fg :foreground ,bg))
      (vertical-border (:foreground ,fg))
      (default-italic (:italic t))
      (line-number (:background ,bg :foreground ,weak))
      (line-number-current-line (:background ,bg :foreground ,fg))

      ;; mode line
      (mode-line (:foreground ,fg :background ,weakest))
      (mode-line-inactive (:foreground ,weaker :background ,weakest))

      ;; font lock
      (font-lock-builtin-face (:foreground ,fg))
      (font-lock-comment-face (:inherit font-lock-string-face))
      (font-lock-negation-char-face (:foreground ,fg))
      (font-lock-reference-face (:foreground ,fg))
      (font-lock-constant-face (:bold t))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (font-lock-function-name-face (:foreground ,fg :bold t))
      (font-lock-keyword-face (:foreground ,fg))
      (font-lock-string-face (:foreground ,weak))
      (font-lock-type-face (:foreground ,fg))
      (font-lock-variable-name-face (:foreground ,fg :bold t))
      (font-lock-warning-face (:underline (:color ,warning :style wave)))
      (fill-column-indicator (:foreground ,weak))

      ;; clojure mode
      (clojure-keyword-face (:foreground ,fg))

      ;; hl line
      (hl-line (:background ,weaker))

      ;; hl fill column
      (hl-fill-column-face (:background ,bg-alt))))))


(defun sketch-themes--variant-name (variant)
  "Create symbol for color theme variant VARIANT."
  (intern (format "sketch-%s" (symbol-name variant))))

(defmacro sketch-themes--define-theme (variant)
  "Define a theme for the sketch variant VARIANT."
  (let ((name (sketch-themes--variant-name variant))
        (doc (format "Sketch Theme (%s version)" variant)))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (sketch-themes--variant-with-colors
        ',variant
        (apply 'custom-theme-set-faces ',name
               (sketch-themes--faces-spec)))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'sketch-themes)

;;; sketch-themes.el ends here
