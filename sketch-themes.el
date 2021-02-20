;;; sketch-themes.el --- Sketch color themes -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Daw-Ran Liou

;; Author: Daw-Ran Liou <hi@dawranliou.com>
;; URL: https://github.com/dawranliou/emacs.d/themes
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
              (weak      . "#888888")
              (weaker    . "#dddddd")
              (weakest   . "#efefef")
              (highlight . "#fee761")
              (success   . "#63c74d")
              (warning   . "#e43b44")))

    (black . ((fg        . "#f0f6f0")
              (bg        . "#222323")
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
      (fringe (:background ,bg))
      (shadow (:background ,weakest))
      (highlight (:foreground ,fg :background ,highlight))
      (region (:foreground ,fg :background ,highlight))
      (show-paren-match (:foreground ,success :bold t))
      (show-paren-mismatch (:foreground ,warning :bold t))
      (minibuffer-prompt (:bold t :foreground ,fg))
      (isearch (:bold t :foreground ,fg :background ,weak :bold t))
      (lazy-highlight (:foreground ,fg :background ,weaker))
      (link (:underline t))
      (parenthesis (:foreground ,weak))
      (trailing-whitespace (:foreground nil :background ,warning))
      (cursor (:background ,fg :foreground ,bg))
      (vertical-border (:foreground ,weaker))
      (default-italic (:italic t))
      (line-number (:background ,bg :foreground ,weaker))
      (line-number-current-line (:background ,bg :foreground ,fg))

      ;; mode line
      (mode-line (:foreground ,fg :background ,weakest))
      (mode-line-inactive (:foreground ,weaker :background ,weakest))

      ;; font lock
      (font-lock-builtin-face (:foreground ,fg))
      (font-lock-comment-face (:inherit font-lock-string-face))
      (font-lock-negation-char-face (:foreground ,fg))
      (font-lock-reference-face (:foreground ,fg))
      (font-lock-constant-face (:foreground ,fg :bold t))
      (font-lock-doc-face (:inherit font-lock-comment-face))
      (font-lock-function-name-face (:foreground ,fg :bold t))
      (font-lock-keyword-face (:foreground ,fg))
      (font-lock-string-face (:foreground ,weak))
      (font-lock-type-face (:foreground ,fg))
      (font-lock-variable-name-face (:foreground ,fg :bold t))
      (font-lock-warning-face (:underline (:color ,warning :style wave)))
      (fill-column-indicator (:foreground ,weakest))

      ;; clojure mode
      (clojure-keyword-face (:foreground ,fg))

      ;; hl line
      (hl-line (:background ,weakest))

      ;; hl fill column
      (hl-fill-column-face (:background ,weaker))

      ;; company
      (company-tooltip (:foreground ,fg :background ,weakest))
      (company-tooltip-selection (:background ,weaker :foreground ,fg))
      (company-tooltop-annotation (:foreground ,fg))
      (company-tooltip-common (:foreground ,fg :bold t))
      (company-tooltip-common-selection (:foreground ,fg :bold t))
      (company-scrollbar-bg (:background ,weaker))
      (company-scrollbar-fg (:background ,weak))

      ;; git gutter
      (git-gutter:modified (:background ,highlight :foreground ,highlight))
      (git-gutter:added (:background ,success :foreground ,success))
      (git-gutter:deleted (:background ,warning :foreground ,warning))

      ;; org mode
      (org-block (:extend t :background ,weakest :inherit (shadow fixed-pitch)))
      (org-code (:inherit (shadow fixed-pitch)))
      (org-table (:inherit (shadow fixed-pitch)))
      (org-verbatim (:inherit (shadow fixed-pitch)))
      (org-special-keyword (:inherit (font-lock-comment-face fixed-pitch)))
      (org-meta-line (:inherit (font-lock-comment-face fixed-pitch)))
      (org-checkbox (:inherit fixed-pitch))
      (org-hide (:inherit fixed-pitch :foreground ,bg))
      (org-document-title (:height 1.5))
      (org-level-1 (:height 1.3))
      (org-level-2 (:height 1.2))
      (org-level-3 (:height 1.1))
      (org-level-4 (:height 1.1))
      (org-level-5 (:height 1.1))
      (org-level-6 (:height 1.1))
      (org-level-7 (:height 1.1))
      (org-level-8 (:height 1.1))
      (org-done (:foreground ,weak :bold t))
      (org-headline-done (:foreground ,fg))
      (org-todo (:foreground ,success :bold t))
      (org-drawer (:foreground ,weak))
      (org-date (:underline t))
      (org-ellipsis (:foreground ,weak))

      ;; flymake mode
      (flymake-warning (:underline (:style wave :color ,weak)))
      (flymake-error (:underline (:style wave :color ,warning)))

      ;; flycheck mode
      (flycheck-warning (:underline (:style wave :color ,weak)))
      (flycheck-error (:underline (:style wave :color ,warning)))

      ;; flyspell mode
      (flyspell-duplicate (:underline (:color ,weak :style wave)))
      (flyspell-incorrect (:underline (:color ,warning :style wave)))))))


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
