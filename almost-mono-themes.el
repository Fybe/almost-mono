;;; almost-mono-theme.el --- Almost monochromatic color theme -*- lexical-binding: t; -*-

;; Copyright (C) 2019 - 2022 John Olsson
;; Copyright (C) 2022        Finn Bender

;; Author: John Olsson <john@cryon.se>
;; Maintainer: Finn Bender <mailobender77x@gmail.com>
;; URL: https://github.com/Fybe/almost-mono-black
;; Created: 9th May 2019
;; Version: 1.1.0
;; Keywords: faces
;; Package-Requires: ((emacs "24"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An almost monochrome Emacs theme.

;;; Code:

(defconst almost-mono-theme-colors
  '((background . "#000000")
    (foreground . "#fffcf2")
    (weak       . "#808080")
    (weaker     . "#404040")
    (weakest    . "#202020")
    (highlight  . "#FFA9D8")
    (warning    . "#FBEDCB")
    (error      . "#F68D96")
    (success    . "#B3FAB3")
    (string     . "#DEDEC4")))

(defconst almost-mono-light-theme-colors
  '((background . "#ffffff")
    (foreground . "#000000")
    (weak       . "#252422")
    (weaker     . "#403d39")
    (weakest    . "#D9CEC1")
    (highlight  . "#B30062")
    (warning    . "#D19A10")
    (error      . "#EE2F3F")
    (success    . "#065B06")
    (string     . "#3B3728")))

(defmacro almost-mono-theme--with-colors (color-palette &rest body)
  "Execute BODY in a scope where the different colors are bound."
  `(let* ((colors     ,color-palette)

          (background (cdr (assoc 'background colors)))
          (foreground (cdr (assoc 'foreground colors)))
          (weak       (cdr (assoc 'weak colors)))
          (weaker     (cdr (assoc 'weaker colors)))
          (weakest    (cdr (assoc 'weakest colors)))
          (highlight  (cdr (assoc 'highlight colors)))
          (warning    (cdr (assoc 'warning colors)))
          (error      (cdr (assoc 'error colors)))
          (success    (cdr (assoc 'success colors)))
          (string     (cdr (assoc 'string colors))))
     ,@body))

(defmacro almost-mono-theme--faces-spec ()
  "Provide the faces specification."
  (quote
   (mapcar
    (lambda (entry) (list (car entry) `((t ,@(cdr entry)))))
    `(
      ;; default
      (default (:background ,background :foreground ,foreground))
      (fixed-pitch (:inherit default))
      (fringe  (:background ,background))
      (region  (:background ,weakest))
      (show-paren-match (:background ,weak :foreground ,background))
      (show-paren-mismatch (:background ,error :foregound ,background))
      (minibuffer-prompt (:weight semi-bold))
      (isearch (:background ,weaker :foreground ,foreground))
      (lazy-highlight (:background ,weaker :foreground ,foreground))
      (link (:underline t))
      (highlight (:background ,weakest :weight ultra-bold))
      (italic (:italic t))
      (secondary-selection (:background ,weaker :distant-foreground ,background :extend t))


      (success (:foreground ,success :weight semi-bold))
      (warning (:foreground ,warning :weight semi-bold))
      (error   (:foreground ,error   :weight semi-bold))

      ;; mode line
      (mode-line (:background ,background :foreground ,foreground))
      (mode-line-inactive (:background ,background :foreground ,weaker))

      ;; font lock
      (font-lock-keyword-face (:foreground ,highlight :weight bold))
      (font-lock-function-name-face (:weight semi-bold))
      (font-lock-variable-name-face (:foreground ,foreground :slant italic))
      (font-lock-warning-face (:foreground ,warning))
      (font-lock-builtin-face (:bold t :foreground ,foreground))
      (font-lock-constant-face (:inherit default))
      (font-lock-type-face (:foreground ,foreground))
      (font-lock-preprocessor-face (:foreground, weak))
      (font-lock-comment-face (:foreground ,weak :italic t))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face :weight bold))
      (line-number (:foreground ,weaker))
      (linum (:inherit line-number))
      (vertical-border (:foreground ,weaker))
      (fill-column-indicator (:foreground ,weakest))
      (window-divider (:background ,background :foreground ,weakest))
      (window-divider-first-pixel (:inherit window-divider))
      (window-divider-last-pixel (:inherit window-divider))

      ;; eshell
      (eshell-prompt (:foreground ,foreground :weight bold))
      (eshell-ls-directory (:foreground ,foreground :weight semi-bold))
      (eshell-ls-archive (:inherit eshell-ls-unreadable))
      (eshell-ls-backup (:inherit eshell-ls-unreadable))
      (eshell-ls-clutter (:foreground ,weak))
      (eshell-ls-executable (:inherit eshell-ls-unreadable))
      (eshell-ls-missing (:inherit eshell-ls-unreadable))
      (eshell-ls-product (:inherit eshell-ls-unreadable))
      (eshell-ls-readonly (:inherit eshell-ls-unreadable))
      (eshell-ls-special (:inherit eshell-ls-unreadable))
      (eshell-ls-symlink (:inherit eshell-ls-unreadable))

      ;; hl line
      (hl-line (:background ,weakest))
      (highlight-current-line-face (:inherit hl-line))

      ;; ido
      (ido-first-match (:bold t))
      (ido-only-match (:bold t))
      (ido-subdir (:italic t))
      (ido-virtual (:foreground ,weak))
      (ido-vertical-match-face (:bold t :italic nil))

      ;; org mode
      (org-table (:foreground ,weak))

      ;; corfu
      (corfu-bar nil)
      (corfu-border (:background ,weaker))
      (corfu-current (:inherit highlight))
      (corfu-default (:background ,background :foreground ,foreground))

      ;; flymake
      (flymake-note (:underline (:color ,weak :style wave)))
      (flymake-warning (:underline (:color ,warning :style wave)))
      (flymake-error (:underline (:color ,error :style wave)))

      ;; eglot
      (eglot-highlight-symbol-face ())
      (eglot-mode-line (:weight bold))

      ;; whitespace-mode
      (whitespace-tab (:foreground ,weakest))
      (whitespace-space (:foreground ,weakest))
      (whitespace-indentation (:foreground ,warning))
      (whitespace-trailing (:foreground ,error :background ,background))

      ;; selectrum
      (completion-annotations (:foreground ,weak))

      ;; tree-sitter
      (tree-sitter-hl-face:attribute (:foreground ,weak))
      (tree-sitter-hl-face:constant ())
      (tree-sitter-hl-face:constant.builtin ())
      (tree-sitter-hl-face:constructor ())
      (tree-sitter-hl-face:function.call ())
      (tree-sitter-hl-face:function.macro ())
      (tree-sitter-hl-face:function (:weight bold))
      (tree-sitter-hl-face:keyword (:foreground ,highlight :weight semi-bold))
      (tree-sitter-hl-face:include (:foreground ,highlight :weight semi-bold))
      (tree-sitter-hl-face:label (:foreground ,weak))
      (tree-sitter-hl-face:method.call ())
      (tree-sitter-hl-face:operator ())
      (tree-sitter-hl-face:property ())
      (tree-sitter-hl-face:punctuation.delimiter ())
      (tree-sitter-hl-face:punctuation.bracket ())
      (tree-sitter-hl-face:type ())
      (tree-sitter-hl-face:type.builtin ())
      (tree-sitter-hl-face:variable ())
      ))))

(defmacro almost-mono-theme--define-theme (name doc color-scheme)
  "Define the almost-mono themes."
  `(progn
     (deftheme ,name ,doc)
     (put ',name 'theme-immediate t)
     (almost-mono-theme--with-colors ,color-scheme
                                     (apply 'custom-theme-set-faces ',name
                                            (almost-mono-theme--faces-spec)))
     (provide-theme ',name)))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'almost-mono-themes)

;;; almost-mono-theme.el ends here
