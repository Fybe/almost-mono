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
    (foreground . "#ffffff")
    (weak       . "#aaaaaa")
    (weaker     . "#555555")
    (weakest    . "#333333")
    (highlight  . "#6b83f9")
    (warning    . "#ffaa00")
    (error      . "#f28799")
    (success    . "#99f287")
    (string     . "#b5e4fc")))

(defmacro almost-mono-theme--with-colors (&rest body)
  "Execute BODY in a scope where the different colors are bound."
  `(let* ((colors     almost-mono-theme-colors)

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
      (minibuffer-prompt (:weight extra-bold))
      (isearch (:background ,weak :foreground ,foreground :bold t))
      (lazy-highlight (:background ,weaker :foreground ,foreground))
      (link (:underline t))
      (highlight (:background ,weakest :weight ultra-bold))
      (italic (:italic t))


      (success (:foreground ,success :weight extra-bold))
      (warning (:foreground ,warning :weight extra-bold))
      (error   (:foreground ,error   :weight extra-bold))

      ;; mode line
      (mode-line (:background ,background :foreground ,foreground :weight extra-bold))
      (mode-line-inactive (:background ,background :foreground ,weak))

      ;; font lock
      (font-lock-keyword-face (:foreground ,highlight :bold t))
      (font-lock-function-name-face (:weight extra-bold))
      (font-lock-variable-name-face (:foreground ,foreground))
      (font-lock-warning-face (:foreground ,warning))
      (font-lock-builtin-face (:bold t :foreground ,foreground))
      (font-lock-constant-face (:foreground ,weak))
      (font-lock-type-face (:foreground ,foreground))
      (font-lock-preprocessor-face (:foreground, weak))
      (font-lock-comment-face (:foreground ,weak :italic t))
      (font-lock-string-face (:foreground ,string))
      (font-lock-doc-face (:inherit font-lock-comment-face :weight bold))
      (line-number (:foreground ,weaker))
      (linum (:inherit line-number))
      (vertical-border (:foreground ,weaker))
      (fill-column-indicator (:foreground ,weakest))
      (window-divider (:foreground ,weakest))
      (window-divider-first-pixel (:foreground ,weakest))
      (window-divider-last-pixel (:foreground ,weakest))

      ;; eshell
      (eshell-prompt (:foreground ,foreground :weight bold))
      (eshell-ls-directory (:foreground ,foreground :weight extra-bold))
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
      (compilation-info (:foreground ,weak :weight extra-bold))

      ;; eglot
      (eglot-highlight-symbol-face ())

      ;; whitespace-mode
      (whitespace-tab (:background nil :foreground ,weaker))
      (whitespace-space (:background nil :foreground ,weakest))
      (whitespace-indentation (:background nil :foreground ,weakest))

      ;; selectrum
      (completion-annotations (:foreground ,weak))
      ))))

(defmacro almost-mono-theme--define-theme ()
  "Define the almost-mono theme."
  (let ((name 'almost-mono)
        (doc "almost mono theme"))
    `(progn
       (deftheme ,name ,doc)
       (put ',name 'theme-immediate t)
       (almost-mono-theme--with-colors
        (apply 'custom-theme-set-faces ',name
               (almost-mono-theme--faces-spec)))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(almost-mono-theme--define-theme)

(provide 'almost-mono-theme)

;;; almost-mono-theme.el ends here
