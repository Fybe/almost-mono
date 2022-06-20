Almost mono black theme
==================

An almost monochrome Emacs theme.

Installation
============

Use package
-----------
If you have use-package setup on your system loading almost-mono-themes is as simple as:

```lisp
(use-package almost-mono-black-theme
  :config
  (load-theme 'almost-mono-black t))
```

Manual installation
-------------------
If you prefer, you can install almost-mono-themes manually by downloading the elisp files in this repo and place them somewhere in your <code>load-path</code> or <code>custom-theme-load-path</code> (see example below).

You can set your paths by adding these lines to your <code>.emacs.d</code> or <code>.emacs.d/init.el</code>:

```lisp
;; Put the theme file almost-mono-black-theme.el in your load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Put the individual theme files almost-mono-black-theme.el in your theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
```

You should now be able to load almost-mono-themes with <code>M-x load-theme RET almost-mono-{black, white, etc.} RET</code>!
