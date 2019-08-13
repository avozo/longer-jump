# hst-mode --- Go back to last cursor positions in Emacs

A workable solution to ``going back'' in Emacs. All IDEs (and even Vim/evil)
have ways to go back to the last ``place'' (cursor position) you were
at.

This only works for the local bufffer. You won't jump around to different buffers, windows, and frames by hitting the back button.

## Installation

Just put ``hst-mode.el`` into your LOAD-PATH; e.g.:

```
$ curl https://github.com/hackharmony/longer-jump/blob/master/hst-mode.el > ~/.emacs.d/lisp/hst-mode.el
```


Then add this to your ``~/.emacs``:


```lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'hst-mode)

;; only if you want this to be enabled everywhere!
(global-hst-mode 1)

;; otherwise you need to do M-x hst-mode on a per-buffer basis or something like…
(add-hook 'text-mode-hook #'hst-mode) ;; track history when writing text
(add-hook 'prog-mode'hook #'hst-mode) ;; track history when writing code
```

## Usage

``M-x hst-mode`` in a buffer you want to use this in.

Default keybindings are ``⌘-[`` to go back in history and ``⌘-]`` to go forward. Change them like this:

```lisp
(define-key hst-map (kbd "C-}") #'hst-mode--go-forward)
;                   ^^^^^^^^^^^ or anything you want
(define-key hst-map (kbd "C-{") 'hst-mode--go-back)
;                   ^^^^^^^^^^^ or anything you want
```

## Customizations

A number of things can be customized. ``M-x customize-group hst`` to see the full list.

* ``hst-mode-max-history-entries`` --- How many entries you want to be remembered
* ``hst-mode-threshold-lines-moved`` --- How many lines you need to approximately move before a new history entry is registered
