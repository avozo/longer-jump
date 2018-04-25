# hst-mode

A workable solution to "going back" in Emacs. All IDEs (and even Vim)
have ways to go back to the last "place" (cursor position) you were
at.

It records your cursor positions over time. When you haven't moved
much from a particular area, that is considered a place and is added
to the mark ring. This method is a major simplification over the
methods used by the previous commits, but I'm still tweaking it every
now and then because I really use this everyday.

This only works for the local bufffer. You can't skip to your last
place in another indeterminate buffer. Good.

There are no third party dependencies.

## Installation

Just put longer-jump.el into your LOAD-PATH; e.g.:

``bash
$ curl https://github.com/buysilver/longer-jump/blob/master/hst-mode.el > ~/.emacs.d/lisp/hst-mode.el
```

in your ``~/.emacs``:
```lisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'hst-mode)

;; only if you want this to be enabled everywhere!
(global-hst-mode 1)

;; otherwise you need to do M-x hst-mode on a per-buffer basis or something like…
(add-hook 'text-mode-hook #'hst-mode)
```

## Usage

``M-x hst-mode`` in a buffer you want to use this in.

Use F9 to move back in your history, F12 to move forward. To remap these keys:

```lisp
(define-key hst-map (kbd "C-}") 'hst-forward)
;                   ^^^^^^^^^^^ or anything you want
(define-key hst-map (kbd "C-{") 'hst-back)
;                   ^^^^^^^^^^^ or anything you want
```

To change the number of positions that are remembered, change the max size of your mark ring. As of Emacs 25, it is 16 by default; that is, the last 16 spots will be remembered.

```lisp
(setq-default mark-ring-max 5) ;; or whatever
(setq-default global-mark-ring-max 5)
```