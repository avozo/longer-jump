# Longer Jump

A proper solution to "going back" in Emacs. All IDEs (and even Vim)
have a way to go back to the plast place (cursor position) you were
working on. C-u C-SPC and anything relying on mark rings doesn't quite
cut it. I decided to use the native list of undo positions, which
seems to be quite fast and relevant to this purpose.

This only works for the local bufffer. You can't skip to your last
place in another indeterminate buffer. Good.

There are no third party dependencies.

"Take me back to where I was last" is a very ephemeral request. It
could mean almost anything depending on things not limited to your
mood or what phase of your development cycle you are in. I don't think
this is something that could be solved with a deterministic
approach. A neural net (LSTM?) trained on scrolling movements would be
better suited.

## Installation
Just load the file in your .emacs or init.el. I like these keybindings:

```lisp
(global-set-key (kbd "C-}") 'history-forward)
(global-set-key (kbd "C-{") 'history-back)
```

## TODO
* defcustom
* Better installation instructions
* Record usage for a gif/webm
* Clean up, optimize, etc.
