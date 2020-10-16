#!/usr/bin/env fish
set DIR $HOME/.emacs.d/bin

/usr/bin/emacs --daemon

eval $DIR/emacs.fish
