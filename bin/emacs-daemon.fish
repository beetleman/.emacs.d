#!/usr/bin/env fish
set DIR $HOME/.emacs.d/bin

/usr/bin/emacs --daemon --no-x-resources

eval $DIR/emacs.fish
