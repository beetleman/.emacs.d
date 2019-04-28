#!/usr/bin/env fish

set DIR $HOME/.emacs.d/bin

emacsclient -c -a "$DIR/emacs-daemon.fish" $argv
