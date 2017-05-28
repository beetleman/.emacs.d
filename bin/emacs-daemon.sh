#!/usr/bin/env zsh
DIR=${HOME}/.emacs.d/bin

source ~/.zshrc

/usr/bin/emacs --daemon
$DIR/emacs.sh
