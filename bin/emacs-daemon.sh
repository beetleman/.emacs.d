#!/usr/bin/env zsh
DIR=${HOME}/.emacs.d/bin

source ~/.zshrc

/usr/bin/emacs --fg-daemon
$DIR/emacs.sh
