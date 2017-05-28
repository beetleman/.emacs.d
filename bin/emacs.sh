#!/usr/bin/env bash

DIR=${HOME}/.emacs.d/bin

emacsclient -c -a "${DIR}/emacs-daemon.sh" -s ~/.emacs.d/server/server
