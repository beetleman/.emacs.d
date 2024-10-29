#!/usr/bin/env bash

echo download xml lsp
LEMMINX_DIR=~/.emacs.d/share/lemminx
mkdir -p $LEMMINX_DIR
curl https://download.eclipse.org/lemminx/releases/0.27.1/org.eclipse.lemminx-uber.jar -o $LEMMINX_DIR/org.eclipse.lemminx-uber.jar
