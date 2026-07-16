#!/usr/bin/env bash

EMACS_DIR="$HOME/.emacs.d"
LEMMINX_DIR="$EMACS_DIR/share/lemminx"
PANDOC_DIR="$EMACS_DIR/pandoc"

echo download xml lsp
mkdir -p "$LEMMINX_DIR"
curl --fail --location \
    "https://download.eclipse.org/lemminx/releases/0.31.1/org.eclipse.lemminx-uber.jar" \
    --output "$LEMMINX_DIR/org.eclipse.lemminx-uber.jar"

echo download pandoc preview tools
mkdir -p "$PANDOC_DIR"
curl --fail --location \
    "https://raw.githubusercontent.com/pandoc-ext/diagram/main/_extensions/diagram/diagram.lua" \
    --output "$PANDOC_DIR/diagram.lua"
curl --fail --location \
    "https://raw.githubusercontent.com/sindresorhus/github-markdown-css/v5.9.0/github-markdown.css" \
    --output "$PANDOC_DIR/github-markdown.css"
