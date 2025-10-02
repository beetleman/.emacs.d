#!/usr/bin/env bash

rm -rf eln-cache elpa
emacs -Q --script fix-elpa.el
