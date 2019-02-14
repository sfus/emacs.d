#!/bin/sh

if [ -d ~/.emacs.d ]; then
    cd ~/.emacs.d
    emacs -Q -nw -l init.el
fi
