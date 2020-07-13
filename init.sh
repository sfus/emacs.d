#!/bin/sh

if [ -d ~/.emacs.d ]; then
    cd ~/.emacs.d
    emacs -Q --batch -l init.el
fi
