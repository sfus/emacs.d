#!/bin/sh

BASEDIR=$(cd $(dirname $0); pwd)

emacs -nw -q -l $BASEDIR/init.el $@
