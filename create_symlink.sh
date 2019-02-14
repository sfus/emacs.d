#!/bin/sh

# get script located dir
DIR=$(cd $(dirname $0);pwd)

set -e
set -x

# Emacs
install -d ~/.emacs.d
install -d ~/.emacs.d/lisp
ln -sf ${DIR}/init.el ~/.emacs.d/init.el
ln -sf ${DIR}/init.sh ~/.emacs.d/init.sh
ln -sf ${DIR}/my_snippets ~/.emacs.d/
ln -sf ${DIR}/ac-dict ~/.emacs.d/
ln -sf ${DIR}/init-el-get.el ~/.emacs.d/
ln -sf ${DIR}/init-el-get-extra.el ~/.emacs.d/
ln -sf ${DIR}/init-loader ~/.emacs.d/
