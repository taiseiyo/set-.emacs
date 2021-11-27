#!/usr/bin/env bash

package_list="helm jedi tern tern-auto-complete emmet-mode prettier-js undo-tree lua-mode codic rjsx-mode flycheck irony meghanada tide clang-format visual-regexp-steroids ac-slime slime"

emacsclient -e "(package-refresh-contents)"

for pkg in $package_list;do
    emacsclient -e "(package-install '$pkg)"
done

for dotfile in $(ls dotfiles | grep set | sed -nr "s/set-(.*)/\1/p");do
    cp $dotfile $HOME/
done

sudo cp el-files/* /usr/share/emacs/27.1/site-lisp/
