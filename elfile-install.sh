#!/usr/bin/env bash

# skkdic-extra → 日本語の追加登録
sudo apt install skkdic-extra cmigemo fzf

package_list="helm jedi tern tern-auto-complete emmet-mode prettier-js undo-tree lua-mode codic rjsx-mode flycheck irony meghanada tide clang-format visual-regexp-steroids ddskk"

emacsclient -e "(package-refresh-contents)"

for pkg in $package_list ; do
    echo $pkg 
    emacsclient -e "(package-install '$pkg)"
done

for dotfile in $(ls dotfiles | grep set | sed -nr "s/set-(.*)/\1/p") ; do
    cp dotfiles/$dotfile $HOME/
done


sudo cp el-files/* /usr/local/share/emacs/28.0.92/site-lisp/
cp el-files/init.el el-files/early-init.el ~/.emacs.d/
