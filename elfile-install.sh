#!/usr/bin/env bash

package_list="helm tern tern-auto-complete emmet-mode prettier-js undo-tree lua-mode codic rjsx-mode flycheck irony meghanada tide"

emacsclient -e "(package-refresh-contents)"

for pkg in $package_list;do
    emacsclient -e "(package-install '$pkg)"
done
