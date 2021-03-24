(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;カッコの部分を明るくさせる
(show-paren-mode t)
(setq show-paren-style 'parenthesis) 

;;追加のパッケージを登録

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))

(package-initialize)

