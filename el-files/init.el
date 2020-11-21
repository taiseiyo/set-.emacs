(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;カッコの部分を明るくさせる
(show-paren-mode t)
(setq show-paren-style 'parenthesis) 

;;追加のパッケージを登録

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

