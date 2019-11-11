(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;カッコの部分を明るくさせる
(show-paren-mode t)
(setq show-paren-style 'parenthesis) 

;;追加のパッケージを登録
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/archive-contents/")))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
