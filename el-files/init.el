(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;カッコの部分を明るくさせる
(show-paren-mode t)
(setq show-paren-style 'parenthesis) 

;;追加のパッケージを登録
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/archive-contents/")))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/archive-contents/"))

(package-initialize)

