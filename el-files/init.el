(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;;カッコの部分を明るくさせる
(show-paren-mode t)
(setq show-paren-style 'parenthesis) 
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")



;;追加のパッケージを登録
(require 'package)
(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")))


;; (setq package-archives
;;       '(("gnu"   . "http://elpa.gnu.org/packages/")))

(package-initialize)

