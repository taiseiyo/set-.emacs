;; メニューバーの非表示
(push '(menu-bar-lines . 0) default-frame-alist)
;; ツールバーの非表示
(push '(tool-bar-lines . 0) default-frame-alist)
;; スクロールバーの非表示
(scroll-bar-mode -1)

;; FiraCode の Nerd Font(github より入手)に変更
(add-to-list 'default-frame-alist
             '(font . "FiraCode Nerd Font-18"))

;; 色設定
(set-face-background 'default "#172727")
(set-face-foreground 'default "white")
