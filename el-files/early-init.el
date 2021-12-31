;; メニューバーの非表示
(push '(menu-bar-lines . 0) default-frame-alist)
;; ツールバーの非表示
(push '(tool-bar-lines . 0) default-frame-alist)
;; スクロールバーの非表示
(scroll-bar-mode -1)

;; 色設定
(set-face-background 'default "#172727")
(set-face-foreground 'default "white")
