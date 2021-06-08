;; -*- Emacs-Lisp -*-

;;(setq initial-buffer-choice t)

;; swap BS and DEL

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq load-path(append '("~/.emacs.d/lisp/") load-path))


(keyboard-translate ?\C-h ?\C-?)

(add-to-list 'default-frame-alist
	     '(font . "terminus-18"))

;; X11 specific
(when (eq window-system 'x)
  (scroll-bar-mode -1)
  ;; sudo apt install xfonts-terminus
  (set-frame-font "terminus-18")
  ;; face
  (dolist (elem '((bold "LightGoldenrod")
		  (underline "PaleGreen")
		  (mode-line "black" "PaleGreen3" bold)
		  (mode-line-inactive "PaleGreen" "black")
		  (link "PaleGreen")
		  (link-visited "salmon")
		  (font-lock-builtin-face "aquamarine1")
		  (font-lock-keyword-face "aquamarine1" nil bold)
		  (font-lock-function-name-face "aquamarine1" nil bold)

		  (font-lock-constant-face "aquamarine2")
		  (font-lock-variable-name-face "aquamarine2")
		  (font-lock-type-face "LightCyan" nil bold)
		  (font-lock-preprocessor-face "LightCyan")

		  (font-lock-warning-face "orange")
		  (font-lock-negation-char-face "orange")
		  (font-lock-regexp-grouping-backslash "orange")
		  (font-lock-regexp-grouping-construct "orange")

		  (font-lock-comment-face "orange")
		  (font-lock-comment-delimiter-face "orange")
		  (font-lock-string-face "orange")
		  (font-lock-doc-face "orange")))
    (set-face-attribute (car elem) nil
			:foreground (nth 1 elem)
			:background (nth 2 elem)
			:weight (or (nth 3 elem) 'normal))))



;;()を対応させる設定
(electric-pair-mode 1)
;;~/.emacs.d/init.el を読み込む設定
(load (expand-file-name (concat (getenv "HOME") "/.emacs.d/init")))

;;auto-complete を読み込む設定
(require 'auto-complete)
(require 'auto-complete-config)
;;デフォルトで auto-complete を有効にする
(global-auto-complete-mode t)
;; (custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (prettier-js add-node-modules-path migemo undo-tree package-utils multiple-cursors js-format js-auto-format-mode jedi helm exec-path-from-shell company-jedi))))
;; (custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; )

;; latex の変数
(setq org-latex-pdf-process
      '(
	;; "sed -i -e /title\{/d %b.tex" "sed -i -e /author\{/d %b.tex"
	;; "sed -i -e /today\{/d %b.tex"
	"uplatex %b.tex" "uplatex %b.tex" "dvipdfmx %b.dvi"))


;; 一部漢字が変に表示されるのを戻す
(set-language-environment "Japanese")

;; C-c C-e l o でコンパイルした pdf ファイルを開くコマンドを設定する
;; ↓ だと mupdf で開く
(eval-after-load "org"
  '(progn
     (delete '("\\.pdf\\'" . default) org-file-apps)
     (add-to-list 'org-file-apps '("\\.pdf\\'" . "mupdf %s"))
     )
  )

;;utf-8 の設定
(prefer-coding-system 'utf-8)
;; (setq coding-system-for-write 'utf-8)
;; (put 'upcase-region 'disabled nil)

;;undo-tree の設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;package-install の鍵設定(gnu)
(setq package-check-signature nil)

;;flylint の設定
(autoload 'flylint-mode "flylint" nil t)
(add-hook 'python-mode-hook
	  '(lambda ()
	     (flylint-mode 1)))
(add-hook 'sh-mode-hook
	  '(lambda ()
	     (flylint-mode 1)))
(add-hook 'c-mode-hook
  	  '(lambda ()
	     (flylint-mode 1)))
(add-hook 'emacs-lisp-mode-hook
  	  '(lambda ()
	     (flylint-mode 1)))



;; mark 消さない
(setq mew-delete-unread-mark-by-mark nil)
;;mew の IMAP passward 飛す
(setq mew-use-cached-passwd t)
;; ~file がでないようにする設定
(setq make-backup-files nil)

;; pep8 の設定：保存時にバッファ全体を自動整形する
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; emacs 上で PDF の注釈を付ける設定
(setq doc-view-scale-internally nil)
(add-hook 'doc-view-mode-hook
      '(lambda ()
         (local-set-key "c" 'doc-annotate-add-annotation)
         (local-set-key [mouse-1] 'doc-annotate-add-annotation)))
(autoload 'doc-annotate-mode "doc-annotate")
(autoload 'doc-annotate-add-annotation "doc-annotate")
(add-to-list 'auto-mode-alist '("\\.ant$" . doc-annotate-mode))

;; シェバンがあれば自動で実行権限を与える
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; helm の簡易設定
(require 'helm-lib)
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; tab 補完時に後方からの検索が可能になっている
;; (ctrl-x,ctrl-f),(ctrl-x,ctrl-b) などのディレクトリ検索時に使用可能
(ido-mode t)

;;Open Emacs/shell directory in shell/Emacs
(require 'server)
(unless (server-running-p)
  (server-start))

;;org-table mode を楽にする
;; (require 'org-eldoc)

;; (defadvice org-eldoc-documentation-function (around add-field-info activate)
;;   (or
;;    (ignore-errors (and (not (org-at-table-hline-p)) (org-table-field-info nil)))
;;    ad-do-it))

;; (add-hook 'org-mode-hook 'eldoc-mode)

;; (eldoc-add-command-completions
;;  "org-table-next-" "org-table-previous" "org-cycle")

;;emacs 上での jedi の python mode の設定
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (require 'jedi-core)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:server-command '("/home/taisei/.emacs.d/elpa/jedi-core-20210202.856/jediepcserver.py"))

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t)


;;emacs での自動スペルチェック
;;sudo apt-get install aspell aspell-en
;;echo lang en_US > ~/.aspell.conf

(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
'(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(mapc                                   ;; 以下 flyspell-mode の設定
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   ;; ここに書いたモードではコメント領域のところだけ flyspell-mode が有効になる
   python-mode
   emacs-lisp-mode-hook
   ))
(mapc
 (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
 '(
   ;; ここに書いたモードでは flyspell-mode が有効になる
     latex-mode-hook
     org-mode-hook
     ))

(require 'wise-compile)
(require 'pman)

(global-set-key "\C-xp" 'pman)

;; emacs lisp を書く時の設定
(require 'hl-defined)
(add-hook 'emacs-lisp-mode-hook 'hdefd-highlight-mode nil)
;; エンターキーを押した時に改行をいれない
(setq skk-egg-like-newline t)

;; org-mode で presentation を行う時の設定 
;; (autoload 'org-present "org-present" nil t)

;; html,css で使いやすいようにする設定: emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation

;; javascript の format を整える
;; semicoron の自動挿入
(require 'prettier-js)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))
(add-hook 'web-mode-hook #'(lambda ()
                            (enable-minor-mode
                             '("\\.jsx?\\'" . prettier-js-mode))))
(setq prettier-js-args '(
  "--trailing-comma" "all"
  "--bracket-spacing" "false"
))

;; 全角と半角の間に自動でスペースを入れる
;; https://github.com/coldnew/pangu-spacing
(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company-irony flycheck-irony irony helm-migemo auto-complete jedi-core company-jedi rjsx-mode codic helm-elscreen elscreen org-preview-html tern-auto-complete tern org-plus-contrib pangu-spacing migemo undo-tree prettier-js package-utils multiple-cursors js-format js-auto-format-mode jedi helm exec-path-from-shell add-node-modules-path))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq js-indent-level 2)
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

;;org-preview-mode の設定
(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "elisp/")))

;; 動的補完
(setq skk-dcomp-activate t)
;; アノテーション表示
(setq skk-show-annotation t)
;; 送り仮名が厳密に正しい候補を優先して表示する
(setq skk-henkan-strict-okuri-precedence t)
;; codic の api key の作成
(setq codic-api-token "O34o0PIozjDzqSLnuU31AxwOF8l1ERb7Lk")
;; codic をグローバル設定にする
(global-set-key "\C-c\C-o" 'codic-translate)

;; React の jsx の設定
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))

;; package-install -> tern and tern-auto-complete
;; javascript プログラムの関数自動補完設定
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '(".*\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
    (lambda ()
        (tern-mode t)))

(eval-after-load 'tern
    '(progn
        (require 'tern-auto-complete)
        (tern-ac-setup)))

;; ロックファイルを消す
(setq create-lockfile nil)

;; scss-mode の編集の設定 → scss  → css
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save t)

(require 'auto-complete-config)
(defun css-setup-ac ()
  (setq ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (ac-css-mode-setup)
  (auto-complete-mode 1))

(add-hook 'css-mode-hook #'css-setup-ac)

;; migemo の設定

(require 'migemo)
;; cmigemo(default) → sudo apt install cmigemo
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
;; ruby migemo
(setq migemo-command "ruby")
(setq migemo-options '("-S" "migemo" "-t" "emacs" "-i" "\a"))
;; Set your installed path → https://github.com/kyanagi/migemo-dict
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)

;; irony -> c 言語関係 → https://note.com/imaich1/n/nbb909b4bb55b#W2eiS
;; flyckeck -> c 言語関係 error highlight
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(setq company-idle-delay 0)

;; c++ の formatter → sudo apt install clang-format and ~/.clang-format
;; auto-clang-format.el
(require 'auto-clang-format)
(add-hook 'c++-mode-hook 'auto-clang-format-mode)
(add-hook 'c-mode-hook 'auto-clang-format-mode)



;; Lookup → 辞書の設定
;; https://qiita.com/tet_kawagishi/items/f860801befb7674ae498
;; https://wordnetepwing.osdn.jp/ から wordnet-enjp をダウンロード
;; https://classicalepwing.osdn.jp/ から eld をダウンロード

(autoload 'lookup "lookup" nil t)
(autoload 'lookup-region "lookup" nil t)
(autoload 'lookup-pattern "lookup" nil t)
(define-key ctl-x-map "l" 'lookup)
(define-key ctl-x-map "y" 'lookup-region)
(define-key ctl-x-map "\C-y" 'lookup-pattern)
(setq lookup-search-agents
      '(
	(ndeb "/usr/share/emacs/26.1/site-lisp/dict/wordnet-enjp")
	(ndeb "/usr/share/emacs/26.1/site-lisp/dict/eld")))

(setq lookup-default-dictionary-options
      '((:stemmer .  stem-english)))
(setq lookup-use-kakasi nil)

;; 色設定
(set-face-background 'default "#172727")
(set-face-foreground 'default "white")
;; メニューバーの非表示
(menu-bar-mode -1)
;; ツールバーの非表示
(tool-bar-mode -1)

