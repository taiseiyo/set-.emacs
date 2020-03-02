;; -*- Emacs-Lisp -*-

;;(setq initial-buffer-choice t)

;; swap BS and DEL

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq load-path(append '("~/.emacs.d/lisp/") load-path))

(package-initialize)

(keyboard-translate ?\C-h ?\C-?)

;; X11 specific
(when (eq window-system 'x)
  (scroll-bar-mode -1)
  (set-default-font "terminus-24")
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
;;~/.emacs.d/init.elを読み込む設定
(load (expand-file-name (concat (getenv "HOME") "/.emacs.d/init")))

;;auto-completeを読み込む設定
(require 'auto-complete)
(require 'auto-complete-config)
;;デフォルトでauto-completeを有効にする
(global-auto-complete-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm exec-path-from-shell undo-tree package-utils auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;utf-8の設定
(prefer-coding-system 'utf-8)
;; (setq coding-system-for-write 'utf-8)
;; (put 'upcase-region 'disabled nil)

;;undo-treeの設定
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;package-install の鍵設定(gnu)
(setq package-check-signature nil)

;;flylintの設定
(autoload 'flylint-mode "flylint" nil t)
(add-hook 'python-mode-hook
	  '(lambda ()
	     (flylint-mode 1)))
(add-hook 'perl-mode-hook
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
;;mewのIMAP passward 飛す
(setq mew-use-cached-passwd t)
;; ~fileがでないようにする設定
(setq make-backup-files nil)

;; pep8の設定：保存時にバッファ全体を自動整形する
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; emacs上でPDFの注釈を付ける設定
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
(require 'org-eldoc)

(defadvice org-eldoc-documentation-function (around add-field-info activate)
  (or
   (ignore-errors (and (not (org-at-table-hline-p)) (org-table-field-info nil)))
   ad-do-it))

(add-hook 'org-mode-hook 'eldoc-mode)

(eldoc-add-command-completions
 "org-table-next-" "org-table-previous" "org-cycle")

;;emacs 上での jedi の python mode の設定
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;emacs での自動スペルチェック
;;sudo apt-get install aspell aspell-en
;;echo lang en_US > ~/.aspell.conf

(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
'(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(mapc                                   ;; 以下flyspell-modeの設定
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   ;; ここに書いたモードではコメント領域のところだけflyspell-mode が有効になる
   python-mode
   emacs-lisp-mode-hook
   ))
(mapc
 (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
 '(
   ;; ここに書いたモードではflyspell-mode が有効になる
     latex-mode-hook
     org-mode-hook
     ))
