;;; emacs-setup Sat Nov  9 12:29:18 2013
;;; for emacs 24.3(now)
;;; 想定環境 Linux(Debian) Mac Windows
;;; 外部に投げる場合や環境依存の物を使う場合、system-typeを見てunless
;;; かwhenしましょう。 unlessよりは(when (not (eq system-type 'hoge)))
;;; のほうが可読性良いかもしれない。
(set-language-environment 'Japanese)
(setq default-major-mode 'text-mode)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;use commonlisp
(with-no-warnings
  (require 'cl))

;;package config
(require 'package)
(add-to-list 'package-archives
     '("marmalade" .
       "http://marmalade-repo.org/packages/")
     '("org" .
       "http://orgmode,org/elpa/"))
(package-initialize)

;;;  alias emacs = emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))
(setq gc-cons-threshold (* 50 gc-cons-threshold))

;;外部へのIOはコストが高すぎる。　二度目のevalがされないように何かしらの対策をすることが必要である。
(package-refresh-contents)

(defvar my/favorite-packages
  '(
    starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell
    ;; for Clojure settings
    clojure-mode
    clojure-project-mode
    clojure-test-mode
    clojurescript-mode
    nrepl

    color-theme
    color-theme-molokai

    lua-mode
    project-mode
    rainbow-mode
    undo-tree
    yasnippet

    ;;term
    multi-term

    multiple-cursors
    ))

;; my/favorite-packagesからインストールする
(dolist (package my/favorite-packages)
  (when (not (package-installed-p package))
    (package-install package)))


(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Consolas")
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic ProN"))
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("Hiragino Maru Gothic ProN")))

(require 'color-theme)
(load-theme 'misterioso t)


;;multi-tarm setting
(require 'multi-term)
(setq multi-term-program "/bin/zsh")

;;; other settings 
;;; あのビープ音を削除する
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;変更された時に自動でバッファ読み直し
(global-auto-revert-mode 1)

;parenthesis
(show-paren-mode t)

(line-number-mode t)
(column-number-mode t)

;time setting
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t)



;;;(set-background-color "Black")
;;;(set-foreground-color "White")
;;;(set-cursor-color "White")
(set-frame-parameter nil 'alpha 85)

(set-cursor-color "Blue")

;;; kill -9 fuckin em-space!!!!!!!!!!!!!!!!!!
(global-whitespace-mode 1)

(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-display-mappings
       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        ))
(set-face-foreground 'whitespace-tab "#adff2f")
(set-face-background 'whitespace-tab 'nil)
(set-face-underline  'whitespace-tab t)
(set-face-foreground 'whitespace-space "#7cfc00")
(set-face-background 'whitespace-space 'nil)
(set-face-bold-p 'whitespace-space t)


;;;programming support 
(require 'undo-tree)
(global-undo-tree-mode)

(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

(require 'flymake)

;;src package setting
(add-to-list 'load-path "/Users/keihosoya/.emacs.d/src/bongo")
(add-to-list 'exec-path "/Applications/VLC.app/Contents/MacOS")
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)
(setq bongo-enabled-backends '(vlc))

;; howm
(add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
(setq howm-menu-lang 'ja)
(require 'howm-mode)

;; my function
(when (eq system-type 'darwin)
  (defun open-current-dir-with-finder  ()
    (interactive)
    (shell-command (concat "open ."))))

