;;;; emacs-setup Sat Nov  9 12:29:18 2013
;;;; for emacs 24.3(now)
;;;; 基本的に例外が無い限り最新版を追うこととします
;;;; 想定環境 Linux(Debian) Mac Windows
;;;; 外部に投げる場合や環境依存の物を使う場合、system-typeを見てunless
;;;; かwhenしましょう。 unlessよりは(when (not (eq system-type 'hoge)))
;;;; のほうが可読性良いかもしれない。
;;;; 使っているシステムの情報
;;;; mac:darwin windows:windows-nt
(set-language-environment 'Japanese)
(setq default-major-mode 'text-mode)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; use commonlisp
(eval-when-compile
  (require 'cl))

;;; def macro

(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))

(defmacro addlist (target &rest body)
  `(mapcar '(lambda (x) (add-to-list ,target x)) ',body))

;;; (lazyload (triger-function　...) "filename" &rest body)
(defmacro lazyload (func lib &rest body)
  `(when (locate-library ,lib)
     ,@(mapcar (lambda (f) `(autoload ',f ,lib nil t)) func)
     (eval-after-load ,lib
       '(progn
          ,@body)) t))

;;; alias emacs = emacsclient
(req server
  (unless (server-running-p)
  (server-start)))

;;; package config
(req package
  (addlist 'package-archives
             ("marmalade" .
              "http://marmalade-repo.org/packages/")
             ("org" .
              "http://orgmode.org/elpa/")
             ("melpa" .
              "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;;; 外部へのIOはコストが高すぎる。　二度目のevalがされないように何かしらの対策をすることが必要である。
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
    cider
    ac-nrepl
    clojure-cheatsheet
    clojure-snippets

    rainbow-delimiters
    paredit
    smartparens

    color-theme
    color-theme-molokai

    lua-mode
    project-mode
    rainbow-mode
    undo-tree
    yasnippet
    auto-complete
    
    ;; helm packages
    helm
    helm-ag
    helm-ls-git
    
    ;; term
    multi-term

    tabbar

    multiple-cursors
    org

    ;; git
    magit
    git-gutter
    
    ))

;;; my/favorite-packagesからインストールする
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

(req color-theme
  (load-theme 'misterioso t))

;;; terminal
;;multi-tarm setting
(req multi-term
  (setq multi-term-program "/bin/zsh"))

(quote (setq eshell-command-aliases-list
             (append
              (list
               (list "ls" "ls -a"))
              eshell-command-aliases-list)))

;;; 詳細設定

(setq gc-cons-threshold (* 50 gc-cons-threshold))

(setq visible-bell t)
(setq ring-bell-function 'ignore)

;変更された時に自動でバッファ読み直し
(global-auto-revert-mode 1)

;;;parenthesis
(show-paren-mode t)

(req raibow-delimiters
     (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(req paredit
     (add-hook 'clojure-mode 'paredit-mode)
     (add-hook 'nrepl-mode-hook 'paredit-mode)
     (add-hook 'cider-repl-mode-hook 'paredit-mode))

(req smartparens
     (req smartparens-config))


(line-number-mode t)
(column-number-mode t)

;;; time setting
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t)

;;; C-kで行全体を削除
(setq kill-whole-line t)

;;; ウインドウの外見設定

(req tabbar
     (tabbar-mode 1))

;;; (set-background-color "Black")
;;; (set-foreground-color "White")
;;; (set-cursor-color "White")
(set-frame-parameter nil 'alpha 85)
(set-cursor-color "Blue")

(require 'whitespace)
;;; kill -9 fuckin em-space!!!!!!!!!!!!!!!!!!
(global-whitespace-mode 1)
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

;;; programming support
(req undo-tree
     (global-undo-tree-mode))

;;; C-c M-c connect C-c M-j jack-in C-c C-q end
(req cider
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu)
(global-auto-complete-mode t)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)

(req ac-nrepl
     (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
     (add-hook 'cider-mode-hook 'ac-nrepl-setup)
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'cider-repl-mode)))

(req yasnippet
     (yas-global-mode 1))

(req clojure-snippets
     (clojure-snippets-initialize))

(req flymake
     (add-hook 'java-mode-hook 'flymake-mode-on))

;;; test code
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))


;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; src package setting

;;; Bongo setting
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/Users/keihosoya/.emacs.d/src/bongo")
  (add-to-list 'exec-path "/Applications/VLC.app/Contents/MacOS")
  (autoload 'bongo "bongo"
    "Start Bongo by switching to a Bongo buffer." t)
  (setq bongo-enabled-backends '(vlc)))

;;; howm
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
  (setq howm-menu-lang 'ja)
  (require 'howm-mode))

;;; my function
(when (eq system-type 'darwin)
  (defun open-current-dir-with-finder  ()
    (interactive)
    (shell-command (concat "open ."))))

;;; keybind
(global-set-key "\M-p" 'scroll-down-command)
(global-set-key "\M-n" 'scroll-up-command)

;;; play-sound周りはわけ分からん
;;; どうも同期的な関数っぽいので使えないっぽい

;;; (play-sound :file sound-current-dic+hoge :volume 0.3)
;;; (setq sound-current-dic "/Users/keihosoya/.emacs.d/sound")
;;; (add-hook 'after-save-hook '(lambda  (play-sound-file "~/.emacs.d/wav/SAMUEAI.wav")))
;;; (copy-file "~/.emacs.d/init.el" "~/emacs-setup/init.el" t)
