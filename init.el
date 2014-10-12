;;;; emacs-setup Sat Nov  9 12:29:18 2013
;;;; for emacs 24.3(now)
;;;; 基本的に例外が無い限り最新版を追うこととします
;;;; 想定環境 Linux(Debian) Mac Windows
;;;; 外部に投げる場合や環境依存の物を使う場合、system-typeを見てunless
;;;; かwhenしましょう。 unlessよりは(when (not (eq system-type 'hoge)))
;;;; のほうが可読性良いかもしれない。
;;;; 使っているシステムの情報
;;;; mac:darwin windows:windows-nt
;;; TODO
;;; to autoload: git-gutter,cider,yasnippet,

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

;;; need fix
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
(when (eq system-type 'gnu/linux)
  (req server
       (unless (server-running-p)
         (server-start))))

;;; 詳細設定

(setq gc-cons-threshold (* 50 gc-cons-threshold))

(setq visible-bell t)
(setq ring-bell-function 'ignore)

;;;変更された時に自動でバッファ読み直し
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; auto-saveファイルを削除する
(setq delete-auto-save-file t)

;;;parenthesis
(show-paren-mode t)


;;; package config

;;; もっと良いパッケージ管理を使いたい
;;; need el-get
(req package
  (addlist 'package-archives
             ("marmalade" .
              "http://marmalade-repo.org/packages/")
             ("org" .
              "http://orgmode.org/elpa/")
             ("melpa" .
              "http://melpa.milkbox.net/packages/"))
  (package-initialize))

;;; #PACLIST#
(defvar my/favorite-packages
  '(
    starter-kit
    starter-kit-lisp
    starter-kit-bindings
    starter-kit-eshell

    ;; emacs lisp lib
    deferred
    dash
    
    ;; for Clojure settings
    clojure-mode
    clojure-project-mode
    clojurescript-mode
    nrepl
    cider
    ac-nrepl
    clojure-cheatsheet
    clojure-snippets
    slamhound
    typed-clojure-mode

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
    ac-c-headers
    ac-cider-compliment
    ac-helm
    
    flycheck
    ;; helm packages
    helm
    helm-ag
    helm-ls-git
    helm-descbinds
    ;; term
    multi-term

    tabbar

    multiple-cursors
    org
    org-plus-contrib
    

    ;; git
    magit
    git-gutter

    esup
    htmlize
    ))

;;; org 2 bibtex でbibtex2htmlを要求してた

;(package-refresh-contents)

;;; my/favorite-packagesからインストールする
(defun  set-pac ()
  "my package install command"
  (interactive)
  (package-refresh-contents)
  (dolist (package my/favorite-packages)
    (when (not (package-installed-p package))
      (package-install package))))


(when (and (eq system-type 'darwin) (not (eq window-system nil)))
  (set-face-attribute 'default nil :family "Ricty Diminished" :height 150)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("Ricty Diminished"))
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("Ricty Diminished"))
  ;; (set-fontset-font "fontset-default"
  ;;                   'japanese-jisx0208
  ;;                   '("Hiragino Maru Gothic ProN"))
  ;; (set-fontset-font "fontset-default"
  ;;                   'katakana-jisx0201
  ;;                   '("Hiragino Maru Gothic ProN"))
  )


;;; pref setting  by Dr.Yas
(autoload 'gomoku 			"gomoku" "" t)
(autoload 'calendar 			"calendar" "" t)
(autoload 'find-elisp-file 		"menu" "" t)
(autoload 'find-file-using-menu 	"menu" "" t)
(autoload 'electric-buffer-list 	"ebuff-menu" "" t)
(autoload 'electric-command-history 	"echistory" "" t)
(autoload 'run-dbx 			"dbx" "" t)
(autoload 'gnus				"gnus" "" t)
(autoload 'another-shell 		"a-sh" "run another shell." t)
(autoload 'new-lisp-complete-symbol 	"lisp-complete" "" t)
(autoload 'kcl-help 			"kcl-help" "" t)
(autoload 'kcl-help* 			"kcl-help" "" t)
(autoload 'insert-reference-char-region "inseart-ref" "" t)
(autoload 'hide-ifdef-mode		"hideif" "" t)
(autoload 'c++-mode			"c++" "" t)
(autoload 'yacc-mode 			"yacc-mode" "" t)
(autoload 'vman 			"vman" "" t)
(autoload 'klpr-buffer 			"klp" "" t)
(autoload 'klpr-region 			"klp" "" t)
(autoload 'read-file-compressed		"z-mode" "" t)
(autoload 'after-find-file-compressed	"z-mode" "" t)
(autoload 'dired-find-file-compressed	"z-mode" "" t)
(autoload 'monkey-directory		"monkey" "" t)
(autoload 'browse-yank			"byank" "" t)
(autoload 'electric-shell-history	"eshistory" "" t)
(autoload 'lisp-hide-sexp		"lisp-hide" "" t)
;(autoload 'vn 				"vn" "" t)
;(autoload 'template-mode 		"template" "" t)
(autoload 'run-idraw "idraw-mode" nil t) ;kanji input for idraw
(autoload 'webster   		      	"webster" "" t)
(autoload 'webster-spell	      	"webster" "" t)
(autoload 'html-helper-mode "html-helper-mode" "HTML Helper Mode" t)
(autoload 'html-mode "html-mode" "HTML Mode" t)

;;; emacs lisp setting
;;; M-. 定義元へ M-, 元居た位置へ C-c C-d C-d カーソル下のドキュメント表示

(req esup
     (autoload 'esup "esup" "Emacs Start Up Profiler." nil))

(req color-theme
     (load-theme 'misterioso t))



;;; Git
(lazyload (git-gutter) "git-gutter"
     (global-git-gutter-mode t)
     (setq git-gutter:update-hooks '(after-save-hook after-revert-hook)))

;;; terminal
;;; multi-tarm setting
(req multi-term
  (setq multi-term-program "/bin/zsh"))

;;; eshell setting

(quote (setq eshell-command-aliases-list
             (append
              (list
               (list "ls" "ls -a"))
              eshell-command-aliases-list)))


(req raibow-delimiters
     (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(req paredit
     (add-hook 'clojure-mode 'paredit-mode)
     (add-hook 'nrepl-mode-hook 'paredit-mode)
     (add-hook 'cider-repl-mode-hook 'paredit-mode))

(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))
(define-key paredit-mode-map (kbd "M-)")
  'paredit-wrap-round-from-behind)

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

(req whitespace
;;; kill -9 fuckin em-space!!!!!!!!!!!!!!!!!!
     (global-whitespace-mode 1)
     (setq whitespace-style '(face tabs tab-mark spaces space-mark))
     (setq whitespace-display-mappings
           ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
           '(
             (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
             (newline-mark 10 [182 10]) ; 10 LINE FEED
             ))
     (when (not (eq window-system nil))
       (set-face-foreground 'whitespace-tab "#adff2f")
       (set-face-background 'whitespace-tab 'nil)
       (set-face-underline  'whitespace-tab t)
       (set-face-foreground 'whitespace-space "#7cfc00")
       (set-face-background 'whitespace-space 'nil)
       (set-face-bold-p 'whitespace-space t)))

;;; programming support
(req undo-tree
     (global-undo-tree-mode))

;;; C-c M-c connect C-c M-j jack-in C-c C-q end
;;; switch-to-buffer SPC after hidden buffers visible
(req cider
     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (setq nrepl-hide-special-buffers t)
     (setq nrepl-buffer-name-show-port t))

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
     (yas-global-mode 0))

(req clojure-snippets
     (clojure-snippets-initialize))

(req flymake
     (add-hook 'java-mode-hook 'flymake-mode-on))

(req flycheck)


;;; test code
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.java$" my-java-flymake-init flymake-simple-cleanup))


;;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; Org-mode
(req ox-latex
     (require 'ox-bibtex)
     (setq org-latex-pdf-process
           '("latexmk %f"))
     (setq org-latex-with-hyperref nil)
     (add-to-list 'org-latex-classes
             '("thesis"
               "\\documentclass[a4j]{jarticle}
                [NO-PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage[dvipdfmx]{graphicx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


;;; src package setting
;;; howm
(when (eq system-type 'darwin)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
  (setq howm-menu-lang 'ja)
  (require 'howm))

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

(setenv "TMPDIR" ".")
(setq temporary-file-directory "/Users/keihosoya/tmp")
(setq debug-on-error t
      debug-on-signal nil)






