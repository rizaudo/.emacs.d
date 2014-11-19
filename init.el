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
;;; need el-get <-これでは救われなそう
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
    ;; elisp-slime-nav

    ;; CommonLisp
    slime
    ac-slime
    
    ;; for Clojure settings
    clojure-mode
    clojure-project-mode
    clojurescript-mode
    cider
    ac-cider
    clojure-cheatsheet
    clojure-snippets
    slamhound
    typed-clojure-mode

    ;; OCaml setting
    tuareg
    merlin

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

    multiple-cursors
    org
    org-plus-contrib

    ido-ubiquitous

    ;; git
    magit
    git-gutter

    ;; shell
    exec-path-from-shell

    quickrun
    ssh-config-mode
    diminish
    powerline
    esup
    htmlize
    anything
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


;;; font
(when (and (eq system-type 'darwin) (not (eq window-system nil)))
  (set-face-attribute 'default nil :family "Inconsolata" :height 150)
  (set-fontset-font "fontset-default"
                    'japanese-jisx0208
                    '("Hiragino Maru Gothic ProN"))
  (set-fontset-font "fontset-default"
                    'katakana-jisx0201
                    '("Hiragino Maru Gothic ProN"))
  ;; japanese-jisx0213.2004-1
  (set-fontset-font "fontset-default"
                    'japanese-jisx0213.2004-1
                    '("Hiragino Maru Gothic ProN"))
  )

;;; emacs lisp setting
;;; M-. 定義元へ M-, 元居た位置へ C-c C-d C-d カーソル下のドキュメント表示

(autoload 'esup "esup"  nil t)

;;; tips

(setq ido-everywhere t)

;;; eldoc in M-:
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(req diminish
     ;; (diminish 'undo-tree-mode)
     ;; (diminish 'git-gutter-mode "GG")
     ;; (diminish 'eldoc-mode "doc")
     ;; (diminish 'paredit-mode "PE")
     ;; (diminish 'elisp-slime-nav-mode "SN")
     )

;;; Git
(lazyload (git-gutter) "git-gutter"
     (global-git-gutter-mode t)
     (setq git-gutter:update-hooks '(after-save-hook after-revert-hook)))

;;; terminal
;;; multi-tarm setting
(autoload 'multi-term  "multi-term" nil t)
(setq multi-term-program "/bin/zsh")

;;; eshell setting
(quote (setq eshell-command-aliases-list
             (append
              (list
               (list "ls" "ls -a"))
              eshell-command-aliases-list)))


(req raibow-delimiters
     (global-rainbow-delimiters-mode))

;; (req paredit
;;      (add-hook 'clojure-mode 'paredit-mode)
;;      ;; (add-hook 'nrepl-mode-hook 'paredit-mode)
;;      (add-hook 'cider-repl-mode-hook 'paredit-mode))

;; (defun paredit-wrap-round-from-behind ()
;;   (interactive)
;;   (forward-sexp -1)
;;   (paredit-wrap-round)
;;   (insert " ")
;;   (forward-char -1))
;; (define-key paredit-mode-map (kbd "M-)")
;;   'paredit-wrap-round-from-behind)

(req smartparens
     (req smartparens-config)
     (add-hook 'clojure-mode 'smartparens-mode)
     (add-hook 'cider-repl-mode-hook 'smartparens-mode))


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

(req color-theme
     (load-theme 'misterioso t))

(req powerline
     (powerline-center-theme))

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

;;; common lisp
(setq inferior-lisp-program "clisp")
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(req slime
     (slime-setup '(slime-repl slime-fancy slime-banner slime-indentation))
     (setq slime-contribs '(slime-fancy)) ; ALL IN!
     )

(req ac-slime
     (add-hook 'slime-mode-hook 'set-up-slime-ac)
     (add-hook 'slime-repl-mode-hook 'set-up-slime-ac))


;;; Clojure
(lazyload (clojure-mode) "clojure-mode"
          (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
          )

;;; C-c M-c connect C-c M-j jack-in C-c C-q end
;;; switch-to-buffer SPC after hidden buffers visible
(req cider
     ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (setq nrepl-hide-special-buffers t)
     (setq nrepl-buffer-name-show-port t))

(req clj-refactor
     (add-hook 'clojure-mode-hook (lambda ()
                                    (clj-refactor-mode 1)
                                    ;; insert keybinding setup here
                                    )))

(req auto-complete
     (req auto-complete-config)
     (ac-config-default)
     (setq ac-auto-show-menu 0.6)        ;タイマーに使われている
     (setq ac-dwim t)
     (setq ac-use-menu-map t)
     (setq ac-quick-help-delay 1)
     (setq ac-quick-help-height 60)
     (setq ac-disable-inline t)
     (setq ac-show-menu-immediately-on-auto-complete t)
     (setq ac-auto-start 2)
     (setq ac-candidate-menu-min 0)
     (global-auto-complete-mode t)
     ;; (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
     ;; (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
     ;; (add-hook 'clojure-nrepl-mode-hook 'ac-nrepl-setup)
     )

;; (req ac-nrepl
;;      (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;;      (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;;      (eval-after-load "auto-complete"
;;        '(add-to-list 'ac-modes 'cider-repl-mode)))

(req ac-cider
     (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
     (add-hook 'cider-mode-hook 'ac-cider-setup)
     (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
     (eval-after-load "auto-complete"
       '(add-to-list 'ac-modes 'cider-mode)))

;; (req yasnippet
;;      (yas-global-mode 0))

;; (req clojure-snippets
;;      (clojure-snippets-initialize))

(req flymake
     (add-hook 'java-mode-hook 'flymake-mode-on))

(req flycheck)

;;; OCaml support
(setq auto-mode-alist
      (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(setq inferior-caml-program "/usr/local/bin/ocaml")

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
     (req ox-bibtex)
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
(when (eq system-type 'darwin)
  ;; howm
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/howm/")
  (req howm
       (setq howm-menu-lang 'ja))
  ;; coq
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'load-path "/usr/local/opt/coq/lib/emacs/site-lisp/")
  (require 'coq)
  ;; proof general
  (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
  (defadvice coq-mode-config (after deactivate-holes-mode () activate)
    "Deactivate holes-mode when coq-mode is activated."
    (progn (holes-mode 0))
  )
  (add-hook 'proof-mode-hook
            '(lambda ()
               (define-key proof-mode-map (kbd "C-c C-j") 'proof-goto-point)))
  ;; agda
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "~/Library/Haskell/bin/")
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  (setq agda2-include-dirs
        (list "." (expand-file-name "~/AgdaLibrary/")))
  
  )

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

(require 'cl)  ; loop, delete-duplicates

(defun anything-font-families ()
  "Preconfigured `anything' for font family."
  (interactive)
  (flet ((anything-mp-highlight-match () nil))
    (anything-other-buffer
     '(anything-c-source-font-families)
     "*anything font families*")))

(defun anything-font-families-create-buffer ()
  (with-current-buffer
      (get-buffer-create "*Fonts*")
    (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
          do (insert
              (propertize (concat family "\n")
                          'font-lock-face
                          (list :family family :height 2.0 :weight 'bold))))
    (font-lock-mode 1)))

(defvar anything-c-source-font-families
  '((name . "Fonts")
    (init lambda ()
          (unless (anything-candidate-buffer)
            (save-window-excursion
              (anything-font-families-create-buffer))
            (anything-candidate-buffer
             (get-buffer "*Fonts*"))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" lambda
      (candidate)
      (kill-new candidate))
     ("Insert Name" lambda
      (candidate)
      (with-current-buffer anything-current-buffer
        (insert candidate))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eww-search-prefix "https://google.com/?q="))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "White"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "dark magenta"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "light green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan4")))))


;;; font-lock Experience
(defface extra-ws
  '((t (:background "blue" :foreground "black" :height 0.8))
    :group 'hi-lock-faces)
  "Used in text-mode and friends for exactly one space after a period.")

(font-lock-add-keywords 'agda2-mode
                        '(("[ℕτΓ]" . 'extra-ws)
  ;;   ("　" . 'trailing-whitespace)
  ;; ("[ \t]+$" . 'trailing-whitespace)
  ))

(defun help-me-rubikitch-san (package)
  (interactive "sPackageName:")
  (eww-browse-url (concat "http://rubikitch.com/tag/package:" package "/")))

