;;;; emacs-setup
;;;; for emacs 24.4(now)
;;;; 基本的に例外が無い限り最新版を追うこととします
;;;; 想定環境 Linux(Debian) Mac Windows
;;;; 使っているシステムの情報
;;;; mac:darwin windows:windows-nt

;;; Code:
(set-language-environment 'Japanese)
(setq major-mode 'text-mode)
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
  `(mapc '(lambda (x) (add-to-list ,target x)) ',body))

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

;;; 詳細設定 <- not req package

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq gc-cons-threshold (* 50 gc-cons-threshold))

(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t
      color-theme-is-global t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)

(setq read-file-name-completion-ignore-case t)
;;; C-kで行全体を削除
(setq kill-whole-line t)

(random t)

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
     (add-to-list 'grep-find-ignored-files "*.class")))

(blink-cursor-mode 0)

;;;変更された時に自動でバッファ読み直し
(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; auto-saveファイルを削除する
(require 'files)
;; (setq delete-auto-save-files t)

;;;parenthesis
(show-paren-mode t)


;;; package config
;;; TODO:バージョンごとのパッケージ管理
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
    ;; emacs lisp lib
    deferred
    dash
    elisp-slime-nav

    ;; CommonLisp
    slime
    ac-slime
    
    ;; for Clojure settings
    clojure-mode
    clojurescript-mode
    cider
    ac-cider
    clojure-cheatsheet
    clojure-snippets
    slamhound
    typed-clojure-mode
    flycheck-clojure
    latest-clojure-libraries

    ;; OCaml setting
    tuareg
    merlin

    ;; R
    ess

    rainbow-delimiters
    paredit
    smartparens

    ;; Color-Theme
    color-theme
    color-theme-molokai
    tron-theme
    org-beautify-theme

    lua-mode
    rainbow-mode
    undo-tree
    yasnippet
    auto-complete
    ac-c-headers
    ac-cider-compliment
    ac-helm
    ac-dabbrev
    
    flycheck
    ;; helm packages
    helm
    helm-ag
    helm-ls-git
    helm-descbinds
    helm-R
    helm-itunes
    ;; term
    multi-term

    markdown-mode
    multiple-cursors
    org
    org-plus-contrib
    org-octopress
    yatex

    ido-ubiquitous
    ido-vertical-mode
    smex
    idle-highlight-mode
    find-file-in-project
    scpaste
    direx
    popwin
    guide-key

    ;; git
    magit
    git-gutter

    ;; shell
    exec-path-from-shell

    quickrun
    diminish
    powerline
    esup
    htmlize
    anything
    command-log-mode
    use-package
    e2wm
    calfw
    apropos-fn+var
    emms
    emms-player-mpv
    emms-soundcloud
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
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

(autoload 'esup "esup"  nil t)

;;; tips
(req uniquify
     (setq uniquify-buffer-name-style'post-forward-angle-brackets))
(req ido)
(req ido-ubiquitous)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)
(setq ido-everywhere t
      ido-max-window-height 0.75)
(req smex
     (setq smex-save-file (concat user-emacs-directory ".smex-items"))
     (smex-initialize)
     (global-set-key (kbd "M-x") 'smex))

(req popwin
     (setq display-buffer-function 'popwin:display-buffer)
     (setq popwin:popup-window-position 'bottom))

(req saveplace
     (setq save-place t))

(require 'woman)
(setq woman-use-own-frame nil)

(req guide-key
     (setq guide-key/guide-key-sequence '("C-h" "C-x 4"))
     (setq guide-key/highlight-command-regexp "rectangle")
     (defun guide-key/my-hook-function-for-org-mode ()
       (guide-key/add-local-guide-key-sequence "C-c")
       (guide-key/add-local-guide-key-sequence "C-c C-x")
       (guide-key/add-local-highlight-command-regexp "org-"))
     (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
     (guide-key-mode 1))

;;; eldoc in M-:
(add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)

(req diminish
     ;; (diminish 'undo-tree-mode)
     ;; (diminish 'git-gutter-mode "GG")
     ;; (diminish 'eldoc-mode "doc")
     ;; (diminish 'paredit-mode "PE")
     ;; (diminish 'elisp-slime-nav-mode "SN")
     )

;; Helm 使わないけど一応
(req helm
     (req helm-R)
     (req helm-ls-git)
     (req helm-descbinds)
     (req helm-itunes))

;;; Git
(lazyload (git-gutter) "git-gutter"
     (global-git-gutter-mode t)
     (setq git-gutter:update-hooks '(after-save-hook after-revert-hook)))

;;; terminal
;;; multi-tarm setting
(lazyload (multi-term) "multi-term"
          (setq multi-term-program "/bin/zsh"))

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
;;      (add-hook 'cider-repl-mode-hook 'paredit-mode)
;;      (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
;;      (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp))


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
     (add-hook 'clojure-mode-hook 'smartparens-mode)
     (add-hook 'cider-repl-mode-hook 'smartparens-mode)
     (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
;; (define-key paredit-mode-map (kbd "C-j") nil)
)
(line-number-mode t)
(column-number-mode t)

;;; time setting
(require 'time)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

;;; ウインドウの外見設定

(require 'tron-theme)
;; (load-theme 'tron t)

(req powerline
     (powerline-center-theme))

(set-frame-parameter nil 'fullscreen 'maximized)
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
       (set-face-bold 'whitespace-space t)))
(req emms
         (require 'emms-setup)
         (emms-standard)
         (emms-default-players)
         (setq emms-player-list '(emms-player-mplayer)))

;;; programming support
(req undo-tree
     (global-undo-tree-mode))

;;; common lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/slime"))
(req slime
     (setq inferior-lisp-program "sbcl")
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
(lazyload (cider cider-jack-in) "cider"
     ;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
     (setq nrepl-hide-special-buffers t)
     (setq nrepl-buffer-name-show-port t))

(lazyload (clj-refactor-mode) "clj-refactor"
     (add-hook 'clojure-mode-hook (lambda ()
                                    (clj-refactor-mode 1)
                                    ;; insert keybinding setup here
                                    )))

(req auto-complete
     (req auto-complete-config)
     (ac-config-default)
     (setq ac-auto-show-menu t)        ;タイマーに使われている
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
     (add-hook 'cider-repl-mode-hook 'ac-cider-setup))

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
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;; Org-mode
(req ox-latex
     (req ox-bibtex)
     (req ox-publish)
     (req ox-jekyll)
     (setq org-publish-project-alist
      '(
        ("emacs-customize-101-jp"
         ;; Path to your org files.
         :base-directory "~/project/emacs-customize-101-jp/org/"
         :base-extension "org"
         ;; Path to your Jekyll project.
         :publishing-directory "~/project/emacs-customize-101-jp/_posts/"
         :recursive t
         :publishing-function org-jekyll-publish-to-html
         :headline-levels 4
         :html-extension "html"
         ;; :body-only t ;; Only export section between <body> </body>
         )))
     (setq org-latex-pdf-process
           '("latexmk %f"))
     (setq org-latex-with-hyperref nil)
     (setq org-latex-listings 'listings)
     (add-to-list 'org-latex-packages-alist '("" "listings"))
     (add-to-list 'org-latex-packages-alist '("" "color"))
     (custom-set-variables
      '(org-latex-listings-langs
        (quote
         ((emacs-lisp "Lisp")
          (lisp "Lisp")
          (clojure "Lisp")
          (c "C")
          (cc "C++")
          (fortran "fortran")
          (perl "Perl")
          (cperl "Perl")
          (python "Python")
          (ruby "Ruby")
          (html "HTML")
          (xml "XML")
          (tex "TeX")
          (latex "[LaTeX]TeX")
          (shell-script "bash")
          (gnuplot "Gnuplot")
          (ocaml "Caml")
          (caml "Caml")
          (sql "SQL")
          (sqlite "sql")
          (shell "bash")))))
     (add-to-list 'org-latex-classes
             '("thesis"
               "\\documentclass[a4j]{jarticle}
                [PACKAGES]
                [NO-DEFAULT-PACKAGES]
                \\usepackage[dvipdfmx]{graphicx}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; TeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;;; src package setting
(when (eq system-type 'darwin)
  ;; howm
  (add-to-list 'load-path "~/.emacs.d/howm-1.4.2/")
  (req howm
       (setq howm-menu-lang 'ja))
  ;; ;; YaTeX
  ;; (setq load-path (cons (expand-file-name "~/.emacs.d/yatex1.77/") load-path))
  ;; (setq auto-mode-alist
  ;;       (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  ;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  ;; coq
  (add-to-list 'exec-path "/usr/local/bin/")
  (add-to-list 'load-path "/usr/local/opt/coq/lib/emacs/site-lisp/")
  (require 'coq)
  ;; proof general
  (load-file "~/.emacs.d/ProofGeneral-4.2/generic/proof-site.el")
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
        (list "." (expand-file-name "~/AgdaLibrary/"))))

;;; my function
(when (eq system-type 'darwin)
  (defun open-current-dir-with-finder  ()
    (interactive)
    (shell-command (concat "open ."))))

;;; keybind
(global-set-key "\M-p" 'scroll-down-command)
(global-set-key "\M-n" 'scroll-up-command)
(global-set-key "\C-t" 'other-window)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

(global-set-key (kbd "C-x C-i") 'imenu)

(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))

(define-key 'help-command "a" 'apropos)
(global-set-key (kbd "C-c g") 'magit-status)

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

;; loop, delete-duplicates

(defun anything-font-families ()
  "Preconfigured `anything' for font family."
  (interactive)
  (cl-flet ((anything-mp-highlight-match () nil))
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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#15abc3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "nil" :family "Inconsolata"))))
 '(rainbow-delimiters-depth-1-face ((t (:background "blue3" :foreground "White" :inverse-video t :strike-through t :overline t :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "chartreuse3"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "alice blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "dodger blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "dark blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "forest green"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "cyan4"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :box (:line-width 2 :color "VioletRed4" :style released-button))))))


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


(provide 'init)
;;; init.el ends here
