;emacs-setup Fri Jul  5 13:20:35 JST 2013


(set-language-environment 'Japanese)
(setq default-major-mode 'text-mode)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;変更された時に自動でバッファ読み直し
(global-auto-revert-mode 1)

;use commonlisp
(with-no-warnings
  (require 'cl))


(require 'package)
(add-to-list 'package-archives
     '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'color-theme)
(load-theme 'misterioso t)

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
(set-frame-parameter nil 'alpha 80)


; kill -9 fuckin full-space!!!!!!!!!!!!!!!!!!
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
