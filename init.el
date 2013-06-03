
(set-language-environment 'Japanese)

(setq default-major-mode 'text-mode)

(set-keyboard-coding-system 'euc-jp)
(prefer-coding-system 'euc-jp)
¡¡
(require 'cl)

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)



(show-paren-mode t)

(line-number-mode t)
(column-number-mode t)

(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t)



(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "White")
(set-frame-parameter nil 'alpha 90)

