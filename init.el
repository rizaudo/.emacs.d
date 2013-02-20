;;デフォルトランゲージ　日本語
(set-language-environment 'Japanese)

(setq default-major-mode 'text-mode)

(set-keyboard-coding-system 'sjis)
(prefer-coding-system 'utf-8)
;コンパイル時　cl使用　
(require 'cl)
;package設定
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)


;括弧対応強調
(show-paren-mode t)
;カーソル位置の列と行数表示
(line-number-mode t)
(column-number-mode t)
;時間を２４時表記で表示
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t)

;;黒字に白地
(set-foreground-color "white")
(set-background-color "black")
(set-cursor-color "white")

