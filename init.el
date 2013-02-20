;;�ǥե���ȥ�󥲡��������ܸ�
(set-language-environment 'Japanese)

(setq default-major-mode 'text-mode)

(set-keyboard-coding-system 'sjis)
(prefer-coding-system 'utf-8)
;����ѥ������cl���ѡ�
(require 'cl)
;package����
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)


;����б���Ĵ
(show-paren-mode t)
;����������֤���ȹԿ�ɽ��
(line-number-mode t)
(column-number-mode t)
;���֤򣲣���ɽ����ɽ��
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)

(setq read-file-name-completion-ignore-case t)

;;����������
(set-foreground-color "white")
(set-background-color "black")
(set-cursor-color "white")

