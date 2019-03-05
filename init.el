(ido-mode t)
(setq ido-enable-flex-matching t)
(menu-bar-mode -1)
; (tool-bar-mode -1)
; (scroll-bar-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(global-hl-line-mode)
(global-auto-revert-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq require-final-newline t)
(setq visible-bell t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq backup-directory-alist `((".*". ,temporary-file-directory)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(add-hook 'org-mode-hook (lambda ()
  (setq org-hide-leading-stars t)
  (local-unset-key (kbd "M-h"))    ; org-mode の M-h を利用しない。
  (org-indent-mode t)              ; インデントをヘッダに合わせる。
  ))

(keyboard-translate ?\C-h ?\C-?)                           ; C-h で delete を発行
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)  ; 矩形選択/入力
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)      ; 行末で折り返す <-> 折り返さない

(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-refresh-contents)
(defvar my/favorite-packages '(magit key-chord rebecca-theme))
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(setq key-chord-two-keys-delay 0.04)
(key-chord-mode 1)
(key-chord-define-global "fd" 'find-file)
(key-chord-define-global "gh" 'magit-status)

(load-theme 'rebecca t)

;; モードラインのカスタマイズ
;; https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
(require 'cl-lib)
(set 'eol-mnemonic-dos "(CRLF)")
(set 'eol-mnemonic-unix "(LF)")
(set 'eol-mnemonic-mac "(CR)")
(set 'eol-mnemonic-undecided "(?)")
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))
(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))
(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))
