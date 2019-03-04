(ido-mode t)
(setq ido-enable-flex-matching t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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

(package-initialize)
(package-refresh-contents)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar my/favorite-packages
  '(
    magit
    ))
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))
