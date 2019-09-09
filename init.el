(setenv "SPEC_OPTS" "--format documentation --fail-fast")
(setenv "RSPEC_RETRY_RETRY_COUNT" "1")

(menu-bar-mode -1)
(show-paren-mode 1)
(column-number-mode t)
(global-hl-line-mode)
(global-auto-revert-mode 1)
(electric-pair-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)
(setq save-interprogram-paste-before-kill t)
(setq require-final-newline t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq backup-directory-alist `((".*". ,temporary-file-directory)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; 長い行（とくに整形されてないjson等の表示）の処理が非常に重いためそれを軽減する
; https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow/601
(setq-default bidi-display-reordering nil)

; 日本語入力時のちらつきを防止する
;; http://hylom.net/emacs-25.1-ime-flicker-problem
(setq redisplay-dont-pause nil)

(add-hook 'org-mode-hook (lambda ()
  (setq org-hide-leading-stars t)
  (setq org-indent-indentation-per-level 8)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (local-unset-key (kbd "M-h"))    ; org-mode の M-h を利用しない。
  (org-indent-mode t)              ; インデントをヘッダに合わせる。
  ))

(defun other-window-back ()
  (interactive)
  (other-window -1)
  )

(keyboard-translate ?\C-h ?\C-?)                           ; C-h で delete を発行
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "M-t") 'other-window-back)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)  ; 矩形選択/入力
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)      ; 行末で折り返す <-> 折り返さない

; (font-family-list) でフォント一覧を見れるのでそこから選ぶ
(set-face-attribute 'default nil :family "Ricty" :height 170)
(set-fontset-font t 'japanese-jisx0208 (font-spec :family "Ricty")) ; これがないと一部の漢字のフォントがおかしくなる
(fset 'yes-or-no-p 'y-or-n-p) ; yes or no の質問を y, n で答えられるようにする

(add-hook 'occur-mode-hook (lambda ()
  (next-error-follow-minor-mode)
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
 ))

(package-initialize)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(defvar my/favorite-packages
  '(auto-complete rubocop exec-path-from-shell rspec-mode direnv
    rbenv yasnippet dumb-jump rg dired-subtree ivy counsel
    slim-mode string-inflection
    coffee-mode wgrep dashboard paradox web-mode
    projectile projectile-rails spaceline
    use-package magit key-chord rebecca-theme wdired))
(dolist (package my/favorite-packages)
  (unless (package-installed-p package)
    (package-install package)))

(load-theme 'rebecca t)

(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-deep-indent-paren-style nil)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))

(add-hook 'ruby-mode-hook '(lambda()
  (rubocop-mode)
  (local-set-key (kbd "C-c , R") 'rubocop-autocorrect-current-file)
))

;; @see https://github.com/pezra/rspec-mode#debugging
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
;; @see https://github.com/pezra/rspec-mode#auto-scrolling
(setq compilation-scroll-output t)

(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt))

(defun insert-current-date (&optional diff)
  "日にちをカレントバッファに出力します"
  (interactive "P")
    (insert
     (shell-command-to-string
      (format
       "echo -n $(LANG=ja_JP date -v-%dd +'%%Y/%%m/%%d (%%a)')"
       (or diff 0)))))

;; 今開いているファイルのフルパスを得る。dired バッファは NG
(defun file-full-path ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

; for emacs cocoa
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-set-key (kbd "s-f") 'find-file)                    ; C-x C-f
(global-set-key (kbd "s-b") 'switch-to-buffer)             ; C-x b
(global-set-key (kbd "s-0") 'delete-window)                ; C-x 0
(global-set-key (kbd "s-1") 'delete-other-windows)         ; C-x 1
(global-set-key (kbd "s-2") 'split-window-vertically)      ; C-x 2
(global-set-key (kbd "s-3") 'split-window-horizontally)    ; C-x 3
(global-set-key (kbd "s-t") 'make-frame-command)           ; C-x 5 2
(global-set-key (kbd "s-D") 'split-window-vertically)      ; iterm と同じ
(global-set-key (kbd "s-d") 'split-window-horizontally)    ; iterm と同じ
(global-set-key (kbd "s-q") 'version)                      ; 誤操作防止用
(global-set-key (kbd "M-i") 'imenu)

(use-package recentf
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (ivy-mode)
  (counsel-mode))

(use-package dired
  :config
  (setq dired-use-ls-dired nil)
  (setq dired-dwim-target t) ; 2個のdiredバッファがある時、コピー/移動先のパスを他方のバッファにする
  (unbind-key "C-t" dired-mode-map)
  :bind (:map dired-mode-map
              ("j" . 'dired-next-line)
              ("k" . 'dired-previous-line)
              ("<tab>" . 'dired-subtree-insert)
              ("<backtab>" . 'dired-subtree-remove)
              ("h" . 'dired-subtree-remove)
              ("r" . 'wdired-change-to-wdired-mode)))
(use-package dired-subtree)
(use-package wdired)

(use-package coffee-mode
  :config
  (add-hook 'coffee-mode-hook (lambda()
    (auto-complete-mode)
    (setq coffee-tab-width 2))
))

; https://github.com/senny/rbenv.el
(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode)
)

(use-package wgrep
  :config
  (setf wgrep-enable-key "r")
  (setq wgrep-auto-save-buffer t)
  (setq my-ignore-directories '("node_modules" ".bundle" "yardoc" "coverage" "log" "tmp"))
  (setq my-ignore-files '("*.min.js" "*.log"))
  (setq grep-find-ignored-directories (append grep-find-ignored-directories my-ignore-directories))
  (setq grep-find-ignored-files (append grep-find-ignored-files my-ignore-files))
)

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/"))
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  ("s-p" . projectile-command-map))

(use-package projectile-rails
  :config
  (projectile-rails-global-mode)
  )

(use-package dashboard
  :config
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

(use-package auto-complete)
(use-package auto-complete-config
  :config
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq ac-ignore-case nil) ; auto-complete で大文字小文字を区別する。
  (setq ac-auto-start 4)
)

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package forge
  :after magit)

(use-package direnv
 :config
 (setq direnv-always-show-summary nil)
 (if (direnv--detect) (direnv-mode)))

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode 1)
  (key-chord-define-global "fd" 'find-file)
  (key-chord-define-global "gh" 'magit-status)
  (key-chord-define-global "sd" 'save-buffer)
  (key-chord-define-global "bm" 'bookmark-jump)
  (key-chord-define-global "dj" 'dumb-jump-go)
  (key-chord-define-global "p@" 'dumb-jump-go-prompt)
  (key-chord-define-global "y7" 'yas-insert-snippet)
  (key-chord-define-global "id" 'insert-current-date)
  (key-chord-define-global "fp" 'file-full-path)
  (key-chord-define-global "cy" 'string-inflection-ruby-style-cycle)
)

(use-package rg)
(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy)
)

(use-package spaceline-config
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-off)
)

(use-package paradox
 :ensure t
 :custom
 (paradox-github-token t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx"  . ".js[x]?\\'")))
  (add-hook 'web-mode-hook (lambda()
    (auto-complete-mode)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2))))

; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit nil :foreground "#ae81ff" :height 1.0))))
 '(org-level-2 ((t (:inherit nil :foreground "#ccccff" :height 1.0)))))
