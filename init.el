;; bootstrap straight https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(add-hook 'org-mode-hook (lambda ()
  (setq org-agenda-files '("~/work/text/report.org"))
  (setq org-hide-leading-stars t)
  (setq org-hide-emphasis-markers t)
  (setq org-indent-indentation-per-level 8)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "DONE")))
  (local-unset-key (kbd "M-h"))    ; org-mode の M-h を利用しない。
  (local-unset-key (kbd "C-M-t"))
  (org-indent-mode t)              ; インデントをヘッダに合わせる。
  ))

(defun other-window-back ()
  (interactive)
  (other-window -1))

(keyboard-translate ?\C-h ?\C-?)                           ; C-h で delete を発行
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "M-t") 'other-window-back)
(global-set-key (kbd "C-M-t") 'other-frame)
(global-set-key (kbd "C-x SPC") 'cua-rectangle-mark-mode)  ; 矩形選択/入力
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)      ; 行末で折り返す <-> 折り返さない

(setq my-font (if (member "Ricty" (font-family-list)) "Ricty" "Monaco"))
(set-face-attribute 'default nil :family my-font :height 200)
(set-fontset-font t 'japanese-jisx0208 (font-spec :family my-font)) ; これがないと一部の漢字のフォントがおかしくなる

(add-hook 'occur-mode-hook (lambda ()
  (next-error-follow-minor-mode)
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
 ))

(setq ruby-insert-encoding-magic-comment nil)
(setq ruby-deep-indent-paren-style nil)
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Schemafile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\ruby$" . ruby-mode))

;; https://qiita.com/eggc/items/718dd41fa778b91f302e
(defalias '~ruby-syntax-propertize-function
  (syntax-propertize-rules
   ;; 文字列2重展開があるとシンタックスハイライトがおかしくなるので、 ruby-expression-expansion-re を修正したやつを追加
   ("\\(?:[^\\]\\|\\=\\)\\(\\\\\\\\\\)*\\(#{[^{^}]*#{[^}]*}[^}]*}\\)\\|\\(#\\({[^}\n\\\\]*\\(\\\\.[^}\n\\\\]*\\)*}\\|\\(\\$\\|@\\|@@\\)\\(\\w\\|_\\)+\\|\\$[^a-zA-Z \n]\\)\\)"
    (0 (ignore (ruby-syntax-propertize-expansion))))))
(defun ~ruby-fix-syntax-propertize ()
  (add-function :before (local 'syntax-propertize-function) '~ruby-syntax-propertize-function))
(add-hook 'ruby-mode-hook '~ruby-fix-syntax-propertize t)

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
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package eguchi-ken
  :straight (eguchi-ken :type git :host github :repo "eguchi-ken/eguchi-ken.el"))

(use-package rebecca-theme
  :config
  (load-theme 'rebecca t))

(use-package rubocop
  :config
  (add-hook 'ruby-mode-hook '(lambda()
  (rubocop-mode)
  (local-set-key (kbd "C-c , R") 'rubocop-autocorrect-current-file))))

(use-package recentf
  :config
  (setq recentf-max-saved-items 2000)
  (setq recentf-auto-cleanup 'never)
  (recentf-mode 1))

(use-package ivy
  :config
  (setq counsel-find-file-at-point t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (ivy-mode))

(use-package counsel
  :config
  (counsel-mode))

(use-package dired
  :ensure nil
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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-arguments ())
  (exec-path-from-shell-initialize))

(use-package coffee-mode
  :after (auto-complete)
  :config
  (add-hook 'coffee-mode-hook (lambda()
    (auto-complete-mode)
    (setq coffee-tab-width 2))
))

; https://github.com/senny/rbenv.el
(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (global-rbenv-mode))

(use-package wgrep
  :config
  (setf wgrep-enable-key "r")
  (setq wgrep-auto-save-buffer t)
  (setq my-ignore-directories '("node_modules" ".bundle" "yardoc" "coverage" "log" "tmp"))
  (setq my-ignore-files '("*.min.js" "*.log" "*bundle.js"))
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
  :after (projectile)
  :config
  (projectile-rails-global-mode))

(use-package dashboard
  :config
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

(use-package auto-complete)
(use-package auto-complete-config
  :ensure nil
  :config
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq ac-ignore-case nil) ; auto-complete で大文字小文字を区別する。
  (setq ac-auto-start 4)
)

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags))

(use-package forge
  :after magit
  :config
  ;; (remove-hook 'magit-status-sections-hook 'forge-insert-pullreqs)
  ;; (remove-hook 'magit-status-sections-hook 'forge-insert-issues)
  (setq forge-topic-list-columns
        '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
          ("Assignees" 15 t nil assignees nil)
          ("Title" 35 t nil title  nil))))

(use-package direnv
  :if (file-exists-p "/usr/local/bin/direnv")
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.04)
  (key-chord-mode 1)
  (key-chord-define-global "fd" 'find-file)
  (key-chord-define-global "gh" 'magit-status)
  (key-chord-define-global "sd" 'save-buffer)
  (key-chord-define-global "dj" 'dumb-jump-go)
  (key-chord-define-global "p@" 'dumb-jump-go-prompt)
  (key-chord-define-global "i9" 'insert-current-date)
  (key-chord-define-global "fp" 'file-full-path-org-link-to-clipboard)
  (key-chord-define-global "cy" 'string-inflection-ruby-style-cycle)
  (key-chord-define-global "o0" 'open-current-buffer-file)
)

(use-package ripgrep
  :if (file-exists-p "/usr/local/bin/rg"))

(use-package dumb-jump
  :config
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy)
  :bind (("M-g M-o" . dumb-jump-go-other-window)
         ("M-g M-j" . dumb-jump-go)
         ("M-g M-b" . dumb-jump-back)
         ("M-g M-i" . dumb-jump-go-prompt)
         ("M-g M-x" . dumb-jump-go-prefer-external)
         ("M-g M-z" . dumb-jump-go-prefer-external-other-window)))

(use-package spaceline)
(use-package spaceline-config
  :ensure nil
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-version-control-off)
)

(use-package paradox
 :custom
 (paradox-github-token t))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts[x]?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx"  . ".js[x]?\\'")))
  (add-hook 'web-mode-hook (lambda()
    (auto-complete-mode)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2))))

(use-package eww
  :config
  (setq eww-search-prefix "http://www.google.co.jp/search?q="))

(use-package js
  :config
  (setq js-indent-level 2))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package yasnippet
  :bind (("s-y" . yas-insert-snippet)))

(use-package ruby-electric
  :hook ((ruby-mode . ruby-electric-mode)))

(use-package rspec-mode
  :after (yasnippet)
  :config
  ;; @see https://github.com/pezra/rspec-mode#debugging
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (setq rspec-use-spring-when-possible nil)
  (rspec-install-snippets)
  (add-hook 'rspec-mode-hook #'yas-minor-mode))

(use-package yaml-mode)

(use-package go-mode)
(use-package slim-mode)
(use-package string-inflection)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-comment-dwim ((t (:inherit nil :foreground "#ccccff" :height 1.0))))
 '(org-level-1 ((t (:inherit nil :foreground "#ae81ff" :height 1.0))))
 '(org-level-2 ((t (:inherit nil :foreground "#ccccff" :height 1.0))))
 '(org-link ((t (:inherit nil :foreground "#ffff55" :height 1.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (wgrep ripgrep yaml-mode web-mode use-package twittering-mode tide spaceline smex slim-mode rubocop rspec-mode rjsx-mode restclient rebecca-theme rbenv projectile-rails paradox nvm key-chord jest idomenu ido-vertical-mode ido-completing-read+ gist forge exec-path-from-shell evil-string-inflection dumb-jump doom-modeline direnv dired-subtree dashboard csv-mode counsel coffee-mode auto-complete atom-one-dark-theme))))
