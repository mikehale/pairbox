;; Custom
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     default))))

;; Misc
;;
(fset 'yes-or-no-p 'y-or-n-p) ; short answers
(setq compilation-scroll-output t)

;; Formatting
;;
(progn
  (setq-default tab-width 2)
  (setq-default indent-tabs-mode nil) ;; always indent with spaces
  (setq tab-stop-list (number-sequence 2 60 2))
  (setq visible-bell t))

;; Display
;;
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(dolist (mode '(column-number-mode))
  (when (fboundp mode) (funcall mode 1)))

;; Window management
;;
(progn
  (global-set-key (kbd "<up>") 'shrink-window)
  (global-set-key (kbd "<down>") 'enlarge-window)
  (global-set-key (kbd "<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "<right>") 'enlarge-window-horizontally))

;; Elpa
;;
(require 'package)
(dolist (s '(("melpa-stable" . "http://stable.melpa.org/packages/")
             ("melpa" . "http://melpa.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives s t))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install use-package
;;
(dolist (p '(use-package))
  (when (not (package-installed-p p))
    (package-install p)))
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)

;; Packages
;;
(use-package smart-mode-line
  :ensure t
  :config (sml/setup))

(use-package ag :ensure t)

(use-package magit
  :ensure t
  :bind   ("C-x g" . magit-status))

(use-package smartparens
  :ensure t
  :init   (progn
            (smartparens-global-mode)
            (show-smartparens-global-mode)
            (smartparens-global-strict-mode))
  :diminish smartparens-mode)

(use-package projectile
  :ensure t
  :init   (progn
            (projectile-global-mode))
  :diminish projectile-mode)

(use-package helm
  :ensure t
  :init   (progn
            (helm-mode t)
            (require 'helm-config)
            (setq helm-M-x-fuzzy-match t
                  helm-recentf-fuzzy-match t
                  helm-semantic-fuzzy-match t
                  helm-imenu-fuzzy-match t)
            (use-package semantic) ; this doesn't work?
            (use-package helm-projectile
              :ensure t
              :pin    melpa-stable
              :bind   (
                       ("C-c h i" . helm-semantic-or-imenu)
                       ("C-x b" . helm-mini)
                       ("C-x C-f" . helm-find-files)
                       )))
  :bind   ("M-x" . helm-M-x)
  :diminish helm-mode)

(use-package recentf
  :init (progn (setq recentf-max-menu-items 25)
               (recentf-mode t)))

(use-package abbrev
  :diminish abbrev-mode)

(use-package enh-ruby-mode
  :ensure      t
  :interpreter "ruby"
  :mode        (("\\.rb$" . enh-ruby-mode)
                ("Rakefile" . enh-ruby-mode)
                ("\\.rake" . enh-ruby-mode)
                ("\\.gemspec" . enh-ruby-mode)
                ("Gemfile" . enh-ruby-mode)
                ("Guardfile" . enh-ruby-mode)
                ("Vagrantfile" . enh-ruby-mode)
                ("\\.ru" . enh-ruby-mode))
  :init        (progn
                 (use-package rspec-mode :ensure t)
                 (use-package inf-ruby
                   :ensure t
                   :config (inf-ruby-switch-setup))
                 (use-package bundler :ensure t)
                 (setq ruby-deep-indent-paren nil)
                 (setq ruby-insert-encoding-magic-comment nil)
                 (setq end-ruby-deep-indent-paren nil)
                 (setq end-ruby-deep-arglist t)
                 (setq enh-ruby-hanging-brace-deep-indent-level 2)
                 (setq enh-ruby-hanging-brace-indent-level 2)
                 (setq enh-ruby-hanging-indent-level 2)
                 (setq enh-ruby-hanging-paren-deep-indent-level 2)
                 (setq enh-ruby-hanging-paren-indent-level 2)
                 (setq enh-ruby-indent-level 2)))

;; Keybindings
;;
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Functions
;;
(defun cleanup-buffer()
  "indent and clean buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun turn-on-linum-hl-mode-hook()
  (interactive)
  (hl-line-mode 1)
  (linum-mode 1))

(defun turn-off-linum-hl-mode-hook()
  (interactive)
  (hl-line-mode -1)
  (linum-mode -1))
