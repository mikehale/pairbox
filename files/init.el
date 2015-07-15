;; Misc
;;

(fset 'yes-or-no-p 'y-or-n-p) ; short answers

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
  :config (progn
            (setq sml/no-confirm-load-theme t)
            (sml/setup)))

(defun turn-on-linum-hl-mode-hook()
  (interactive)
  (hl-line-mode 1)
  (linum-mode 1))
;; (turn-on-linum-hl-mode-hook)

(use-package ag :ensure t)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package flx-ido :ensure t)

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
            (projectile-global-mode)))

(use-package helm
  :ensure t
  :init   (progn
            (use-package helm-projectile
              :ensure t
              :pin    melpa-stable
              :bind   ("C-c h" . helm-projectile))))

(use-package recentf
  :init (progn (setq recentf-max-menu-items 25
                     recentf-exclude        '("/tmp" "/ssh:" "\\ido.last" "recentf"))
               (recentf-mode t))
  :bind ("C-x C-r" . helm-recentf))

(use-package enh-ruby-mode
  :ensure t
  :init   (progn
            (use-package rspec-mode :ensure t)
            (use-package inf-ruby :ensure t)
            (use-package bundler :ensure t)

            ;; file type associations
            (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
            (add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))

            ;; configure ruby-mode
            (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

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

(defun cleanup-buffer()
  "indent and clean buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
