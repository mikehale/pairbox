;; Custom
;;
(custom-set-variables
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))))

;; Misc
;;
(setq load-prefer-newer t)    ; Please don't load outdated byte code
(fset 'yes-or-no-p 'y-or-n-p) ; short answers

;; Formatting
;;
(progn
  (setq-default tab-width 2
                indent-tabs-mode nil       ; always indent with spaces
                tab-stop-list (number-sequence 2 60 2)
                visible-bell t
                show-trailing-whitespace t ; show extra whitespace
                indicate-empty-lines t
                require-final-newline t))  ; ensure last line is a return

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

;; Bootstrap `use-package'
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)

;; Packages
;;

(use-package server
  :defer t
  :init (server-mode)
  :diminish server-buffer-clients
                                        ; export ALTERNATE_EDITOR=
                                        ; export EDITOR=emacsclient -t
  )

(use-package smart-mode-line
  :ensure t
  :config (progn
            (sml/setup)))

(use-package compile
  :config (setq compilation-scroll-output t))

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
  :ensure  t
  :defines (helm-M-x-fuzzy-match helm-semantic-fuzzy-match helm-imenu-fuzzy-match)
  :init    (progn
             (helm-mode t)
             (require 'helm-config)
             (setq helm-M-x-fuzzy-match t
                   helm-recentf-fuzzy-match t
                   helm-semantic-fuzzy-match t
                   helm-imenu-fuzzy-match t)
             (use-package semantic) ; this doesn't work?
             (use-package helm-projectile
               :ensure t
               ;; :pin    melpa-stable
               :bind   (
                        ("C-c h i" . helm-semantic-or-imenu)
                        ([remap switch-to-buffer] . helm-mini)
                        ([remap find-file] . helm-find-files)
                        )))
  :bind   (([remap execute-extended-command] . helm-M-x))
  :diminish helm-mode)

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil ; Shut up, please!
                ;; Revert Dired buffers, too
                global-auto-revert-non-file-buffers t))

(use-package recentf
  :init (progn (setq recentf-max-menu-items 25)
               (recentf-mode t)))

(use-package global-auto-revert-mode
  :init (global-auto-revert-mode 1))

(use-package abbrev
  :diminish abbrev-mode)

(use-package flycheck
  :ensure t
  :init   (progn
            (global-flycheck-mode)
            (use-package flycheck-pos-tip
              :disabled t
              :ensure   t
              :config   (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
            (use-package flycheck-color-mode-line
              :ensure t
              :init   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
  :config (progn
            (setq flycheck-highlighting-mode 'symbols)
            (set-face-attribute 'flycheck-error nil
                                :background "#660000"
                                :foreground nil)
            (set-face-attribute 'flycheck-warning nil
                                :background "#775500"
                                :foreground nil)
            (set-face-attribute 'flycheck-color-mode-line-error-face nil
                                :background "#660000"
                                :foreground nil)
            (set-face-attribute 'flycheck-color-mode-line-warning-face nil
                                :background "#775500"
                                :foreground nil)))

(use-package enh-ruby-mode
  :ensure      t
  :defines     (end-ruby-deep-indent-paren end-ruby-deep-arglist)
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
                 (setq ruby-deep-indent-paren nil
                       ruby-insert-encoding-magic-comment nil
                       end-ruby-deep-indent-paren nil
                       end-ruby-deep-arglist t
                       enh-ruby-hanging-brace-deep-indent-level 2
                       enh-ruby-hanging-brace-indent-level 2
                       enh-ruby-hanging-indent-level 2
                       enh-ruby-hanging-paren-deep-indent-level 2
                       enh-ruby-hanging-paren-indent-level 2
                       enh-ruby-indent-level 2)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (highlight-numbers-mode))

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
