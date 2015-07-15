;; Packages
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ag
                      magit
                      enh-ruby-mode
                      helm-projectile
                      flx-ido
                      paredit-everywhere
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Projectile and helm
;;
(projectile-global-mode)


;; Keybindings
;;
(global-set-key (kbd "C-c h") 'helm-projectile)
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-current-buffer)

;; Misc
;;
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)


;; Ruby
;;
(defun my-enh-ruby-mode-defaults()
  (highlight-indentation-current-column-mode +1)
  (smartparens-strict-mode)
  (flycheck-mode -1)
  (setq enh-ruby-use-encoding-map nil)
  (setq enh-ruby-deep-indent-paren nil);; don't deep indent arrays and hashes
  (setq enh-ruby-comment-column 32)
  (enh-ruby-bounce-deep-indent t)
  (enh-ruby-deep-indent-paren t)
  (enh-ruby-hanging-brace-deep-indent-level 1)
  (enh-ruby-hanging-brace-indent-level 2)
  (enh-ruby-hanging-indent-level 2)
  (enh-ruby-hanging-paren-deep-indent-level 0)
  (enh-ruby-hanging-paren-indent-level 2)
  (enh-ruby-indent-level 2)
  )

(add-hook 'enh-ruby-mode-hook 'my-enh-ruby-mode-defaults)

(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rake\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
