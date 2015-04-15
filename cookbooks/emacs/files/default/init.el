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
(projectile-get-project-directories)


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
