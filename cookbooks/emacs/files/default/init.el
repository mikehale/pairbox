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
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Projectile and helm
;;
(projectile-global-mode)
(global-set-key (kbd "C-c h") 'helm-projectile)
(projectile-get-project-directories)
