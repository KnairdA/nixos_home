(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

(require 'package)
(package-initialize)
(eval-when-compile (require 'use-package))

(load-theme 'leuven)
(set-frame-font "Iosevka 11" nil t)
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(global-visual-line-mode t)

(use-package org)

(setq org-agenda-files (list "~/org/org.org"))

(use-package evil)
(use-package evil-leader)
(use-package evil-org)

(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-mode 1)

(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional todo))

(setq org-fontify-whole-heading-line t)
(setq org-adapt-indentation nil)
