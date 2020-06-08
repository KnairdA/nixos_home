(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq backup-directory-alist `((".*" . "~/.emacs.d/backup")))

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(setq user-full-name "Adrian Kummerlaender"
      user-mail-address "adrian@kummerlaender.eu")

(set-frame-font "Iosevka 11" nil t)
(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(global-visual-line-mode t)

(add-hook 'prog-mode-hook 'linum-mode)

(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(set-face-attribute 'default        nil :family "Iosevka")
(set-face-attribute 'fixed-pitch    nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.1)

(load-library "akr-theme")

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-vcs-max-length 24))

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode 1)
  (evil-leader/set-leader ","))

(use-package darkroom
  :ensure t
  :config
  (setq darkroom-text-scale-increase 0))

(use-package pdf-tools
  :defer t
  :mode "\\.pdf$"
  :config (pdf-tools-install))

(setq browse-url-browser-function 'eww-browse-url) 

(use-package nix-buffer
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-agenda-files '("~/org"))
  (setq org-link-frame-setup '((file . find-file))) ; open links in same frame
  (add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1))))

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
    (sequence "EXAM(e)" "|" "DONE(d)")))

(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
   'user
   '(org-level-1 ((t (:family "Source Serif Pro" :weight regular :height 1.4 ))))
   '(org-level-2 ((t (:family "Source Serif Pro" :weight regular :height 1.3))))
   '(org-level-3 ((t (:family "Source Serif Pro" :weight regular :height 1.1))))
   '(org-level-4 ((t (:family "Source Serif Pro" :weight regular :height 1.0))))
   '(org-document-title ((t (:family "Source Serif Pro" :height 2.0)))))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("●"))
  ;(setq org-bullets-bullet-list '("●" "◉" "◎"))
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-directory "~/org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-org-mode-title-prefix t)
  (setq deft-file-naming-rules
    '((noslash . "_")
      (nospace . "_")
      (case-fn . downcase))))

(use-package evil-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional todo)))

(evil-define-key 'normal org-mode-map
  "J" 'org-next-visible-heading
  "K" 'org-previous-visible-heading
  (kbd "<return>") 'org-open-at-point)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (ivy-mode 1))

(use-package swiper :ensure t)
(use-package counsel :ensure t)

(defun go-to-deft ()
  (interactive)
  (deft)
  (evil-insert-state))

(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
(define-key evil-normal-state-map (kbd "C-f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "C-n") 'go-to-deft)
(define-key evil-normal-state-map (kbd "M-<tab>") 'next-buffer)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(define-key evil-normal-state-map (kbd "<backspace>") 'switch-to-last-buffer)

(evil-define-key 'normal eww-mode-map
  "o" 'eww
  "H" 'eww-back-url
  "L" 'eww-forward-url)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python  . t)
   (shell   . t)
   (C       . t)))

(setq org-confirm-babel-evaluate nil)

(defun org-deft-insert-link (file)
  (interactive (list
    (completing-read "Note: "
      (deft-find-all-files))))
  (org-insert-link nil (concat "file:" file) (file-name-base file)))

(evil-leader/set-key
  "e" 'org-babel-execute-src-block
  "l" 'org-deft-insert-link)

(use-package org-noter
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package fzf
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-p") 'fzf-git-files))

(use-package modern-cpp-font-lock
  :ensure t
  :config
  (modern-c++-font-lock-global-mode t))

(use-package counsel-etags
  :ensure t
  :config
  (evil-leader/set-key
    "d" 'counsel-etags-find-tag-at-point))

(use-package ag
  :ensure t)

(defun hide-banner ()
  (save-excursion
    (let* ((start (progn (beginning-of-buffer) (point)))
           (end (progn (forward-comment (buffer-size)) (point)))
           (header-comment-hider (make-overlay start end)))
      (overlay-put header-comment-hider 'invisible t))))

(defun unhide-banner ()
  (interactive)
  (delete-overlay 'header-comment-hider))

(add-hook  'c-mode-common-hook 'hide-banner)
