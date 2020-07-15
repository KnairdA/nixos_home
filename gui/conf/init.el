(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq backup-directory-alist `((".*" . "~/.emacs.d/backup")))

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq user-full-name "Adrian Kummerlaender"
      user-mail-address "adrian@kummerlaender.eu")

(defun startup (frame)
  (select-frame frame)
  (set-frame-font "Iosevka 11" nil t)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'startup)
  (startup (selected-frame)))

(savehist-mode 1)
(setq savehist-additional-variables
      '(kill-ring search-ring))
(setq savehist-file "~/.emacs.d/savehist")

(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)
(setq fast-but-imprecise-scrolling t)

(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default python-indent-offset 4)

(setq password-cache t)
(setq password-cache-expiry 3600)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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

(load-library "custom-runtime-env")
(load-library "akr-theme")

(use-package evil-leader
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-leader/set-leader "SPC")
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'calendar-mode 'emacs))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dired eshell eww pdf))
  (evil-collection-init))

(use-package which-key
  :ensure t
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package hydra
  :ensure t)

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-visualizer-diff t)
  :config
  (evil-leader/set-key "hu" 'undo-tree-undo)
  (evil-leader/set-key "hr" 'undo-tree-redo)
  (evil-leader/set-key "hv" 'undo-tree-visualize))

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-vcs-max-length 24)
  (doom-modeline-minor-modes t))

(setq dired-listing-switches "-Bahl --group-directories-first")

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-subtree
  :ensure t
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "TAB") 'dired-subtree-toggle))

(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package org
  :ensure t
  :custom
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-files '("~/org"))
  (org-link-frame-setup '((file . find-file))) ; open links in same frame
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-src-window-setup 'current-window)
  (org-latex-preview-ltxpng-directory "~/.emacs.d/ltxpng/")
  :config
  (define-key org-mode-map (kbd "<C-tab>") nil)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1))))

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
    (sequence "EXAM(e)" "|" "DONE(d)")))

(setq org-capture-templates
  '(("t"
     "Todo item"
     entry
     (file org-default-notes-file)
     "* TODO %?\n%a")
    ("j"
     "Journal entry"
     entry
     (file org-default-notes-file)
     "* %U %^{Title}\n%?")
    ("s"
     "Quote selection"
     entry
     (file org-default-notes-file)
     "* %^{Description}\n%U\n#+BEGIN_QUOTE\n%i#+END_QUOTE")))

(evil-leader/set-key "oc" 'counsel-org-capture)
(evil-leader/set-key "on" 'counsel-org-agenda-headlines)

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package ob-async
  :ensure t)

(custom-theme-set-faces
   'user
   '(org-level-1 ((t (:family "Source Serif Pro"))))
   '(org-level-2 ((t (:family "Source Serif Pro"))))
   '(org-level-3 ((t (:family "Source Serif Pro"))))
   '(org-level-4 ((t (:family "Source Serif Pro"))))
   '(org-document-title ((t (:family "Source Serif Pro")))))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("●" "●" "⤷" "⤷"))
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package evil-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional todo)))

(defhydra hydra-org-mode ()
  "Org mode"
  ("e" org-babel-execute-buffer   "Execute buffer"   :column "Babel")
  ("t" org-babel-tangle           "Tangle"           :column "Babel")
  ("p" org-babel-expand-src-block "Tangle (preview)" :column "Babel")

  ("i" org-toggle-inline-images     "Toggle images"  :column "View")
  ("L" org-latex-preview            "Toggle LaTeX"   :column "View")
  ("n" org-toggle-narrow-to-subtree "Toggle narrow"  :column "View")
  ("l" org-toggle-link-display      "Toggle links"   :column "View")

  ("s" org-time-stamp-inactive  "Timestamp (inactive)" :column "Fragments")

  ("h" org-html-export-to-html  "HTML export" :column "Export")

  ("q" nil "Exit menu" :column "Other"))

(evil-define-key 'normal org-mode-map
  "J" 'org-next-visible-heading
  "K" 'org-previous-visible-heading
  "m" 'hydra-org-mode/body
  (kbd "<return>") 'org-open-at-point)

(evil-leader/set-key-for-mode 'org-mode
  "c" 'org-edit-src-code
  "lp" 'org-insert-link
  "g" 'org-goto)

(global-set-key (kbd "<print>") 'org-store-link)

(use-package ivy
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (evil-leader/set-key "x" 'helm-M-x)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (global-set-key (kbd "C-b") 'helm-mini)
  (setq helm-split-window-in-side-p       t
        helm-move-to-line-cycle-in-source t)
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :config
  (evil-leader/set-key "s" 'helm-swoop-without-pre-input))

(use-package helm-ls-git
  :ensure t
  :config
  (evil-leader/set-key "pf" 'helm-browse-project))

(global-set-key (kbd "<M-tab>")         'next-buffer)
(global-set-key (kbd "<M-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>")         'other-window)

(evil-define-key 'normal 'global
  "J" 'evil-forward-paragraph
  "K" 'evil-backward-paragraph
  "P" 'helm-show-kill-ring)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(define-key evil-normal-state-map (kbd "<backspace>") 'switch-to-last-buffer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python  . t)
   (shell   . t)
   (latex   . t)
   (C       . t)))

(setq org-confirm-babel-evaluate nil)

(evil-leader/set-key-for-mode 'org-mode
  "e" 'org-ctrl-c-ctrl-c)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-system 'helm))

(defhydra hydra-roam ()
  "Roam"
  ("s" org-roam                "Toggle sidebar" :column "View")
  ("r" org-roam-db-build-cache "Update cache" :column "View")

  ("f" org-roam-find-file "Find file" :column "Navigation")

  ("c" org-roam-insert     "Create link" :column "Links")
  ("y" org-roam-store-link "Store link"  :column "Links")
  ("p" org-insert-link     "Insert link" :column "Links")

  ("dy" org-roam-dailies-yesterday "Yesterday" :column "Dailies")
  ("dc" org-roam-dailies-today     "Today"     :column "Dailies")
  ("da" org-roam-dailies-date      "Arbitrary" :column "Dailies")
  ("dt" org-roam-dailies-tomorrow  "Tomorrow"  :column "Dailies")

  ("q" nil "Exit menu" :column "Other"))

(global-set-key (kbd "C-c r") 'hydra-roam/body)
(evil-leader/set-key "rh" 'hydra-roam/body)
(evil-leader/set-key "rf" 'org-roam-find-file)

(use-package org-noter
  :ensure t)

(use-package helm-org-rifle
  :ensure t
  :config
  (evil-define-key 'normal 'global
    (kbd "C-o")  'helm-org-rifle-org-directory
    (kbd "M-o")  'helm-org-rifle-current-buffer))

(use-package helm-ag
  :ensure t
  :config
  (evil-leader/set-key "pa" 'helm-projectile-ag))

(use-package magit
  :ensure t
  :config
  (evil-leader/set-key "pg" 'magit))

(use-package evil-magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-project-search-path '("~/projects"))
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (evil-leader/set-key "pp" 'helm-projectile-switch-project))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(defun get-related-files ()
  (let ((common-basename-files (seq-filter (lambda (file) (string= (file-name-sans-extension file) (file-name-base)))
																					 (directory-files "."))))
    (sort (seq-remove (lambda (file) (string= file (buffer-name)))
											common-basename-files)
					#'string-greaterp)))

(defun jump-to-related ()
  (interactive)
  (find-file (ivy-read "related:" (get-related-files))))

(defun jump-to-first-related ()
  (interactive)
  (find-file (car (get-related-files))))

(evil-define-key 'normal prog-mode-map
  (kbd "<tab>") 'jump-to-first-related
  (evil-leader/set-key "fr" 'jump-to-related))

(use-package counsel-etags
  :ensure t
  :config
  (setq tags-revert-without-query t)
  (setq large-file-warning-threshold nil)
  (evil-leader/set-key "d" 'counsel-etags-find-tag-at-point)
  (evil-leader/set-key "t" 'counsel-etags-list-tag-in-current-file))

(add-hook 'c-mode-common-hook 'hs-minor-mode t)
(add-hook 'c-mode-common-hook 'hs-hide-initial-comment-block t)

(use-package ace-jump-mode
  :ensure t
  :config
  (evil-leader/set-key "SPC" 'ace-jump-mode))

(setq org-roam-capture-templates
  '(("d" "default" plain (function org-roam-capture--get-point)
     "%?"
     :file-name "${slug}"
     :head "#+title: ${title}\n"
     :unnarrowed t)))

(use-package nix-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package literate-calc-mode
  :ensure t)

(use-package darkroom
  :ensure t
  :custom
  (darkroom-text-scale-increase 0)
  :bind
  ("<f12>" . darkroom-mode))

(use-package pdf-tools
  :defer t
  :mode "\\.pdf$"
  :config (pdf-tools-install))

(setq browse-url-browser-function 'eww-browse-url)

(add-hook 'eshell-mode-hook
  (lambda () 
    (define-key eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (completion-at-point)))))

(defadvice org-babel-tangle-single-block (around inhibit-redisplay activate protect compile)
  (let ((inhibit-redisplay t)
        (inhibit-message t))
    ad-do-it))

(defadvice org-babel-tangle (around time-it activate compile)
  (let ((time (current-time)))
    ad-do-it
    (message "org-tangle took %f sec" (float-time (time-subtract (current-time) time)))))

(let ((mu4e-config "~/.emacs.d/email.el"))
 (when (file-exists-p mu4e-config)
   (load-file mu4e-config)))
