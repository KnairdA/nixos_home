(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(require 'package)
(package-initialize)

(eval-when-compile
  (require 'use-package))

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
(global-eldoc-mode -1)
(global-visual-line-mode 1)

(setq warning-minimum-level :error)

(setq mouse-wheel-scroll-amount '(5))
(setq mouse-wheel-progressive-speed nil)
(setq fast-but-imprecise-scrolling t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(c-set-offset 'cpp-macro 0)
(setq-default python-indent-offset 4)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq whitespace-style '(face trailing tabs))

(setq dabbrev-case-fold-search nil)

(setq password-cache t)
(setq password-cache-expiry 3600)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(set-face-attribute 'default        nil :family "Iosevka")
(set-face-attribute 'fixed-pitch    nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "Source Sans Pro" :height 1.1)

(global-set-key (kbd "<M-tab>")         'next-buffer)
(global-set-key (kbd "<M-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>")         'other-window)

(load-library "custom-runtime-env")

(setq auto-mode-alist
      (append '(("\\.tikz\\'" . latex-mode))
              auto-mode-alist))

(use-package akr-theme
  :config
  (enable-theme 'akr))

(use-package gcmh ; noticeably reduces unwanted GC pauses when editing e.g. large fancy org files
  :ensure t
  :config
  (gcmh-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
  (evil-set-undo-system 'undo-tree)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (define-key evil-motion-state-map (kbd "C-b") nil))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-mode-list '(dired eshell eww pdf magit ediff pdf))
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
  (global-undo-tree-mode)
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

(use-package htmlize
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :custom
  (org-adapt-indentation nil)
  (org-startup-indented t)
  (org-startup-folded t)
  (org-hide-emphasis-markers t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-files '("~/org"))
  (org-refile-targets '((nil :maxlevel . 3)
                        (org-agenda-files :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-link-frame-setup '((file . find-file))) ; open links in same frame
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-src-window-setup 'current-window)
  (org-html-htmlize-output-type 'inline-css)
  (org-latex-preview-ltxpng-directory "~/.emacs.d/ltxpng/")
  (org-image-actual-width nil)
  (org-fontify-done-headline nil)
  :init
  (require 'org-protocol)
  (require 'ox-bibtex)
  :config
  (evil-leader/set-key "oy" 'org-store-link)
  (evil-leader/set-key "on" 'org-capture)
  (evil-leader/set-key "oa" 'org-agenda)
  (evil-leader/set-key-for-mode 'org-mode
    "c"  'org-edit-src-code
    "g"  'org-goto
    "e"  'org-ctrl-c-ctrl-c
    "lp" 'org-insert-link)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1))))

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
    (sequence "TALK(k)" "|" "DONE(d)")
    (sequence "MEET(e)" "|" "DONE(d)")
    (sequence "MAIL(m)" "|" "DONE(d)")
    (sequence "IDEA(i)" "TODO(t)" "|" "DONE(d)")))

(setq org-capture-templates
  '(("t" "Todo item" entry
     (file org-default-notes-file)
     "* TODO %?\n%a"
     :prepend t)
    ("c" "Calendar entry" entry
     (file org-default-notes-file)
     "* TODO %?\nSCHEDULED: %t"
     :prepend t)
    ("j" "Journal entry" entry
     (file org-default-notes-file)
     "* %U %^{Title}\n%?"
     :prepend t)
    ("s" "Quote selection" entry
     (file org-default-notes-file)
     "* %^{Description}\n%U\n#+BEGIN_QUOTE\n%i#+END_QUOTE"
     :prepend t)
    ("wq" "Website (Quote)"
     entry (file org-default-notes-file)
     "* %:description\n%:link %U\n#+BEGIN_QUOTE\n%:initial\n#+END_QUOTE")
    ("w" "Website" entry
     (file org-default-notes-file)
     "* %:description\n%:link %U"
     :immediate-finish t)))

(use-package calfw
  :ensure t
  :custom
  (calendar-week-start-day 1)
  (calendar-holidays nil)
  (cfw:org-face-agenda-item-foreground-color "#F2F2F2")
  (cfw:face-item-separator-color "#080808")
  (cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap))

(use-package calfw-org
  :ensure t
  :config
  (evil-leader/set-key "oc" 'cfw:open-org-calendar))

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package ob-async
  :ensure t)

(use-package org-ql
  :ensure t)

(custom-theme-set-faces
 'user
 '(org-level-1 ((t (:family "Source Serif Pro"))))
 '(org-level-2 ((t (:family "Source Serif Pro"))))
 '(org-level-3 ((t (:family "Source Serif Pro"))))
 '(org-level-4 ((t (:family "Source Serif Pro"))))
 '(org-document-title ((t (:family "Source Serif Pro"))))
 '(font-latex-sectioning-5-face ((t (:family "Source Serif Pro" :height 1.1))))

 ;; calfw
 '(cfw:face-title ((t (:inherit variable-pitch :family "Source Serif Pro" :foreground "#AADB0F" :height 2.0))))
 '(cfw:face-header ((t (:inherit fixed-pitch))))
 '(cfw:face-sunday ((t (:inherit fixed-pitch))))
 '(cfw:face-saturday ((t (:inherit fixed-pitch))))
 '(cfw:face-holiday ((t (:inherit fixed-pitch))))
 '(cfw:face-grid ((t (:inherit fixed-pitch))))
 '(cfw:face-default-content ((t (:inherit fixed-pitch))))
 '(cfw:face-periods ((t (:foreground "#909636"))))
 '(cfw:face-day-title ((t :background "#161616")))
 '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
 '(cfw:face-annotation ((t :foreground "#909636" :inherit cfw:face-day-title)))
 '(cfw:face-disable ((t :inherit cfw:face-day-title)))
 '(cfw:face-today-title ((t :foreground "#161616" :background "#AADB0F" :weight bold)))
 '(cfw:face-today ((t (:inherit fixed-pitch))))
 '(cfw:face-select ((t (:background "#F2F2F2" :foreground "#161616"))))
 '(cfw:face-toolbar ((t (:inherit fixed-pitch))))
 '(cfw:face-toolbar-button-off ((t (:inherit fixed-pitch))))
 '(cfw:face-toolbar-button-on  ((t (:inherit fixed-pitch)))))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list '("●" "●" "⤷" "⤷"))
  (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package ox-reveal
  :ensure t)

(use-package evil-org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional todo))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

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
  ("S" org-time-stamp           "Timestamp (active)"   :column "Fragments")

  ("h" org-html-export-to-html  "HTML export" :column "Export")

  ("q" nil "Exit menu" :column "Other"))

(evil-define-key 'normal org-mode-map
  "J" 'org-next-visible-heading
  "K" 'org-previous-visible-heading
  "m" 'hydra-org-mode/body
  (kbd "<return>") 'org-open-at-point)

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-b") 'helm-mini)
  (evil-leader/set-key "x" 'helm-M-x)
  (setq helm-split-window-in-side-p       t
        helm-move-to-line-cycle-in-source t
        helm-buffer-max-length            60)
  (evil-leader/set-key "d" 'helm-etags-select)
  :init
  (helm-mode 1))

(use-package helm-ls-git
  :ensure t
  :config
  (evil-leader/set-key "pf" 'helm-browse-project))

(use-package helm-swoop
  :ensure t
  :config
  (evil-leader/set-key "s" 'helm-swoop-without-pre-input))

(use-package org-ref
  :ensure t
  :custom
  (reftex-default-bibliography '("~/university/bib/lit.bib"))
  (org-ref-bibliography-notes "~/org/literature.org")
  (org-ref-default-bibliography '("~/university/bib/lit.bib"))
  (org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography '("~/university/bib/lit.bib"))
  (setq bibtex-completion-pdf-field "file"))

(use-package zotxt
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-zotxt-mode))

(evil-define-key 'normal 'global
  "J" 'evil-forward-paragraph
  "K" 'evil-backward-paragraph)

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(define-key evil-normal-state-map (kbd "<backspace>") 'switch-to-last-buffer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python  . t)
   (shell   . t)
   (latex   . t)
   (org     . t)
   (C       . t)))

(setq org-confirm-babel-evaluate nil)

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-system 'helm)
  (org-roam-rename-file-on-title-change nil))

(defhydra hydra-roam ()
  "Roam"
  ("s" org-roam                "Toggle sidebar" :column "View")
  ("r" org-roam-db-build-cache "Update cache" :column "View")

  ("f" org-roam-find-file "Find file" :column "Navigation")

  ("c" org-roam-insert     "Create link" :column "Links")
  ("y" org-store-link      "Store link"  :column "Links")
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
  (evil-leader/set-key "or" 'helm-org-rifle-org-directory)
  (evil-leader/set-key "ol" 'helm-org-rifle-current-buffer))

(use-package helm-org-ql
  :ensure t)

(use-package magit
  :ensure t
  :config
  (evil-leader/set-key "pg" 'magit))

(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/projects"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (evil-leader/set-key "pt" 'projectile-regenerate-tags)
  (evil-leader/set-key "pk" 'projectile-kill-buffers)
  :init
  (projectile-mode))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (evil-leader/set-key "ph" 'helm-projectile)
  (evil-leader/set-key "pp" 'helm-projectile-switch-project)
  (evil-leader/set-key "pb" 'helm-projectile-switch-to-buffer)
  (evil-leader/set-key "pa" 'helm-projectile-ag)
  :init
  (helm-projectile-on))

(defun get-related-files ()
  (let ((common-basename-files (seq-filter (lambda (file) (string= (file-name-sans-extension file) (file-name-base)))
																					 (directory-files "."))))
    (sort (seq-remove (lambda (file) (string= file (buffer-name)))
											common-basename-files)
					#'string-greaterp)))

(defun jump-to-related ()
  (interactive)
  (find-file (helm :sources
                   (helm-build-sync-source "Related files"
                     :candidates (get-related-files)
                     :fuzzy-match t))))

(defun jump-to-first-related ()
  (interactive)
  (find-file (car (get-related-files))))

(evil-define-key 'normal prog-mode-map
  (kbd "<tab>") 'jump-to-first-related
  (evil-leader/set-key "fr" 'jump-to-related))

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

(use-package auctex
  :defer t)

(use-package expand-region
  :ensure t
  :config
  (evil-leader/set-key "v" 'er/expand-region))

(use-package nix-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package literate-calc-mode
  :ensure t)

(use-package gnuplot
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
  :config
  (pdf-tools-install)
  :init
  (evil-collection-pdf-setup))

(use-package pocket-reader
  :ensure t
  :config
  (evil-set-initial-state 'pocket-reader-mode 'emacs))

(use-package edit-server
  :ensure t
  :init
  (edit-server-start))

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

(let ((patches "~/.emacs.d/patches.el"))
  (when (file-exists-p patches)
    (load-file patches)))
