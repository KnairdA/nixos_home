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
  (tool-bar-mode -1)
  (global-visual-line-mode t))

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
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "s" 'evil-ex-nohighlight)
  (global-evil-leader-mode))

(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t)

(use-package minions
  :ensure t
  :config
  (minions-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-vcs-max-length 24)
  (setq doom-modeline-minor-modes t))

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

(use-package hydra
  :ensure t)

(use-package darkroom
  :ensure t
  :config
  (setq darkroom-text-scale-increase 0))

(use-package pdf-tools
  :defer t
  :mode "\\.pdf$"
  :config (pdf-tools-install))

(setq browse-url-browser-function 'eww-browse-url)

(use-package direnv
  :ensure t
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package org
  :ensure t
  :config
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-agenda-files '("~/org"))
  (setq org-link-frame-setup '((file . find-file))) ; open links in same frame
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-src-window-setup 'current-window)
  (define-key org-mode-map (kbd "<C-tab>") nil)
  (add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1))))

(setq org-todo-keywords
  '((sequence "TODO(t)" "|" "DONE(d)")
    (sequence "EXAM(e)" "|" "DONE(d)")))

(defun capture-new-note ()
  (interactive)
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s.org" name) "~/org/")))

(setq org-capture-templates
  '(("t"
     "Todo item"
     entry
     (file org-default-notes-file)
     "* TODO %?\n%a")
    ("n"
     "New zettel"
     plain
     (file capture-new-note)
     "#+TITLE: %^{Title}\n\n%?")
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

(global-set-key (kbd "C-c c") 'counsel-org-capture)
(global-set-key (kbd "C-c o") 'counsel-org-agenda-headlines)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltxpng/")
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

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
  ("l" org-latex-preview            "Toggle LaTeX"   :column "View")
  ("n" org-toggle-narrow-to-subtree "Toggle narrow"  :column "View")

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
  "p" 'org-insert-link
  "g" 'org-goto)

(global-set-key (kbd "<f12>") 'org-store-link)

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
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (global-set-key (kbd "C-b") 'helm-mini)
  (setq helm-split-window-in-side-p       t
        helm-move-to-line-cycle-in-source t)
  (helm-mode 1))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-s")   'helm-swoop-without-pre-input)
  (global-set-key (kbd "C-S-s") 'helm-multi-swoop-current-mode))

(use-package helm-ls-git
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'helm-browse-project))

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

(evil-define-key 'normal eww-mode-map
  "o" 'eww
  "H" 'eww-back-url
  "L" 'eww-forward-url)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python  . t)
   (shell   . t)
   (latex   . t)
   (C       . t)))

(setq org-confirm-babel-evaluate nil)

(defun org-deft-insert-link (file)
  (interactive (list
    (completing-read "Note: "
      (deft-find-all-files))))
  (org-insert-link nil (concat "file:" file) (file-name-base file)))

(evil-leader/set-key-for-mode 'org-mode
  "e" 'org-ctrl-c-ctrl-c
  "l" 'org-deft-insert-link)

(use-package org-noter
  :ensure t)

(use-package helm-org-rifle
  :ensure t
  :config
  (evil-define-key 'normal 'global
    (kbd "C-o")  'helm-org-rifle-org-directory
    (kbd "M-o")  'helm-org-rifle-current-buffer))

(use-package helm-ag
  :ensure t)

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-project-search-path '("~/projects"))
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

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
  (kbd "M-r")   'jump-to-related)

(use-package counsel-etags
  :ensure t
  :config
  (setq tags-revert-without-query t)
  (setq large-file-warning-threshold nil)
  (evil-leader/set-key
    "d" 'counsel-etags-find-tag-at-point)
  (evil-define-key 'normal 'global
    (kbd "C-t") 'counsel-etags-list-tag))

(add-hook 'c-mode-common-hook 'hs-minor-mode t)
(add-hook 'c-mode-common-hook 'hs-hide-initial-comment-block t)

(use-package ace-jump-mode
  :ensure t
  :config
  (global-set-key (kbd "C-x SPC") 'ace-jump-mode)
  (evil-define-key 'normal 'global
    (kbd "SPC") 'ace-jump-mode))

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

(add-hook 'eshell-mode-hook
  (lambda () 
    (define-key eshell-mode-map (kbd "<tab>")
      (lambda () (interactive) (completion-at-point)))))

(let ((mu4e-config "~/.emacs.d/email.el"))
 (when (file-exists-p mu4e-config)
   (load-file mu4e-config)))
