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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "<M-tab>") 'next-buffer)
(global-set-key (kbd "<M-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'other-window)

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

(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-vcs-max-length 24)
  (setq doom-modeline-minor-modes t))

(use-package hydra
  :ensure t)

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

(use-package nix-mode
  :ensure t)
(use-package nix-buffer
  :ensure t)

(use-package org
  :ensure t
  :config
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  (setq org-hide-emphasis-markers t)
  (setq org-src-preserve-indentation t)
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-agenda-files '("~/org"))
  (setq org-link-frame-setup '((file . find-file))) ; open links in same frame
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

(global-set-key (kbd "C-c c") 'org-capture)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-latex-preview-ltxpng-directory "~/.emacs.d/ltxpng/")
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(evil-leader/set-key
  "p" 'org-latex-preview)

(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

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

(defhydra hydra-org-mode ()
  "Org mode"
  ("e" org-babel-execute-buffer "Execute buffer" :column "Babel")
  ("t" org-babel-tangle         "Tangle"         :column "Babel")

  ("i" org-toggle-inline-images "Toggle images"  :column "Preview")
  ("l" org-latex-preview        "Toggle LaTeX"   :column "Preview")

  ("q" nil "Exit menu" :column "Other"))

(evil-define-key 'normal org-mode-map
  "J" 'org-next-visible-heading
  "K" 'org-previous-visible-heading
  "m" 'hydra-org-mode/body
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

(evil-define-key 'normal 'global
  "J" 'evil-forward-paragraph
  "K" 'evil-backward-paragraph
  (kbd "C-b")     'ivy-switch-buffer

  (kbd "C-f")     'counsel-find-file
  (kbd "C-r")     'counsel-recentf
  (kbd "C-p")     'counsel-git

  (kbd "C-n")     'go-to-deft)

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

(evil-leader/set-key
  "e" 'org-babel-execute-src-block
  "l" 'org-deft-insert-link
  "s" 'evil-ex-nohighlight)

(use-package org-noter
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package magit
  :ensure t)

(use-package counsel-etags
  :ensure t
  :config
  (setq tags-revert-without-query t)
  (setq large-file-warning-threshold nil)
  (add-hook 'prog-mode-hook
    (lambda ()
      (add-hook 'after-save-hook
                'counsel-etags-virtual-update-tags 'append 'local)))
  (evil-leader/set-key
    "d" 'counsel-etags-find-tag-at-point))

(use-package ag
  :ensure t)

(add-hook 'c-mode-common-hook 'hs-minor-mode t)
(add-hook 'c-mode-common-hook 'hs-hide-initial-comment-block t)

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/projects/dev"
																				 "~/projects/contrib"
																				 "~/projects/playground")))

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
