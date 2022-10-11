(use-package mu4e
  :custom
  (mu4e-maildir "~/mail")
  (mu4e-get-mail-command "true")
  (mu4e-update-interval 120)

  (sendmail-program "msmtp")
  (send-mail-function 'smtpmail-send-it)
  (message-sendmail-f-is-evil t)
  (message-send-mail-function 'message-send-mail-with-sendmail)
  (mu4e-change-filenames-when-moving t)

  (mu4e-compose-complete-only-after "2016-01-01")

  (mu4e-use-fancy-chars t)
  (mu4e-headers-attach-mark '("a" . "@")) ; alignment fix
  (mu4e-split-view 'single-window)
  (mu4e-headers-fields '((:human-date . 12)
                         (:flags . 6)
                         (:from . 22)
                         (:subject)))
  (mu4e-hide-index-messages t)
  (mu4e-completing-read-function 'helm-comp-read)
  (shr-color-visible-luminance-min 80)
  (doom-modeline-mu4e t)
  (doom-modeline-gnus nil)

  :config
  (require 'org-mu4e)
  (evil-collection-init 'mu4e)
  (define-obsolete-function-alias 'mu4e-search-bookmark 'mu4e-headers-search-bookmark nil)
  (define-obsolete-function-alias 'mu4e-search 'mu4e-headers-search nil)
  (define-obsolete-function-alias 'mu4e-search-narrow 'mu4e-headers-search-narrow nil)
  (add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1))))

(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "private"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/automatix" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "adrian@kummerlaender.eu")
                  (user-full-name    . "Adrian Kummerländer")
                  (mu4e-sent-folder   . "/automatix/Sent")
                  (mu4e-drafts-folder . "/automatix/Drafts")
                  (mu4e-trash-folder  . "/automatix/Trash")
                  (message-sendmail-extra-arguments . ("--read-envelope-from" "--account=automatix"))))
        ,(make-mu4e-context
          :name "KIT"
          :match-func (lambda (msg)
                        (when msg
                          (string-match-p "^/KIT" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "adrian.kummerlaender@kit.edu")
                  (user-full-name     . "Adrian Kummerländer")
                  (mu4e-sent-folder   . "/KIT/Sent Items")
                  (mu4e-trash-folder  . "/KIT/Deleted Items")
                  (mu4e-drafts-folder . "/KIT/Drafts")
                  (message-sendmail-extra-arguments  . ("--read-envelope-from" "--account=KIT"))
                  (mu4e-compose-signature-auto-include . nil)
                  (mu4e-compose-signature . (concat "\n"
                                                    "Karlsruhe Institute of Technology (KIT)\n"
                                                    "Institute for Mechanical Process Engineering and Mechanics (MVM)\n"
                                                    "Institute for Applied and Numerical Mathematics (IANM)\n"
                                                    "Lattice Boltzmann Research Group (LBRG)\n\n"
                                                    "M.Sc. Adrian Kummerländer\n"
                                                    "Research Associate\n\n"
                                                    "E-mail: adrian.kummerlaender@kit.edu\n"
                                                    "Phone: +49 721 608 43157\n\n"
                                                    "Room 3.016\n"
                                                    "Building 20.30\n"
                                                    "Englerstr. 2\n"
                                                    "D-76131 Karlsruhe"))))))

(evil-leader/set-key "m" 'mu4e)

(add-to-list 'org-capture-templates
             '("m" "eMail note" entry (file org-default-notes-file)
               "* TODO /%:subject/\n See %a\n%?"
               :prepend t))
(add-to-list 'org-capture-templates
             '("r" "Respond to eMail" entry (file org-default-notes-file)
               "* TODO Respond to /%:subject/\n See %a\n%?"
               :prepend t
               :immediate-finish t))
