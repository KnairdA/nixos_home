(use-package mu4e
  :config
  (setq mu4e-maildir "~/mail")
  (setq mu4e-get-mail-command "true")
  (setq mu4e-update-interval 120)
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-attach-mark '("a" . "@")) ; alignment fix
  (setq mu4e-change-filenames-when-moving t)
  (setq sendmail-program "msmtp"
        send-mail-function 'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-send-mail-function 'message-send-mail-with-sendmail)
  (setq mu4e-hide-index-messages t)
  (setq mu4e-completing-read-function 'ivy-completing-read)
  (require 'org-mu4e)
  (evil-collection-init 'mu4e)
  (setq doom-modeline-mu4e t))

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
                          (string-match-p "^/kit" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "adrian.kummerlaender@student.kit.edu")
                  (user-full-name     . "Adrian Kummerländer")
                  (mu4e-sent-folder   . "/kit/Sent")
                  (mu4e-drafts-folder . "/kit/Drafts")
                  (mu4e-trash-folder  . "/kit/Trash")
                  (message-sendmail-extra-arguments  . ("--read-envelope-from" "--account=kit"))))))
