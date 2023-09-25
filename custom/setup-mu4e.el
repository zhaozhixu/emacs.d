(use-package mu4e
  :defer t
  :custom
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
  (message-kill-buffer-on-exit t) ; don't keep message buffers around

  ;; use mu4e for e-mail in emacs
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-drafts-folder "/Foxmail/Local Drafts")
  (mu4e-sent-folder   "/Foxmail/Sent Messages")
  (mu4e-trash-folder  "/Foxmail/Trash")
  (mu4e-refile-folder "/Foxmail/Archive")

  ;; don't save message to Sent Messages, Foxmail/IMAP takes care of this
  (mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Foxmail addresses and want assign them different
  ;; behavior.)

  (mu4e-bookmarks
   '(("flag:unread AND NOT flag:trashed"                "Unread messages"        ?u)
     ("date:today..now"                                 "Today's messages"       ?t)
     ("date:7d..now"                                    "Last 7 days"            ?w)
     ("date:1d..now"                                    "Last 1 days"            ?d)
     ("date:1d..now AND NOT list:vger.kernel.org"       "Last 1 days (others)"   ?o)
     ("date:1d..now AND list:vger.kernel.org"           "Last 1 days (kernel)"   ?k)
     ("maildir:/Foxmail/INBOX"                          "INBOX"                  ?i)
     ("maildir:/Foxmail/Sent Messages"                  "Sent Messages"          ?s)
     ("mime:image/*"                                    "Messages with images"   ?p)))

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'Archive' folder by pressing ``ma''.
  (mu4e-maildir-shortcuts
   '(("/Foxmail/INBOX"          . ?i)
     ("/Foxmail/Sent Messages"  . ?s)
     ("/Foxmail/Local Drafts"   . ?d)
     ("/Foxmail/Archive"        . ?a)))

  ;; allow for updating mail using 'U' in the main view:
  ;; See ~/.offlineimaprc for config.
  (mu4e-get-mail-command "offlineimap" mu4e-update-interval 300) ; update every 5 minutes

  ;; something about ourselves
  (user-mail-address "zhixu001@foxmail.com"
                     user-full-name  "Zhixu Zhao"
                     mu4e-compose-signature
                     (concat
                      "Zhixu Zhao\n"
                      "https://github.com/zhaozhixu\n"))

  (mu4e-headers-fields
   '((:human-date . 12)
     (:flags . 6)
     (:mailing-list . 10)
     (:from-or-to . 22)
     (:subject)))
  (mu4e-view-show-addresses t)
  (mu4e-attachment-dir "~/Downloads/attachment")

  :config
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 80)
              ;; (flyspell-mode)
              (setq tab-width 8)))

  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-bcc ()
              "Add a Bcc: header."
              (save-excursion (message-add-header "Bcc: \n"))))

  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-cc ()
              "Add a Cc: header."
              (save-excursion (message-add-header "Cc: \n"))))
  )

;; sending mail -- replace USERNAME with your foxmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu
(use-package smtpmail
  :after (mu4e)
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-user "zhixu001@foxmail.com")
  (smtpmail-smtp-server "smtp.qq.com")
  (smtpmail-smtp-service 465)
  (smtpmail-debug-info t)
  (smtpmail-debug-verb t))

(provide 'setup-mu4e)
