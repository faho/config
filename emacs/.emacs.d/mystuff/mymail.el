(use-package mu4e
  :ensure nil ;; Installed outside of emacs
  :commands mu4e
  :config
  (load-private-file)
  ;; Default to the first context, don't ask every start
  (setq mu4e-context-policy 'pick-first)
  ;;; mbsync
  (setq mu4e-maildir "~/.mail"
  		mu4e-drafts-folder "/gmail/[Google Mail].Drafts"
  		mu4e-trash-folder  "/gmail/Trash")
  (setq mu4e-maildir-shortcuts
  		'( ("/gmail/INBOX" . ?i)
  		   ("/Uni/INBOX"  . ?n)
  		   ("/gmail/[Google Mail].Drafts" . ?e)
  		   ("/gmail/arch/.[arch-projects]" . ?p)
  		   ("/gmail/arch/.[arch-dev-public]" . ?d)
		   ("/gmail/fish" . ?f)
  		   ("/gmail/systemd" . ?s)))
  (add-to-list 'mu4e-bookmarks
  			   '("maildir:/Uni/*" "Uni" ?n))
  (add-to-list 'mu4e-bookmarks
  			   '("maildir:/gmail/arch/.*" "Arch lists" ?a))
  ;; mbsync seems to have problems without this
  (setq mu4e-change-filenames-when-moving t)
  ;; Quiet normal output since it doesn't appear properly
  ;; enable "main" debug output instead
  ;; Also don't clutter home directory
  (setq mu4e-get-mail-command "mbsync -a -q -DM -c ~/.config/mbsync/mbsyncrc")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-update-interval nil)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-visible-columns 80)
  (setq mu4e-headers-visible-lines 10)
  (setq mu4e-attachment-dir "~/Downloads")

  ;; Make it so the older messages are on top, so the reading direction matches
  (setq mu4e-headers-sort-direction 'ascending)

  (setq mu4e-headers-skip-duplicates t)
  (setq message-send-mail-function 'smtpmail-send-it
		smtpmail-auth-credentials "~/.netrc")

  (defun mu4e-headers-jump-to-next-unread ()
	(interactive)
	(search mu4e-headers-new-mark))

  (setq mail-host-address "fordprefect")
  (setq message-kill-buffer-on-exit t)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Aesthetics
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-compose-signature nil)

  (setq mu4e-headers-fields
		'( (:human-date . 12)
		   (:flags . 6)
		   (:mailing-list . 15)
		   (:from . 28)
		   ;; Introduced in 0.9.12
		   ;; Shows subject only once per thread
		   (:thread-subject)))
  (setq mu4e-view-scroll-to-next nil)

  ;; Disable linum
  (if (boundp 'linum-disabled-modes-list)
	(progn
	  (add-to-list 'linum-disabled-modes-list 'mu4e-headers-mode)
	  (add-to-list 'linum-disabled-modes-list 'mu4e-view-mode)
	  (add-to-list 'linum-disabled-modes-list 'mu4e-main-mode)
	  (add-to-list 'linum-disabled-modes-list 'mu4e~update-mail-mode))
	(setq linum-disabled-modes-list '(mu4e-headers-mode mu4e-view-mode
														mu4e-main-mode
														mu4e~update-mail-mode)))

  ;; Encryption/Signing
  (unless (version< emacs-version "25.1")
    (require 'pinentry)
    (pinentry-start t)
    (setq pgp-gpg-use-agent t
          mu4e-decryption-policy 'ask)
    (add-hook
     'message-send-hook
     ;; Unfortunately I can't figure out how to encrypt-if-possible
     'mml-secure-message-sign))
  ;; Automatically decrypt inline-pgp
  (add-hook
   'mu4e-view-mode-hook
   'epa-mail-decrypt)

  (defhydra hydra-mu4e-mark (:color blue)
    "Mu4e marks"
    ("q" nil "cancel")
    ("<SPC>" hydra-leader/body "leader")
    (";"  mu4e-context-switch "Switch context")
    ("!"  mu4e-headers-mark-for-read "Read")
    ("r"  mu4e-headers-mark-for-refile "Refile")
    ("t"  mu4e-headers-mark-subthread "Subthread")
    ("u"  mu4e-headers-mark-for-unmark "Unmark")
    ("<backspace>"     mu4e-headers-mark-for-trash "Trash")
    ("<delete>"        mu4e-headers-mark-for-delete "Delete")
    ("#"  mu4e-mark-resolve-deferred-marks "Resolve deferred")
    ("%"  mu4e-headers-mark-pattern "Mark pattern")
    ("&"  mu4e-headers-mark-custom "Mark Custom")
    ("*"  mu4e-headers-mark-for-something "Something")
    ("+"  mu4e-headers-mark-for-flag "Flag")
    ("m" mu4e-headers-mark-for-move)
    ("T" mu4e-headers-mark-thread)
    ("/" mu4e-headers-search-narrow)
    ("?" mu4e-headers-mark-for-unread)
    ("A" mu4e-headers-mark-for-action)
    ("O" mu4e-headers-change-sorting)
    ("P" mu4e-headers-toggle-threading)
    ("Q" mu4e-headers-toggle-full-search)
    ("V" mu4e-headers-toggle-skip-duplicates)
    ("W" mu4e-headers-toggle-include-related)
    ("-"  mu4e-headers-mark-for-unflag "Unflag"))

  ;; Yes, up and down are reversed (i.e. it's saying where the _text_ scrolls, not the view)
  (bind-keys :map mu4e-view-mode-map
			 ("j" . scroll-up-line)
			 ("k" . scroll-down-line)
			 ("J" . mu4e~headers-jump-to-maildir)
			 ("l" . mu4e-view-headers-next)
			 ("h" . mu4e-view-headers-prev)
			 ("H" . mu4e-view-toggle-hide-cited)
			 ("b" . mu4e-headers-search-bookmark)
			 ("o" . mu4e-view-open-attachment)
			 ("U" . mu4e-update-mail-and-index) ;; Get new mail
			 ("e" . mu4e-view-save-attachment)
			 ("Q" . mu4e-raw-view-quit-buffer)
			 ("i" . mu4e~view-quit-buffer)
			 ("<RET>" . faho/mu4e-open-or-browse)
             ("<SPC>" . hydra-mu4e-mark/body)
			 ("C" . mu4e-compose-new))
  (bind-keys :map mu4e-headers-mode-map
			 ("J" . mu4e~headers-jump-to-maildir)
			 ("j" . next-line)
			 ("k" . previous-line)
			 ("h" . mu4e-headers-prev)
			 ("l" . mu4e-headers-next)
			 ("C" . mu4e-compose-new)
			 ("b" . mu4e-headers-search-bookmark)
			 ("U" . mu4e-update-mail-and-index)
			 ("o" . mu4e-view-message)
			 ("i" . mu4e~headers-quit-buffer)
             ("<SPC>" . hydra-mu4e-mark/body)
			 ((faho/kbd "C-a") . mml-attach-file))
  ;; Make mu4e-compose-reply keep the message in a window
  (defadvice mu4e-compose-reply (before reply-in-other-window)
	()
	(split-window-right))
  (ad-activate 'mu4e-compose-reply)

  (defun faho/mu4e-open-or-browse () 
	"Open point in browser or as an attachment"
	(interactive)
	(unless (mu4e~view-browse-url-from-binding)
	  (mu4e~view-open-attach-from-binding)))

  (require  'org-mu4e)
  (setq mu4e-compose-complete-addresses t)

  ;; Use dired to add attachments
  ;; (http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html)
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
	"Return a list of active message buffers."
	(let (buffers)
	  (save-current-buffer
		(dolist (buffer (buffer-list t))
		  (set-buffer buffer)
		  (when (and (derived-mode-p 'message-mode)
					 (null message-sent-message-via))
			(push (buffer-name buffer) buffers))))
	  (nreverse buffers)))
  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; HTML mail
  ;; Use eww's renderer (requires emacs 24.4 IIRC)
  (if (require 'mu4e-contrib nil t)
	  (setq mu4e-html2text-command 'mu4e-shr2text))

  (defun mu4e-msgv-action-view-in-browser (msg)
	"View the body of the message in a web browser."
	(interactive)
	(let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
		  (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
	  (unless html (error "No html part for this message"))
	  (with-temp-file tmpfile
		(insert "<html>" "<head><meta http-equiv=\"content-type\"" "content=\"text/html;charset=UTF-8\">" html))
	  (browse-url (concat "file://" tmpfile))))
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-msgv-action-view-in-browser) t)
  )

(provide 'mymail)

