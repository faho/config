(req-package mu4e
  :require smtpmail
  :commands mu4e
  :config
  (load-private-file)
  ;;; offlineimap
  ;; (setq mu4e-maildir "~/.mail"
  ;; 		mu4e-drafts-folder "/main/[Google Mail].Drafts"
  ;; 		mu4e-trash-folder  "/main/Trash"
  ;; 		mu4e-sent-folder "/main/[Google Mail].Sent Mail")
  ;; (setq mu4e-maildir-shortcuts
  ;; 		'( ("/main/INBOX" . ?i)
  ;; 		   ("/Uni/INBOX"  . ?n)
  ;; 		   ("/main/[Google Mail].Drafts" . ?e)
  ;; 		   ("/main/arch.[arch-projects]" . ?p)
  ;; 		   ("/main/arch.[arch-dev-public]" . ?d)
  ;; 		   ("/main/systemd" . ?s)))
  ;; (add-to-list 'mu4e-bookmarks
  ;; 			   '("maildir:/Uni/*" "Uni" ?n))
  ;; (add-to-list 'mu4e-bookmarks
  ;; 			   '("maildir:/main/arch.*" "Arch lists" ?a))
  ;; (setq mu4e-get-mail-command "offlineimap -oq")
  ;;; mbsync
  (setq mu4e-maildir "~/.mail"
  		mu4e-drafts-folder "/gmail/[Google Mail]/.Drafts"
  		mu4e-trash-folder  "/gmail/Trash"
		smtpmail-smtp-server "smtp.gmail.com"
		smtpmail-smtp-service 587
  		mu4e-sent-folder "/gmail/[Google Mail]/.Sent Mail")
  (setq mu4e-maildir-shortcuts
  		'( ("/gmail/INBOX" . ?i)
  		   ("/Uni/INBOX"  . ?n)
  		   ("/gmail/[Google Mail]/.Drafts" . ?e)
  		   ("/gmail/arch/.[arch-projects]" . ?p)
  		   ("/gmail/arch/.[arch-dev-public]" . ?d)
  		   ("/gmail/systemd" . ?s)))
  (add-to-list 'mu4e-bookmarks
  			   '("maildir:/Uni/*" "Uni" ?n))
  (add-to-list 'mu4e-bookmarks
  			   '("maildir:/gmail/arch/.*" "Arch lists" ?a))
  ;; mbsync seems to have problems without this
  (setq mu4e-change-filenames-when-moving t)
  ;; Quiet normal output since it doesn't appear properly
  ;; enable "main" debug output instead
  (setq mu4e-get-mail-command "mbsync -a -q -DM")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-update-interval nil)
  (setq mu4e-headers-auto-update t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-headers-visible-columns 72)
  (setq mu4e-headers-visible-lines 10)

  (setq mu4e-headers-skip-duplicates t)
  (setq message-send-mail-function 'smtpmail-send-it
		smtpmail-auth-credentials "~/.netrc")

  (defun jk-mu4e-set-account ()
	"Set the account for composing a message."
	(let* ((account
			(if mu4e-compose-parent-message
				(let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir)))
				  (string-match "/\\(.*?\\)/" maildir)
				  (match-string 1 maildir))
			  (completing-read (format "Compose with account: (%s) "
									   (mapconcat #'(lambda (var) (car var)) jk-mu4e-account-alist "/"))
							   (mapcar #'(lambda (var) (car var)) jk-mu4e-account-alist)
							   nil t nil nil (caar jk-mu4e-account-alist))))
		   (account-vars (cdr (assoc account jk-mu4e-account-alist))))
	  (if account-vars
		  (mapc #'(lambda (var)
					(set (car var) (cadr var)))
				account-vars)))) 
  (defun mu4e-headers-jump-to-next-unread ()
	(interactive)
	(search mu4e-headers-new-mark))
  (add-hook 'mu4e-compose-pre-hook 'jk-mu4e-set-account)

  (setq mail-host-address "fordprefect")
  (setq message-kill-buffer-on-exit t)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Aesthetics
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-seen-mark '("S" . "☑")) ;seen
  (setq mu4e-headers-unseen-mark '("u" . "☐")) ; unseen
  (setq mu4e-headers-flagged-mark '("F" .  "⚵"))  ;flagged
  (setq mu4e-headers-new-mark '("N" .  "✉"))  ;new
  (setq mu4e-headers-replied-mark '("R" . "↵")) ;replied
  (setq mu4e-headers-passed-mark '("P" . "⇉")) ;passed
  (setq mu4e-headers-encrypted-mark '("x" . "⚷")) ;encrypted
  (setq mu4e-headers-signed-mark '("s" . "✍")) ;signed
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
  (setq pgp-gpg-use-agent t
		mu4e-decryption-policy 'ask)
  (add-hook
   'message-send-hook
   'mml-secure-message-sign-pgpmime)

  ;; Add some evil-ish keybindings
  ;; Not using evil-make-overriding-map since it would mean defining mu4e-ish stuff manually
  ;; It would work like this:
  ;;(evil-make-overriding-map mu4e-view-mode-map 'normal t)
  ;;(evil-add-hjkl-bindings mu4e-view-mode-map 'motion "J" 'mu4e-jump-to-maildir)
  ;; And need this:
  ;; Allow evil-mode
  ;;(delete 'mu4e-headers-mode 'evil-emacs-state-modes)
  ;;(setq evil-motion-state-modes (append evil-motion-state-modes (list 'mu4e-headers-mode
  ;;																	'mu4e-main-mode
  ;; 																	'mu4e-view-mode)))
  (eval-after-load 'mu4e
	'(progn
	   ;; Use the standard bindings as a base
	   ;; Yes, up and down are reversed (i.e. it's saying where the _text_ scrolls, not the view)
	   (define-key mu4e-view-mode-map (kbd "j") 'scroll-up-line)
	   (define-key mu4e-view-mode-map (kbd "k") 'scroll-down-line)
	   (define-key mu4e-view-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir)
	   (define-key mu4e-view-mode-map (kbd "l") 'mu4e-view-headers-next)
	   (define-key mu4e-view-mode-map (kbd "h") 'mu4e-view-headers-prev)
	   (define-key mu4e-view-mode-map (kbd "H") 'mu4e-view-toggle-hide-cited)
	   (define-key mu4e-view-mode-map (kbd "b") 'mu4e-headers-search-bookmark)
	   (define-key mu4e-view-mode-map (kbd "o") 'mu4e-view-open-attachment)
	   (define-key mu4e-view-mode-map (kbd "U") 'mu4e-update-mail-and-index) ;; Get new mail
	   (define-key mu4e-view-mode-map (kbd "e") 'mu4e-view-save-attachment)
	   (define-key mu4e-view-mode-map (kbd "Q") 'mu4e-raw-view-quit-buffer)
	   (define-key mu4e-view-mode-map (kbd "i") 'mu4e~view-quit-buffer)
	   (define-key mu4e-view-mode-map (kbd "<RET>") 'my-mu4e-open-or-browse)
	   (define-key mu4e-view-mode-map (kbd "C") 'mu4e-compose-new)
	   (define-key mu4e-headers-mode-map (kbd "J") 'mu4e~headers-jump-to-maildir)
	   (define-key mu4e-headers-mode-map (kbd "j") 'next-line)
	   (define-key mu4e-headers-mode-map (kbd "k") 'previous-line)
	   (define-key mu4e-headers-mode-map (kbd "h") 'mu4e-headers-prev)
	   (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-next)
	   (define-key mu4e-headers-mode-map (kbd "C") 'mu4e-compose-new)
	   (define-key mu4e-headers-mode-map (kbd "b") 'mu4e-headers-search-bookmark)
	   (define-key mu4e-headers-mode-map (kbd "U") 'mu4e-update-mail-and-index)
	   (define-key mu4e-headers-mode-map (kbd "o") 'mu4e-view-message)
	   (define-key mu4e-headers-mode-map (kbd "i") 'mu4e~headers-quit-buffer)
	   (define-key mu4e-compose-mode-map (my-kbd "C-a") 'mml-attach-file)
	   ;; Make mu4e-compose-reply keep the message in a window
	   (defadvice mu4e-compose-reply (before reply-in-other-window)
		 ()
		 (split-window-right))
	   (ad-activate 'mu4e-compose-reply)
	   ))

  (defun my-mu4e-open-or-browse () 
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

  ;; Inhibit openwith so messages can actually be sent
  (require 'mm-util)
  (add-to-list 'mm-inhibit-file-name-handlers 'openwith-file-handler)

  ;; HTML mail
  ;; Use eww's renderer (requires emacs 24.4 IIRC)
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; (setq mu4e-html2text-command "w3m -T text/html")

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

