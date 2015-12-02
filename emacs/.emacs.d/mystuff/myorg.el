;;; Org mode
(req-package org
  :mode ("\\.org$" . org-mode) 
  :config (progn
			(setq org-ellipsis "…") ;; Nice unicode ellipsis in one character
			(setq org-src-fontify-natively t)
			(setq org-directory "~/docs/org/")
			(setq org-mobile-directory "~/owncloud/MobileOrg")
			(setq org-mobile-inbox-for-pull (concat org-directory "index.org"))
			;; (require 'org-caldav)
			;; (require 'auth-source)
			;; (load-private-file)
			(setq org-caldav-calendar-id "default")
			(setq org-caldav-inbox (concat org-directory "calendar.org"))
			(setq org-caldav-files (concat org-directory "appointments.org"))
			(setq org-icalendar-timezone "Europe/Berlin")
			(setq calendar-date-style 'european)
			(setq org-agenda-include-diary t)
			(setq diary-file (concat org-directory "diary.org"))
			(setq org-default-notes-file (concat org-directory "notes.org"))
			(add-hook 'org-agenda-mode-hook
					  (lambda ()
						(define-key org-agenda-mode-map "p" 'org-agenda-goto-date)
						;; Vi-style key bindings
						(define-key org-agenda-mode-map "j" 'org-agenda-next-line)
						(define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
						(define-key org-agenda-mode-map "l" 'org-agenda-later)
						(define-key org-agenda-mode-map "h" 'org-agenda-earlier)))
			(setq org-todo-keywords
				  '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "DONE(d)")))
			(setq org-todo-keyword-faces
				  '(
					("TODO" . (:foreground "red" :weight bold))
					("INPROGRESS" . (:foreground "yellow" :weight bold))
					("WAITING" . (:foreground "red" :weight bold))
					("DONE" . (:foreground "green" :weight bold))
					))
			(org-babel-do-load-languages
			 'org-babel-load-languages '(
										 (sh . t)
										 (python . t)
										 (sqlite . t)
										 (latex . t)))
			(setq org-return-follows-link t)
			;; (require 'ox-reveal)
			;; (setq org-reveal-root (expand-file-name "dev/reveal.js" "~/"))
			))
(provide 'myorg)
