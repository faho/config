;; This is supposed to contain utility functions that don't depend on anything
(defun faho/kbd (chord)
  "Return kbd prefixed with C-c"
  (concat "C-c " chord))

(defun xdg-open (&optional filename)
  "Open parameter or current file with xdg-open"
  (interactive)
  (if filename
	  (call-process "xdg-open" nil nil nil filename)
	(call-process "xdg-open" nil nil nil buffer-file-name)))

(defun open (filename)
  "Open filename specified in minibuffer"
  (interactive (list (read-file-name "Enter filename:")))
  (xdg-open filename))

(defun faho/list-recursively (dir)
  "Return all files in a directory or child-directories as a flat list"
  (interactive "f")
  (unless (member (file-name-nondirectory dir) '("." ".."))
    (if (file-directory-p dir)
       (-flatten (mapcar 'faho/list-recursively (directory-files dir t)))
      dir)))

;; TODO: Pass an argument to reverse direction
(defun faho/configure ()
  "Switch through configuration files"
  (interactive)
  (let ((file (when buffer-file-truename (file-truename buffer-file-truename)))
        (files (mapcar 'file-truename (faho/list-recursively user-emacs-directory))))
    (let ((next (cadr (member file files))))
      (if next
            (find-file next)
        (find-file user-init-file)))))

(defun tags ()
  "Search for tags like FIXME, HACK or TODO"
  (interactive)
  (re-search-forward "FIXME\\|HACK\\|TODO\\|STUB"))

(defun copy-to-clipboard ()
  "Copy current region (or evil-visual) to clipboard"
  (interactive)
  (if (display-graphic-p)
      (progn
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region
		   ;; Use evil-visual if possible, else fallback to region
		   (or evil-visual-beginning region-beginning)
		   (or evil-visual-end region-end)
		   "xsel -i -b")
		  ;; Supress "Shell command completed with no output"
		  (message "")))))

(defun paste-from-clipboard ()
  (interactive)
  (if (display-graphic-p)
	  (progn
		(clipboard-yank)
		)
	(insert (shell-command-to-string "xsel -o -b"))
	))

;; From http://nullprogram.com/blog/2010/09/29/
(defun expose (function)
  "Return an interactive version of FUNCTION."
  (lexical-let ((lex-func function))
    (lambda ()
      (interactive)
      (funcall lex-func))))

(defun expose-partially (function &optional arg)
  "Return an interactive version of an applied FUNCTION"
  (interactive)
  (expose (apply-partially function arg)))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defalias 'mv 'rename-file-and-buffer)

(defvar guess-language-rules
  '(("english" . "\\<\\(of\\|the\\|and\\|or\\|how\\|be\\|to\\|that\\|have\\|about\\|your\\|after\\|back\\)\\>")
    ("deutsch" . "\\<\\(zu\\|aber\\|aus\\|durch\\|wurde\\|nicht\\|sich\\|dass\\|während\\|ohne\\|eines\\|zeit\\|Zeit\\|lernen\\|und\\|oder\\|der\\|die\\|das\\|wie\\)\\>")
    ("dutch" . "\\<\\(bij\\|het\\|sluiten\\|van\\|kijk\\|de\\|vraag\\|voor\\|antwoorden\\)\\>"))
  ;; Don't have dictionaries for these:
  ;;("french" . "\\<\\(et\\|ou\\|[ld]es\\|que\\)\\>")
  ;;("portuguese" . "\\<\\(de\\|para\\|e\\|ou\\|como\\)\\>"))
  "Alist of rules to determine the language of some text.
  Each rule has the form (CODE . REGEXP) where CODE is a string to
  identify the language (probably according to ISO 639), and REGEXP is a
  regexp that matches some very common words particular to that language.
  The default language should be listed first. That will be the language
  returned when no REGEXP matches, as would happen for an empty
  document.")

(defun guess-buffer-language ()
  "Guess language in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count (mapcar (lambda (x)
						   (cons (count-matches (cdr x)) (car x)))
						 guess-language-rules)))
      (cdr (assoc (car (sort (mapcar 'car count) '>))
				  count)))))

(defun guess-language ()
  "Guess language in the current buffer."
  (interactive)
  (message (guess-buffer-language)))
(defun set-dict ()
  "Set dictionary to guessed dictionary."
  (interactive)
  (ispell-change-dictionary (guess-buffer-language)))

(defun revert-this-buffer ()
  (interactive)
  (revert-buffer nil t t)
  (message (concat "Reverted buffer " (buffer-name))))

;; TODO: Request git directory
;; TODO: Use a proper git-binding
(defun insert-format-patch ()
  "Insert a git patch"
  (interactive)
  (insert (shell-command-to-string (format "git format-patch --stdout" (ido-completing-read "Commit start:" '("HEAD~" "origin/master") 'nil)))))

(defun faho/emms-call (func) 
  "Load emms and call argument"
  (interactive)
  (require 'myemms)
  (funcall func))

(defun faho/mpd ()
  (interactive)
  (faho/emms-call 'emms-smart-browse))

(defun load-private-file ()
  (interactive)
  (let ((private-file (expand-file-name ".emacs-private" "~/")))
	   (if (file-exists-p private-file)
		   (load-file private-file)
		 (error (concat "Please put a private file to " private-file))
	   )))
  
(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(provide 'myutil)
