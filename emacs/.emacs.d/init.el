;;;Emacs configuration
;; Set these first so we don't see them flicker
(if (display-graphic-p)
	(progn
	  (menu-bar-mode t)
	  (if (fboundp 'tool-bar-mode) tool-bar-mode t)
	  (if (fboundp 'scroll-bar-mode) scroll-bar-mode 'right)
	  )
  (menu-bar-mode -1)
  (if (fboundp 'tool-bar-mode) tool-bar-mode nil)
  (if (fboundp 'scroll-bar-mode) scroll-bar-mode nil)
	)
(setq-default major-mode 'text-mode)

;; Ask for y/n instead of "yes"/"no"
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
	  inhibit-splash-screen t
	  inhibit-startup-buffer-menu t ; Don't show buffer list when opening multiple files
	  inhibit-default-init t ; Don't read default.el - I like my settings to be mine
	  initial-scratch-message "")

;; Don't warn about advice
(setq ad-redefinition-action 'accept)

(setq user-cache-directory (if (getenv "XDG_CACHE_HOME") (getenv "XDG_CACHE_HOME") "~/.cache"))
(setq user-data-directory (if (getenv "XDG_DATA_HOME") (getenv "XDG_DATA_HOME") "~/.local/share"))

(setq package-user-dir (expand-file-name "emacs" user-data-directory))

;;; Packages
;; Stuff I wrote
(add-to-list 'load-path (expand-file-name "mystuff" user-emacs-directory))
;; Stuff that's not in the repos
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

(dolist (dir load-path)
  (make-directory dir t))

(setq faho-config-files '("local/evil-evilified-state.el"
						  "mystuff/mymail.el"
						  "mystuff/myorg.el"
						  "mystuff/myutil.el"))

(setq faho-config-url "https://raw.githubusercontent.com/faho/config/master/emacs/.emacs.d/")

(dolist (file faho-config-files)
  (if (not (file-exists-p (expand-file-name file user-emacs-directory)))
  (url-copy-file (concat faho-config-url file) (expand-file-name file user-emacs-directory))))

;; Utility functions - always load
(require 'myutil)

(setq load-prefer-newer t)

(require 'package)
(setq package-archives
	  '(("gnu" . "http://elpa.gnu.org/packages/")
		("marmalade" . "https://marmalade-repo.org/packages/") 
		;; ("melpa" . "http://melpa.org/packages/") ;; This contains packages from git
		("melpa-stable" . "http://stable.melpa.org/packages/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Pinning packages to repos
;; Useful because ox-reveal may depend on _really_ new org features
;; (setq package-pinned-packages '((ox-reveal . "melpa-stable")))

;; Install use-package because that installs everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; These autoload through use-package
;; Don't error if not found
(require 'mymail nil t)
(require 'myorg nil t)

;;; Aesthetics
;; Mode line
(use-package smart-mode-line
  :init
  (progn
	(setq sml/theme 'respectful)
	(setq sml/no-confirm-load-theme t)
	(sml/setup)
	))
(column-number-mode t)
(display-time-mode t)
(setq-default display-time-24hr-format t)
(setq-default display-time-format nil)
(global-hl-line-mode t)
;; Colors
(setq color-theme-is-global t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; List of okay themes:
;; wheatgrass
;; cyberpunk
;; ir-black
;; ir_black
;; reverse
;; ample
;; flatland-black
(use-package cyberpunk-theme
  :init
  (load-theme 'cyberpunk' t)
  )

;; This should be outside of use-package so we can add to it from outside
(setq linum-disabled-modes-list '(shell-mode inferior-emacs-lisp-mode))
(use-package nlinum
  :init
  (global-linum-mode t)
  ;; Disable linum in select modes
  ;; and all that aren't derived from prog-mode
  (defun linum-on ()
	(if (derived-mode-p 'prog-mode)
		(if (not (boundp 'linum-disabled-modes-list))
			(nlinum-mode 1)
		  (if (eq nil linum-disabled-modes-list)
			  (nlinum-mode 1)
			(unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
			  (nlinum-mode 1))))))
  )
(setq diff-switches "-u")

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )
(show-paren-mode t) ;; Highlight matching parens

(use-package diminish)

(use-package expand-region)

;;; Minor Modes:
;; Vim-Mode
(use-package evil
  :diminish (undo-tree-mode . "")
  :config
  (progn
	(evil-mode t)
	;; Clipboard
	(defadvice evil-yank (after clipboard)
	  ()
	  (copy-to-clipboard))
	(ad-activate 'evil-yank)
	;; Don't signal state in echo area
	(setq evil-echo-state nil)
	;; Remove some unhelpful bindings from evil so they are available elsewhere
	(bind-key "RET" nil evil-motion-state-map)
	(bind-key "C-e" nil evil-motion-state-map)
	(bind-key "C-e" nil evil-insert-state-map)
	(bind-key "TAB" nil evil-normal-state-map)
	(bind-key "TAB" nil evil-motion-state-map)
	(bind-key "C-M-m" 'scroll-other-window-down evil-normal-state-map) ;; Yes, this scrolls up
	;; (bind-key "<escape>" 'keyboard-quit evil-normal-state-map)
	;; (bind-key "<escape>" 'keyboard-quit evil-visual-state-map)
	;; My simple replacement for evil-leader
	(bind-key "SPC" 'hydra-leader/body evil-normal-state-map)
	(bind-key "u" 'undo-tree-undo evil-visual-state-map)
	(bind-key "+" 'er/expand-region evil-visual-state-map)
	(bind-key "q" 'delete-window evil-normal-state-map)
	(setq evil-want-visual-char-semi-exclusive t)
	(evil-define-key 'motion Info-mode-map
	  "\t" 'Info-next-reference
	  "n" 'Info-history-back
	  "/" 'Info-history-forward
	  "l" 'Info-next
	  "h" 'Info-prev
	  "d" 'Info-directory
	  "y" 'evil-yank)
	;; (evil-set-initial-state 'wdired-mode 'normal)
	;; "{" and "}" are badly reachable on QWERTZ, "ö" and "Ö" are unused
	(bind-key "ö" 'evil-forward-paragraph evil-motion-state-map)
	(bind-key "Ö" 'evil-backward-paragraph evil-motion-state-map)
	;; Don't use C-i since that's TAB in a terminal
	(setq evil-want-C-i-jump nil)
	;; http://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
	(setq evil-want-fine-undo 'fine)
	;;; EVILIFY MODES
	;; Adds the "evilify" macro to add evil keybindings to modes
	(require 'evil-evilified-state)
	(evilify package-menu-mode package-menu-mode-map)
	;; From spacemacs
	;; default state for additional modes
	(dolist (mode '(magit-popup-mode
					git-rebase-mode
					comint-mode
					shell-mode
					term-mode
					neotree-mode
					magit-popup-sequence-mode))
	  (add-to-list 'evil-emacs-state-modes mode))
	;; Set cursor when in evil-insert mode
	;; This works for KDE4 Konsole
	(unless (display-graphic-p)
	  ;; Reset on exit
	  (add-hook 'kill-emacs-hook (lambda () (send-string-to-terminal "\e]50;CursorShape=0\x7")))
	  ;; Set to thin line in insert-state
	  (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e]50;CursorShape=1\x7")))
	  ;; Set to box outside of insert-state
	  (add-hook 'evil-insert-state-exit-hook  (lambda () (send-string-to-terminal "\e]50;CursorShape=0\x7"))))
	;; matchit: A way to jump between matched tags (parens, html tags etc)
	(use-package evil-matchit
	  :init
	  (global-evil-matchit-mode 1))

	;; nerd-commenter: An easy way to comment/uncomment lines
	(use-package evil-nerd-commenter
	  :init
	  ;; Bind "M-;" to commenting the selected lines (vim-style)
	  (evilnc-default-hotkeys)
	  )
	))

(use-package company
  :diminish company-mode
  :config (progn
			(setq company-backends (delete 'company-semantic company-backends))
			;; Tab completion - insert tab at start of line, complete otherwise
			(setq-default tab-always-indent 'complete)
			(setq-default c-tab-always-indent 'complete)
			(global-company-mode t)))

;; Code Style
(setq c-default-style "k&r")
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode t)
;;(add-hook 'prog-mode-hook 'c-guess-buffer)

;; Minibuffer
;; Save mini buffer history
;; savehist-file needs to be set _before_ enabling the mode
(setq savehist-file (expand-file-name "emacs/history" user-cache-directory ))
(savehist-mode t)
(setq history-length t)
(setq history-delete-duplicates t)
;; History search (like readline's history-search)
(bind-key "<up>" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<down>" 'next-complete-history-element minibuffer-local-map)
(bind-key "<escape>" 'keyboard-quit minibuffer-local-map)
(bind-key "<escape>" 'keyboard-quit minibuffer-local-ns-map)
(bind-key "<escape>" 'keyboard-quit minibuffer-local-completion-map)
(bind-key "<escape>" 'keyboard-quit minibuffer-local-must-match-map)
(bind-key "<escape>" 'keyboard-quit minibuffer-local-isearch-map)

(setq ido-save-directory-list-file (expand-file-name "emacs/ido.last" user-cache-directory))
;; ido-mode: Nicer minibuffer completion
(use-package ido
  :init
  (progn
	(setq ido-enable-flex-matching t)
	(setq ido-everywhere t)
	(ido-mode t)
	;; Let us cycle through instead of opening up a buffer with candidates!
	(setq ido-cannot-complete-command 'ido-next-match)
	;; (ido-vertical-mode)
	(bind-key "<escape>" 'keyboard-quit ido-common-completion-map)
	;; Make M-x use ido as well
	(global-set-key
	 "\M-x"
	 (lambda ()
	   (interactive)
	   (call-interactively
		(intern
		 (ido-completing-read
		  "M-x "
		  (all-completions "" obarray 'commandp))))))
	(use-package ido-ubiquitous
	  :init
	  (ido-ubiquitous-mode t))
  ))


;; Start in org-mode
;; (setq initial-major-mode 'org-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(autoload 'mu4e-compose-mode "muC" "mu4e compose mode." t)
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
(require 'generic-x)

;; TeX
(setq tex-dvi-view-command "xdg-open")
(setq TeX-PDF-mode t)
(setq TeX-view-program-selection
      '((output-pdf "xdg-open")
		(output-html "xdg-open")
		(output-dvi "xdg-open")))

;; Backup/Autosave etc
(setq version-control t ;; versioned backups
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2)

;; Move temporary files out of the way (to $XDG_CACHE_HOME/emacs/$type)
(setq backup-dir (expand-file-name "emacs/backup" user-cache-directory))
(setq backup-by-copying-when-linked t)
(make-directory backup-dir t)
(setq backup-directory-alist
      `((".*" . ,backup-dir))
      )
(setq autosave-dir (expand-file-name "emacs/save" user-cache-directory))
(setq auto-save-list-file-prefix autosave-dir)
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms
      `((".*" ,autosave-dir t))
      )
(setq undo-dir (expand-file-name "emacs/undo" user-cache-directory))
(setq undo-tree-auto-save-history t)
(make-directory undo-dir t)
(setq undo-tree-history-directory-alist
      `((".*" . ,undo-dir))
      )

;; Save point position between sessions
;; Included in emacs, no need to use-package
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "emacs/places" user-cache-directory))

(setq bookmark-default-file (expand-file-name "emacs/bookmark" user-cache-directory))
(setq delete-old-versions -1)

;; Make backups even when the file is in version control
(setq vc-make-backup-files t)

;; Disable annoying "Should I follow a symlink to a git-controlled source file" question
(setq vc-follow-symlinks t)
(setq tab-stop-list (number-sequence 4 200 4))
(setq flymake-gui-warnings-enabled nil)

;; Highlight TODO, FIXME, STUB, etc
;; FIXME:
;; TODO:
(add-hook 'prog-mode-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\<\\(FIXME\\|BUG\\):" 1 font-lock-warning-face t)))
			(font-lock-add-keywords nil
									'(("\\<\\(WTF\\|TODO\\|STUB\\|HACK\\):" 1 font-lock-keyword-face t))
									)
			;;(flyspell-prog-mode) ; Spellcheck comments and strings
			;;(whitespace-mode) ; Show whitespace (not sure if this isn't too annoying)
			(subword-mode))) ; Count CamelCase as two words

;; Spelling
;;(setq-default ispell-program-name "aspell")
;;(setq ispell-list-command "list")
;; (setq ispell-alternate-dictionary "english")
;; (setq ispell-dictionary "deutsch")
;; (add-hook 'text-mode-hook 'set-dict)
;; (add-hook 'prog-mode-hook 'set-dict)
(setq scroll-step 1)
(setq scroll-conservatively 1000)

(setq save-interprogram-paste-before-kill t)

;; Linux kernel coding style
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))
(add-hook 'c-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(c-set-style "linux-tabs-only")))

;; Open files in external programs
(use-package openwith
  :init
  (progn
	(setq openwith-associations '(("\\.pdf\\'" "xdg-open" (file))))
	(openwith-mode t)
	))

(use-package projectile
  ;; :require neotree
  :diminish projectile-mode
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (projectile-global-mode))

;;; Keybindings
(add-hook 'prog-mode-hook
		  (lambda ()
			(local-set-key [f8] 'compile)
			))

(global-set-key [f5] 'revert-buffer)
;;esc quits
(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (my-kbd "o") 'xdg-open)
(global-set-key (kbd "<backtab>") 'indent-according-to-mode)
;; Allow elisp evaluation in all major modes
;; For quick reconfiguration
(global-set-key (my-kbd "C-x C-e") 'eval-last-sexp)
(global-set-key (kbd "M-o") 'other-window)
;; I literally only start overwrite-mode by accident
(global-set-key (kbd "<insertchar>") nil)

(add-hook 'dired-mode-hook (lambda ()
							 (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)
							 (autoload 'wdired-change-to-wdired-mode "wdired" nil t)
                             (bind-key "U" 'dired-up-directory dired-mode-map)
                             (bind-key "/" 'dired-isearch-filenames dired-mode-map)))

;; This seems to be disabled by something else before it
(line-number-mode)

(use-package hydra
  :init
  (progn
	(defhydra hydra-window-size (:color amaranth)
	  "Resize window: h:Left l:Right j:Down k:Up
   q: Quit"
	  ;; These have problems when the selected window isn't top left
	  ("j" (lambda () (interactive) (enlarge-window 1)) nil)
	  ("k" (lambda () (interactive) (enlarge-window -1)) nil)
	  ("h" (lambda () (interactive) (enlarge-window -1 t)) nil)
	  ("l" (lambda () (interactive) (enlarge-window 1 t)) nil)
	  ("q" nil "cancel" :exit t))
	(global-set-key (my-kbd "r") 'hydra-window-size/body)

	(defhydra hydra-window (:color amaranth)
	  "
   Split:  _v_ert   _x_:horz
   Delete: _o_nly   _d_el
   Select: _h_:Left _l_:Right _j_:Down _k_:Up 
   Scroll other window: _C-j_:Down _C-k_:Up
   _r_: Resize
   "
	  ("h" windmove-left nil)
	  ("j" windmove-down nil)
	  ("k" windmove-up nil)
	  ("l" windmove-right nil)
	  ("v" (lambda ()
			 (interactive)
			 (split-window-right)
			 (windmove-right)) nil)
	  ("x" (lambda ()
			 (interactive)
			 (split-window-below)
			 (windmove-down)) nil)
	  ("o" delete-other-windows :color blue)
	  ("a" ace-window "ace")
	  ("s" ace-swap-window "swap")
	  ("d" ace-delete-window nil)
	  ("i" ace-maximize-window "ace-one" :color blue)
	  ("b" ido-switch-buffer "buf")
	  ("r" hydra-window-size/body "resize" :exit t)
	  ("C-j" scroll-other-window nil)
	  ("C-k" scroll-other-window-down nil)
	  ("q" nil "cancel"))
	(global-set-key (my-kbd "w") 'hydra-window/body)

	(defhydra hydra-emms (:color amaranth)
	  "music"
	  ;; Unfortunately emms doesn't handle unix sockets
	  ;; so the mpd connection doesn't work with my config
	  ;; ("P" (expose-partially 'my-emms-call 'emms-pause) "Pause")
	  ;; ("p" (expose-partially 'my-emms-call 'emms-previous) "Prev")
	  ;; ("n" (expose-partially 'my-emms-call 'emms-next) "Next")
	  ;; ("b" (expose-partially 'my-emms-call 'emms-smart-browse) "Browse")
	  ;; ("d" (expose-partially 'my-emms-call 'emms-play-dired) "Dired")
	  ("P" (shell-command "mpc toggle") "Toggle")
	  ("p" (shell-command "mpc prev") "Prev")
	  ("n" (shell-command "mpc next") "Next")
	  ("s" (shell-command "mpc") "Show")
	  ("l" (shell-command "mpc listall") "Listall")
	  ("q" nil "cancel"))
	(global-set-key (my-kbd "e") 'hydra-emms/body)

	(defhydra hydra-misc-modes (:color blue)
	  "Misc modes"
	  ("a" org-agenda "agenda")
	  ("A" org-capture "capture")
	  ("b" diff-buffer-with-file "diff buffer with file")
	  ("c" calc "calc")
	  ("d" dired "dired")
	  ("e" ediff "ediff")
	  ("g" magit-status "magit")
	  ("m" mu4e "mu4e")
	  ("p" list-packages "packages")
	  ("q" nil "cancel"))
	(global-set-key (my-kbd "m") 'hydra-misc-modes/body)

	(defhydra hydra-leader (:color blue)
	  "Leader"
	  ("c" copy-to-clipboard "copy")
	  ("v" paste-from-clipboard "paste")
	  ("w" hydra-window/body "window" :exit t)
	  ("m" hydra-misc-modes/body "modes" :exit t)
	  ("<left>" previous-buffer "previous buffer")
	  ("<right>" next-buffer "next buffer")
	  ("q" nil "cancel")
	  )
	(global-set-key (my-kbd "<SPC>") 'hydra-leader/body)))


(setq find-file-wildcards t)
(which-function-mode 1)
(setq-default split-width-threshold 100)

(use-package magit
  :commands magit-status
  :config
  ;; Using magit for that - vc-git takes too long to start up and lacks features
  (delq 'Git vc-handled-backends)
  ;; A list of magit's keymaps (from https://github.com/magit/evil-magit/issues/1)
  ;; (defvar magit-keymaps
  ;; '(git-commit-mode-map
  ;;   git-rebase-mode-map
  ;;   magit-blame-mode-map
  ;;   magit-blob-mode-map
  ;;   magit-branch-section-map
  ;;   magit-cherry-mode-map
  ;;   magit-commit-section-map
  ;;   magit-diff-mode-map
  ;;   magit-file-mode-map
  ;;   magit-file-section-map
  ;;   magit-hunk-section-map
  ;;   magit-log-mode-map
  ;;   magit-log-read-revs-map
  ;;   magit-log-select-mode-map
  ;;   magit-minibuffer-local-ns-map
  ;;   magit-module-commit-section-map
  ;;   magit-popup-mode-map
  ;;   magit-process-mode-map
  ;;   magit-reflog-mode-map
  ;;   magit-refs-mode-map
  ;;   magit-remote-section-map
  ;;   magit-staged-section-map
  ;;   magit-stash-section-map
  ;;   magit-stashes-section-map
  ;;   magit-status-mode-map
  ;;   magit-tag-section-map
  ;;   magit-unpulled-section-map
  ;;   magit-unpushed-section-map
  ;;   magit-unstaged-section-map
  ;;   magit-untracked-section-map
  ;;   with-editor-mode-map))
  ;; Evilify magit - unfortunately this now has a million section-maps
  (bind-key "j" 'evil-next-visual-line magit-status-mode-map)
  (bind-key "k" 'evil-previous-visual-line magit-status-mode-map)
  (bind-key "K" 'magit-discard magit-status-mode-map)
  (bind-key "j" 'evil-next-visual-line magit-untracked-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-untracked-section-map)
  (bind-key "K" 'magit-discard magit-untracked-section-map)
  (bind-key "j" 'evil-next-visual-line magit-branch-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-branch-section-map)
  (bind-key "K" 'magit-branch-delete magit-branch-section-map)
  (bind-key "j" 'evil-next-visual-line magit-remote-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-remote-section-map)
  (bind-key "j" 'evil-next-visual-line magit-tag-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-tag-section-map)
  (bind-key "K" 'magit-tag-delete magit-tag-section-map)
  (bind-key "j" 'evil-next-visual-line magit-file-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-file-section-map)
  (bind-key "K" 'magit-discard magit-file-section-map)
  (bind-key "j" 'evil-next-visual-line magit-hunk-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-hunk-section-map)
  (bind-key "K" 'magit-discard magit-hunk-section-map)
  (bind-key "j" 'evil-next-visual-line magit-staged-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-staged-section-map)
  (bind-key "K" 'magit-discard magit-staged-section-map)
  (bind-key "j" 'evil-next-visual-line magit-stashes-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-stashes-section-map)
  (bind-key "K" 'magit-stash-clear magit-stashes-section-map)
  (bind-key "j" 'evil-next-visual-line magit-stash-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-stash-section-map)
  (bind-key "K" 'magit-stash-drop magit-stash-section-map)
  (bind-key "j" 'evil-next-visual-line magit-commit-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-commit-section-map)
  (bind-key "K" 'magit-discard magit-commit-section-map)
  (bind-key "j" 'evil-next-visual-line magit-unstaged-section-map)
  (bind-key "k" 'evil-previous-visual-line magit-unstaged-section-map)
  (bind-key "K" 'magit-discard magit-unstaged-section-map)
  (bind-key "j" 'evil-next-visual-line magit-module-commit-section-map)
  (bind-key "k" 'evil-next-visual-line magit-module-commit-section-map)
  (bind-key "K" 'magit-discard magit-module-commit-section-map)
  )

;; (setq frame-resize-pixelwise t)

(setq tramp-persistency-file-name (expand-file-name "tramp" user-cache-directory))
(setq tramp-default-method "ssh")

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))
;; C-x f is usually bound to "set-fill-column", which is useless.
(global-set-key (kbd "C-x f") 'djcb-find-file-as-root)

(setq ediff-split-window-function 'split-window-horizontally)

(use-package neotree
  :init
  (global-set-key [f9] 'neotree-toggle)
  :config
  (bind-key "l" 'neotree-enter neotree-mode-map)
  ;; I'd like this to _close_ the last node
  ;; (bind-key "h" 'neotree- neotree-mode-map
  (bind-key "j" 'next-line neotree-mode-map)
  (bind-key "k" 'previous-line neotree-mode-map))

(use-package fish-mode
  :commands fish-mode)

;; This should be the last line, after all use-package calls
;; (use-package-finish)

;; Also enable 256 colors on konsole
(when (string= (tty-type) "konsole-256color")
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
