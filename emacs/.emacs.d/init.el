;;;Emacs configuration
;; (setq frame-resize-pixelwise t)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

;; The normal default is fundamental-mode, which does basically nothing
;; Orgmode is also an option, but that's slow to load.
(setq-default major-mode #'text-mode)

;; The default setting causes lots of gc-runs during startup
;; This shaves off about a third of my startup time
(setq gc-cons-percentage 0.80)

;; Ask for y/n instead of "yes"/"no"
(defalias #'yes-or-no-p #'y-or-n-p)

;; Don't warn about "Package cl is deprecated" on startup.
;; I'm not the one to fix it!
(setq byte-compile-warnings '(cl-functions))

(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist tmp--file-name-handler-alist)))

(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t ; Don't show buffer list when opening multiple files
      inhibit-default-init t ; Don't read default.el - I like my settings to be mine
      initial-scratch-message "")

;; Kill the "Have you HURD of our savior GNU" message with fire
(fset 'display-startup-echo-area-message #'ignore)

;; Don't warn about advice
(setq ad-redefinition-action #'accept)

;;; File Structure
;; My custom stuff goes to .emacs.d/mystuff
(add-to-list 'load-path (expand-file-name "mystuff" user-emacs-directory))
;; All my packages go to XDG_DATA_HOME/emacs
(defconst user-data-directory (if (getenv "XDG_DATA_HOME") (getenv "XDG_DATA_HOME") "~/.local/share"))
(setq package-user-dir (expand-file-name "emacs" user-data-directory))
;; Cache files (e.g. history, autosave, undo) go to XDG_CACHE_HOME/emacs
(defconst user-cache-directory (expand-file-name "emacs" (if (getenv "XDG_CACHE_HOME") (getenv "XDG_CACHE_HOME") "~/.cache")))

;; I hate custom
(setq custom-file (expand-file-name "custom" user-cache-directory))

(dolist (dir load-path)
  (make-directory dir t))

(defconst faho/config-files '("mystuff/mymail.el"
                              "mystuff/myorg.el"
                              "mystuff/myutil.el"))

;; Yes, that's a hardcoded config repo
(defconst faho/config-url "https://raw.githubusercontent.com/faho/config/master/emacs/.emacs.d/")

;; Download all files that aren't already here
(dolist (file faho/config-files)
  (if (not (file-exists-p (expand-file-name file user-emacs-directory)))
      (url-copy-file (concat faho/config-url file) (expand-file-name file user-emacs-directory))))

;; Utility functions - always load
(require 'myutil)

(setq load-prefer-newer t)

(require 'package)
(setq tls-checktrust t)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ;; Either melpa or marmalade are needed for goto-chg, but melpa contains moar stuff
        ("melpa" . "https://melpa.org/packages/") ;; This contains packages from git
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("melpa" . 0)
        ("gnu" . 20)))
(setq package-enable-at-startup nil)
(package-initialize)

;; Install use-package because that installs everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; These autoload through use-package
;; Don't error if not found
(require 'myorg nil t)

;;; Aesthetics
;; Mode line
(use-package smart-mode-line
  :config
  (progn
    (setq sml/show-encoding nil)
    (setq sml/theme 'respectful)
    (setq sml/no-confirm-load-theme t)
    (sml/setup)
    ))

(column-number-mode t)
(global-hl-line-mode t)
;; Colors
;; List of okay themes:
;; cyberpunk
;; klere
;; wheatgrass ir-black ir_black reverse ample flatland-black
;; idea-darkula
;; Okay with 24bit (emacs 26.1):
;; atom-one-dark hamburg
;; molokai material planet
(use-package cyberpunk-theme
  :config
  (load-theme 'cyberpunk t)
  )

;; This should be outside of use-package so we can add to it from outside
(setq linum-disabled-modes-list '(shell-mode inferior-emacs-lisp-mode))
;; (use-package nlinum-relative
;;   :mode "prog-mode"
;;   :init
;;   (use-package nlinum)
;;   (add-hook 'prog-mode-hook (lambda ()
;;                               (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
;;                                 (nlinum-relative-mode 1)))))
(setq-default display-line-numbers 'relative)
(global-display-line-numbers-mode t)
(setq diff-switches "-u")

;; Highlight matching parens in different colors
(use-package rainbow-delimiters
  :mode "prog-mode"
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(show-paren-mode t) ;; Highlight matching parens
(setq show-paren-when-point-in-periphery t
      show-paren-when-point-inside-paren t)

;; Display current function in modeline (for supporting major-modes)
(which-function-mode 1)

;; Make modeline less crowded by removing minormodes
(use-package diminish)

(use-package avy)
;;; Minor Modes:
;; Vim-Mode
(setq evil-want-keybinding nil)
(use-package evil
  :diminish (undo-tree-mode . "")
  :config
  (evil-mode t)
  ;; Don't signal state in echo area
  (setq evil-echo-state nil)
  ;; Remove some unhelpful bindings from evil so they are available elsewhere
  (bind-keys :map evil-motion-state-map
             ;; "{" and "}" are badly reachable on QWERTZ, "ö" and "Ö" are unused
             ("ö" . evil-forward-paragraph)
             ("Ö" . evil-backward-paragraph)
             ("RET" . nil)
             ("C-e" . nil)
             ;; This is supposed to be bound to C-i, but that's TAB in a terminal
             ("M-o" . evil-jump-forward)
             ("TAB" . nil))
  (bind-key "C-e" nil evil-insert-state-map)
  ;; Enable C-w C-w for window-switching _everywhere_
  ;; First remove the default binding (kill-region) so C-w can be used as a prefix
  (bind-key "C-w" nil)
  (bind-key "C-w w" 'evil-window-next)
  (bind-key "C-w C-w" 'evil-window-next)
  (bind-keys :map evil-normal-state-map
             ("Q" . avy-goto-char-2)
             ("U" . undo-tree-visualize)
             ("TAB" . nil)
             ("C-M-m" . scroll-other-window-down) ;; Yes, this scrolls up
             ;; My simple replacement for evil-leader
             ("SPC" . hydra-leader/body)
             ("q" . delete-window))
  (bind-keys :map evil-visual-state-map
             ("SPC" . hydra-leader/body)
             ("Q" . avy-goto-char-2)
             ("U" . undo-tree-visualize)
             ("u" . undo-tree-undo))
  (setq evil-want-visual-char-semi-exclusive t)
  (use-package origami
    :config
    ;; (let ((fish-parser (origami-markers-parser "begin" "end")))
    ;;   (add-to-list 'origami-parser-alist '(fish-mode . fish-parser)))
    :init
    (global-origami-mode))
  (evil-define-key 'motion Info-mode-map
    "\t" 'Info-next-reference
    "n" 'Info-history-back
    "/" 'Info-history-forward
    "l" 'Info-next
    "h" 'Info-prev
    "d" 'Info-directory
    "y" 'evil-yank)
  ;; Don't use C-i since that's TAB in a terminal
  (setq evil-want-C-i-jump nil)
  ;; http://emacs.stackexchange.com/questions/3358/how-can-i-get-undo-behavior-in-evil-similar-to-vims
  (setq evil-want-fine-undo 'fine)
    ;;; EVILIFY MODES
  (bind-keys :map package-menu-mode-map
             ("j" . evil-next-line)
             ("k" . evil-previous-line)
             ("g g" . evil-goto-first-line)
             ("G" . evil-goto-line)
             ("/" . evil-search-forward)
             ("n" . evil-search-next)
             ("N" . evil-search-previous))
  ;; From spacemacs
  ;; default state for additional modes
  (dolist (mode '(magit-popup-mode
                  git-rebase-mode
                  comint-mode
                  shell-mode
                  term-mode
                  ;; neotree-mode
                  magit-popup-sequence-mode))
    (add-to-list 'evil-emacs-state-modes mode))
  ;; Set cursor when in evil-insert mode
  ;; This works for KDE4 Konsole
    ;; Reset on exit
    (add-hook 'kill-emacs-hook (lambda () (unless (display-graphic-p) (send-string-to-terminal "\e]50;CursorShape=0\x7"))))
    ;; Set to thin line in insert-state
    (add-hook 'evil-insert-state-entry-hook (lambda () (unless (display-graphic-p) (send-string-to-terminal "\e]50;CursorShape=1\x7"))))
    ;; Set to box outside of insert-state
    (add-hook 'evil-insert-state-exit-hook  (lambda () (unless (display-graphic-p) (send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  ;; matchit: A way to jump between matched tags (parens, html tags etc)
  (use-package evil-matchit
    :init
    (global-evil-matchit-mode 1))
  (use-package evil-commentary
    :diminish evil-commentary-mode
    :init
    (evil-commentary-mode))
  (use-package evil-surround
    :config
   (global-evil-surround-mode t))
  )

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package company
  :diminish company-mode
  :config (progn
            ;; Semantic is slow
            (setq company-backends (delete 'company-semantic company-backends))
            (use-package company-shell
              :ensure t
              :mode "fish-mode"
              :init
              (add-to-list 'company-backends 'company-fish-shell))
            ;; Tab completion - insert tab at start of line, complete otherwise
            (setq-default tab-always-indent #'complete
                          c-tab-always-indent #'complete)
            (global-company-mode t)))

;; Code Style
(setq-default tab-width 4
              standard-indent 4
              indent-tabs-mode nil
              c-basic-offset 4)

;; Minibuffer
;; Save mini buffer history
;; savehist-file needs to be set _before_ enabling the mode
(setq savehist-file (expand-file-name "history" user-cache-directory )
      history-length t
      history-delete-duplicates t
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history))
(savehist-mode t)
;; History search (like readline's history-search)
(bind-key "<up>" 'previous-complete-history-element minibuffer-local-map)
(bind-key "<down>" 'next-complete-history-element minibuffer-local-map)

(use-package counsel
  :diminish ivy-mode
  :diminish counsel-mode
  :bind ("M-y" . counsel-yank-pop)
  ("C-x b" . ivy-switch-buffer)
  :init
  ;; I'm not interested in the number of candidates
  (setq ivy-count-format "")
  (counsel-mode)
  ;; Enter goes into a directory, it doesn't open with dired.
  ;; To open dired, select the "." entry when in the directory.
  (bind-keys :map ivy-minibuffer-map
             ("<RET>" . ivy-alt-done)
             ("C-f" . ivy-immediate-done))
  )
(use-package swiper
  :bind ("C-s" . swiper))

;; Misc language modes
(autoload #'lua-mode "lua-mode" "Lua editing mode." t)
(autoload #'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))
;; Adds basic highlighting for a bunch of files
(require #'generic-x)

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
(setq backup-dir (expand-file-name "backup" user-cache-directory))
(setq backup-by-copying-when-linked t)
(make-directory backup-dir t)
(setq backup-directory-alist `((".*" . ,backup-dir)))
(setq autosave-dir (expand-file-name "save" user-cache-directory))
(setq auto-save-list-file-prefix autosave-dir)
(make-directory autosave-dir t)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq undo-dir (expand-file-name "undo" user-cache-directory))
(make-directory undo-dir t)
(setq undo-tree-history-directory-alist `(("." . ,undo-dir)))
(setq undo-tree-auto-save-history t)

;; Some more paths to check
;; From https://github.com/tarsius/no-littering/blob/master/no-littering.el
;; (setq abbrev-file-name                 (var "abbrev.el"))
;; (setq bookmark-default-file            (var "bookmark-default.el"))
;; (eval-after-load 'desktop
;;   `(make-directory ,(var "desktop/") t))
;; (setq desktop-path                     (list (var "desktop/")))
;; (setq eshell-directory-name            (var "eshell/"))
;; (eval-after-load 'eww
;;   `(make-directory ,(var "eww/") t))
;; (setq eww-bookmarks-directory          (var "eww/"))
;; (setq gamegrid-user-score-file-directory (var "gamegrid-user-score/"))
;; (setq org-clock-persist-file           (var "org/clock-persist.el"))
;; (setq org-id-locations-file            (var "org/id-locations.el"))
;; (setq org-registry-file                (var "org/registry.el"))
;; (setq recentf-save-file                (var "recentf-save.el"))
;; (setq save-place-file                  (var "save-place.el"))
;; (setq savehist-file                    (var "savehist.el"))
;; (setq semanticdb-default-save-directory (var "semantic/"))
;; (setq shared-game-score-directory      (var "shared-game-score/"))
;; (setq tramp-persistency-file-name      (var "tramp-persistency.el"))
;; (setq trash-directory                  (var "trash/"))
;; (setq emms-directory                   (var "emms/"))
;; (setq irony-user-dir                   (var "irony/"))
;; (setq mc/list-file                     (var "mc-list.el"))
;; (setq persistent-scratch-save-file     (var "persistent-scratch.el"))
;; (setq yas-snippet-dirs                 (list (etc "yas-snippets/") 'yas-installed-snippets-dir))

;; Save point position between sessions
;; (use-package saveplace
;;   :ensure nil
;;   :init
;;   (setq-default save-place t)
;;   (setq save-place-file (expand-file-name "places" user-cache-directory)))

(setq bookmark-default-file (expand-file-name "bookmark" user-cache-directory))
(setq delete-old-versions -1)

;; Make backups even when the file is in version control
(setq vc-make-backup-files t)

;; Disable annoying "Should I follow a symlink to a git-controlled source file" question
(setq vc-follow-symlinks t)
(setq tab-stop-list (number-sequence 4 200 4))
(setq flymake-gui-warnings-enabled nil)

;; Highlight TODO, FIXME, STUB, etc
(add-hook #'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|BUG\\):" 1 font-lock-warning-face t)))
            (font-lock-add-keywords nil
                                    '(("\\<\\(WTF\\|TODO\\|STUB\\|HACK\\):" 1 font-lock-keyword-face t))
                                    )
            (subword-mode))) ; Count CamelCase as two words

(setq scroll-step 1)
(setq scroll-conservatively 1000)

(setq save-interprogram-paste-before-kill t)

(use-package projectile
  :bind ("C-x o" . projectile-find-file-in-known-projects)
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
        projectile-cache-file (expand-file-name "projectile.cache" user-cache-directory)
        projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory))
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

;;; Keybindings
(bind-key "<f8>" 'compile prog-mode-map)

;; Misc global bindings
(bind-keys
 ("<f5>" . revert-buffer)
 ;;esc quits
 ("<escape>" . keyboard-quit)
 ("C-x C-b" . ibuffer)
 ((faho/kbd "o") . xdg-open)
 ("<backtab>" . indent-according-to-mode)
 ;; Allow elisp evaluation in all major modes
 ;; For quick reconfiguration
 ((faho/kbd "C-x C-e") . eval-last-sexp)
 ;; C-x f is usually bound to "set-fill-column", which is useless.
 ("C-x f" . 'djcb-find-file-as-root)

 ;; I literally only start overwrite-mode by accident
 ("<insertchar>" . nil))

(use-package dired
  :ensure nil ;; Included in emacs
  :commands dired
  :config
  ;; (dired-hide-details-mode)
  ;; Would be nice, but it's a wiki package
  ;; (use-package dired+)
  (bind-keys :map dired-mode-map
             ("<SPC>" . hydra-leader/body)
             ("r" . wdired-change-to-wdired-mode)
             ("U" . dired-up-directory)
             ("/" . dired-isearch-filenames)))

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
    (bind-key (faho/kbd "r") 'hydra-window-size/body)

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
      ("b" ivy-switch-buffer "buf")
      ("r" hydra-window-size/body "resize" :exit t)
      ("C-j" scroll-other-window nil)
      ("C-k" scroll-other-window-down nil)
      ("q" nil "cancel"))
    (bind-key (faho/kbd "w") 'hydra-window/body)

    (defhydra hydra-emms (:color amaranth)
      "music"
      ("P" (shell-command "mpc toggle") "Toggle")
      ("p" (shell-command "mpc prev") "Prev")
      ("n" (shell-command "mpc next") "Next")
      ("s" (shell-command "mpc") "Show")
      ("l" (shell-command "mpc listall") "Listall")
      ("q" nil "cancel"))
    (bind-key (faho/kbd "e") 'hydra-emms/body)

    (defhydra hydra-misc-modes (:color blue)
      "Misc modes"
      ("a" org-agenda "agenda")
      ("A" org-capture "capture")
      ("b" diff-buffer-with-file "diff buffer with file")
      ("c" calc "calc")
      ("C" faho/configure "configure")
      ("d" dired "dired")
      ("e" ediff "ediff")
      ("g" magit-status "magit")
      ("p" list-packages "packages")
      ("q" nil "cancel"))
    (bind-key (faho/kbd "m") 'hydra-misc-modes/body)

    (defhydra hydra-leader (:color blue)
      "l" ;; Yeah, this takes too much space, unfortunately it can't be empty
      (":" counsel-M-x "M-x")
      ("b" ivy-switch-buffer "Buffer")
      ("f" counsel-find-file "File")
      ("F" projectile-find-file-in-known-projects "Known files")
      ("c" copy-to-clipboard "Copy")
      ("v" paste-from-clipboard "Paste")
      ("w" hydra-window/body "Window" :exit t)
      ("a" hydra-misc-modes/body "Applications" :exit t)
      ("h" windmove-left nil :exit nil)
      ("j" windmove-down nil :exit nil)
      ("k" windmove-up nil :exit nil)
      ("l" windmove-right nil :exit nil)
      ("H" previous-buffer "Prev buffer" :exit nil)
      ("L" next-buffer "Next buffer" :exit nil)
      ("K" which-key-show-top-level "show Keys")
      ("y" counsel-yank-pop "Killring")
      ("x" counsel-M-x "Command")
      ("q" nil "cancel")
      ("C-@" set-mark-command "Mark")
      )

    ;; Bind ctrl-space, which should work everywhere.
    ;; TODO: It might be nice to call set-mark-command or similar if the mark is active.
    (bind-key "C-@" 'hydra-leader/body)
    (bind-key "C-SPC" 'hydra-leader/body)
    ;; Also bind C-c space as a fallback.
    (bind-key (faho/kbd "<SPC>") 'hydra-leader/body)))

(setq find-file-wildcards t)
(setq-default split-width-threshold 100)

;; Remove some vc stuff to improve performance
;; This is better handled by magit (for git),
;; but even without it I don't use vc, so this is useless
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq vc-handled-backends nil)

(use-package magit
  :commands magit-status
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; (use-package evil-magit)
  (bind-keys :map magit-mode-map
             ("<SPC>" . hydra-leader/body))
  )

;; This actually goes through the shell
;; since I use fish, the default is borked.
(setq grep-find-template
      "find . <X> -type f <F> -exec grep <C> -nH -e <R> \\{\\} +")

(setq tramp-persistency-file-name (expand-file-name "tramp" user-cache-directory))
(setq tramp-default-method "ssh")

(setq ediff-split-window-function #'split-window-horizontally)
;; Always use a separate window, not frame, for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package fish-mode
  :mode ("\\.fish\\'" . fish-mode)
  :interpreter ("fish" . fish-mode)
  :commands fish-mode
  :config
  ;; Fish style is 4-space indentation
  (setq-default indent-tabs-mode nil)
  )

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :commands rust-mode
  :config
  (setq-default indent-tabs-mode nil)
  )

(use-package cmake-mode
  :mode ("\\CMakeLists.txt\\'" . cmake-mode)
  :commands cmake-mode
  :config
  ;; 4-space indentation
  (setq-default indent-tabs-mode nil)
  )

(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (setq web-mode-enable-css-colorization t))

(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 0.3)
  (which-key-mode)
  :bind ("<f7>" . which-key-show-top-level)
  )

(use-package flycheck
  :diminish flycheck-mode
  :config
  ;; I'm not writing a package, I don't need headers and footers and such
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc rust-cargo sh-shellcheck))
  :init
  (add-hook #'prog-mode-hook 'flycheck-mode))

;; This is needed to make sentence movement work with evil
(setq sentence-end-double-space nil)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (use-package lsp-mode
;;   :init
;;   (use-package lsp-python)
;;   :config
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-mode t)
;;               ))
;;   )

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t))
  :bind
  (:map global-map
        ("<f9>"        . treemacs)
        ("M-0"       . treemacs-select-window)
        ("C-c 1"     . treemacs-delete-other-windows)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))
