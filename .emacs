
;; If I customize a setting from the UI, DO NOT modify my init.el file
;; create a new file(custom-vars.el) and add it to that.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-message t)  ;; Disable startup message
(setq ring-bell-function 'ignore) ;; Disable the alert bell
(setq undo-limit 20000000)        ;; Increase amount of undos
(setq undo-strong-limit 40000000) ;; Limit the max amount of undos
(setq create-lockfiles nil)       ;; Files starting and ending with # no longer gets created
(setq-default tab-width 4)        ;; Default size for tabs
(setq-default truncate-lines t)   ;; Truncate all lines. Don't fold


(setq backup-directory-alist '(("." . "~/.emacs_saves"))) ;; Set backup directory and keep my project folders clean
(set-language-environment "UTF-8")


;; When in markdown-mode reset the line truncation mode
(add-hook 'markdown-mode-hook (lambda () (setq truncate-lines t)))


(scroll-bar-mode -1)       ;; Disable visible scrollbar
(tool-bar-mode -1)         ;; Disable the toolbar
(tooltip-mode -1)          ;; Disable tooltips
(menu-bar-mode -1)         ;; Disable menu bar
(electric-pair-mode 1)     ;; Auto create closing braces
(global-hl-line-mode)      ;; Higlight the line I am on
(show-paren-mode)          ;; Show matching parens
(display-time)             ;; Display time on mode line
(column-number-mode)       ;; Show column number
(blink-cursor-mode -1)     ;; Don't flash cursor
(save-place-mode 1)        ;; Automatically go back to the last location I was in a file.
(display-battery-mode 1)   ;; Display battery percent in battery line.

;; NOTE(afb) :: May remove
(global-auto-revert-mode 1)  ;; Automatically refresh files that were edited not in emacs
(setq global-auto-revert-non-file-buffers t)  ;; Refresh edited files when using dired.

;; Set font size
(set-face-attribute 'default nil :height 120)
;; Don't change text color when the line is highlighted. Do after turning on global-hl-line-mode.
(set-face-attribute 'hl-line nil :inherit nil :background "midnight blue")

(load-theme 'misterioso)     ;; Sets the theme
(set-cursor-color "#fff000") ;; Sets cursor color

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))			 

;; Initialize use-package on non linux machines
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel :ensure t)
(use-package diminish)
(use-package swiper :ensure t)  ;; Changes the default search, may remove


(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
		 :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (setq swiper-use-visual-line nil)
  (setq swiper-use-visual-line-p (lambda (a) nil)))



(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind(("M-x" . counsel-M-x)
		("C-x b" . counsel-ibuffer)
		("M-." . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


(use-package magit)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom (projectile-completion-system 'ivy)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p '"w:/")
    (setq projectile-project-search-path '("w:/")))
  (setq projectile-switch-project-action #'projectile-dired))

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'hybrid) ;; alien
(setq projectile-globally-ignored-file-suffixes
      '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
(setq projectile-globally-ignored-directories
      '(".git" "node_modules" "__pycache__" ".vs" "build"))
(setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package which-key
  :init(which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 2))

;; (use-package elcord
;;   :init (elcord-mode))


;; (use-package lsp-mode
;;   :commands(lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t))


;; Comment keyword higlighing
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode csharp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-link-face)
(make-face 'font-lock-debug-face)
(make-face 'font-lock-remove-face)

(mapc (lambda (mode)
		(font-lock-add-keywords
		 mode
		 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
		   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
		   ("\\<\\(LINK\\)" 1 'font-lock-link-face t)
		   ("\\<\\(REMOVE\\)" 1 'font-lock-remove-face t))))
	  fixme-modes)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-link-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-remove-face "Dark Red" nil nil t nil t nil nil)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))  ;; Treat .h files as c++
(setq c-default-style "bsd" c-basic-offset 4)           ;; Indentation settings
(c-set-offset 'case-label '+)                           ;; Switch statement indentation

(use-package auto-complete)

(use-package cc-mode
  :config
  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-ignore-case 'smart))

(defun anthony-c-mode-config ()
  (message "CC MODE"))

(add-hook 'c-mode-common-hook 'anthony-c-mode-config)

(use-package company)

(defun anthony-csharp-mode-config ()
  (ac-config-default)
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "bsd")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (add-to-list 'company-backends 'company-omnisharp))

(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key omnisharp-mode-map (kbd "C-.") 'omnisharp-go-to-definition-other-window))

(use-package compile)

;; =====================================
;; Custom Functions

(setq compilation-context-lines 0)
(setq anthony-makescript "build.bat")
(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p anthony-makescript) t
    (cd "../")
    (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive)
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory-recursive) (compile anthony-makescript))
  (other-window 1))


(defun indent-and-move-next-line()
  (interactive)
  (indent-for-tab-command)
  (next-line))

(defun indent-and-move-previous-line()
  (interactive)
  (indent-for-tab-command)
  (previous-line))

(defun new-line-below-and-move()
  (interactive)
  (move-end-of-line 1)
  (open-line 1)
  (next-line))

(defun new-line-above-and-move()
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1))

(defun search-man-for-function(name)
  (interactive "sFunction Name: \n")
  (setq cmnd (concat "wsl -e bash -c \"man " name "\""))
  (shell-command cmnd))

(defun open-file-other-window()
  (interactive)
  (ido-find-file-other-window))


;; =====================================
;; Custom key binds


;; Compiling
(global-set-key (kbd "M-m") 'make-without-asking)

;; Buffer Navigation
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; Opening buffers
(global-set-key (kbd "M->") 'open-file-other-window)

;; Execting custom commands
(global-set-key [f11] 'search-man-for-function)
(global-set-key [f10] 'shell-command)

;; Creating new lines
(global-set-key (kbd "C-o") 'new-line-below-and-move)
(global-set-key (kbd "C-S-o") 'new-line-above-and-move)

;; Indenting
(global-set-key (kbd "M-]") 'indent-and-move-next-line)
(global-set-key (kbd "M-[") 'indent-and-move-previous-line)

;; Saving
(global-set-key (kbd "M-s") 'save-buffer) 

;; Switching buffers
(global-set-key (kbd "M-<up>")    'windmove-up)
(global-set-key (kbd "M-<down>")  'windmove-down) 
(global-set-key (kbd "M-<left>")  'windmove-left) 
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)

;; Search and Replace
(global-set-key (kbd "M-/") 'query-replace)

;; Keyboard Macro
(global-set-key (kbd "C-{") 'start-kbd-macro)
(global-set-key (kbd "C-}") 'end-kbd-macro)
(global-set-key (kbd "C-'") 'call-last-kbd-macro)

;; Auto Complete
(global-set-key (kbd "M-<return>") 'auto-complete)

;; Errors
(global-set-key (kbd "C-\\") 'next-error)


;; ================================================
;; WORKING ON IT ??
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 30))


(defun efs/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-modew 1))

(use-package org
  :hook(org-mode . efs/org-mode-setup))

(use-package org-bullets
  :after org
  :hook(org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; (use-package python-mode
;;   :ensure t
;;   :hook (python-mode . lsp-deferred)
;;   :custom
;;   (pyhton-shell-interpreter "pyhton")
;;   :config
;;   (setq lsp-pyls-plugins-pycodestyle-enabled nil))

;; TODO(afb) ::  MAY NOT WANT IT
;; general
; ;(setq doom-modeline--battery-status nil)
