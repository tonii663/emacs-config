(require 'package)
;; add MELPA repository
;; (add-to-list 'package-archives
;; 			 '("MELPA Stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Load custom or manual packages
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Projectile
(require 'projectile)
(projectile-mode +1)


(require 'ido)
(ido-mode t)

;; Discord status package
(require 'elcord)
(elcord-mode)

;; Files starting and ending with # no longer gets created
(setq create-lockfiles nil)

;; =================== C Sharp Mode ======================================
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'company-mode)

(defun my-csharp-mode-setup ()

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  (global-set-key (kbd "C-.") 'omnisharp-go-to-definition-other-window)
  (add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

(eval-after-load
	'company
  '(add-to-list 'company-backends 'company-omnisharp))

;; (add-hook 'csharp-mode-hook 'flycheck-mode)

;; =======================================================================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (misterioso)))
 '(custom-safe-themes
   (quote
	("8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default)))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
	(("#3C3D37" . 0)
	 ("#679A01" . 20)
	 ("#4BBEAE" . 30)
	 ("#1DB4D0" . 50)
	 ("#9A8F21" . 60)
	 ("#A75B00" . 70)
	 ("#F309DF" . 85)
	 ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
	(projectile omnisharp treemacs s shut-up elcord go-mode ack fixmee company auto-complete undo-tree csharp-mode popup popup-complete auto-indent-mode ace-jump-mode magit smex)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(send-mail-function (quote mailclient-send-it))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#F92672")
	 (40 . "#CF4F1F")
	 (60 . "#C26C0F")
	 (80 . "#E6DB74")
	 (100 . "#AB8C00")
	 (120 . "#A18F00")
	 (140 . "#989200")
	 (160 . "#8E9500")
	 (180 . "#A6E22E")
	 (200 . "#729A1E")
	 (220 . "#609C3C")
	 (240 . "#4E9D5B")
	 (260 . "#3C9F79")
	 (280 . "#A1EFE4")
	 (300 . "#299BA6")
	 (320 . "#2896B5")
	 (340 . "#2790C3")
	 (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
	(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;
;; Packages used
;; * Auto Complete
;; * Smex
;; * monokai
;; * Ace Jump Mode
;; * Auto pair
;; * Org-Bullets
;; * Origami
;; * Undo Tree
;; * Auto Indent Mode
;; Recommended
;; * Nlinim
;; * Switch Window
;; * Imenu Anywhere

;; Set theme
;;(load-theme 'monokai)

;; Run emacs as server(Git reasons)
;;(server-start)

;; Auto indent mode
;; (auto-indent-global-mode)

;;=======================================================
;; Basic Initialization and UI customization
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-hl-line-mode)
(set-face-background 'hl-line "#ffffff")
(set-face-foreground 'highlight nil)
(set-face-background 'hl-line "midnight blue")
(scroll-bar-mode 0)
(show-paren-mode)
(set-cursor-color "#fff000")
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq-default tab-width 4)
(setq column-number-mode t)

;; Font size
(set-face-attribute 'default nil :height 120)

;;========================================================
;; Custom Functions
(defun indent-and-move-next-line()
  (interactive)
  (indent-for-tab-command)
  (next-line))
(global-set-key (kbd "M-]") 'indent-and-move-next-line)

(defun indent-and-move-previous-line()
  (interactive)
  (indent-for-tab-command)
  (previous-line))
(global-set-key (kbd "M-[") 'indent-and-move-previous-line)

(defun new-line-below-and-move()
  (interactive)
  (move-end-of-line 1)
  (open-line 1)
  (next-line))
(global-set-key (kbd "C-o") 'new-line-below-and-move)

(defun new-line-above-and-move()
  (interactive)
  (move-beginning-of-line 1)
  (open-line 1))
(global-set-key (kbd "C-S-o") 'new-line-above-and-move)

(defun open-file-other-window()
  (interactive)
  ;;(delete-other-windows)
  ;;(split-window-right)
  (ido-find-file-other-window))

(defun open-current-buffer-other-window()
  (interactive)
  (delete-other-windows)
  (split-window-right))


(defun search-man-for-function(name)
  (interactive "sFunction Name: \n")
  (setq cmnd (concat "wsl -e bash -c \"man " name "\""))
  (shell-command cmnd))

(global-set-key [f11] 'search-man-for-function)
(global-set-key [f10] 'shell-command)

;; Treemacs
(global-set-key (kbd "C-c C-d") 'treemacs-select-directory)
(global-set-key (kbd "C-c C-t") 'treemacs)

;;=========================================================
;; Custom key bindings

;; Saving
(global-set-key (kbd "M-s") 'save-buffer)

;; Switch between visible buffers
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)

;; Navigating Buffer
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)

;; File Operations
(global-set-key (kbd "M-.") 'ido-find-file)
(global-set-key (kbd "M->") 'open-file-other-window)

(global-set-key (kbd "M-/") 'query-replace)

;; Neotree
;; (global-set-key [f8] 'neotree-toggle)

;; Auto Complete
(global-set-key (kbd "M-<return>") 'auto-complete)

;;Electric Pair
(electric-pair-mode 1)

;; Origami
;; (global-set-key (kbd "C-]") 'origami-toggle-node)

;; Undo-tree
;; (global-set-key (kbd "C-?") 'undo-tree-visualize)

;;Imenu
(global-set-key (kbd "C-.") 'imenu)

;; Keyboard Macro
(global-set-key (kbd "C-{") 'start-kbd-macro)
(global-set-key (kbd "C-}") 'end-kbd-macro)
(global-set-key (kbd "C-'") 'call-last-kbd-macro)

;; Next error
(global-set-key (kbd "C-\\") 'next-error)

;; Open current buffer in other window
;; (global-set-key (kbd "C-x-v") 'open-current-buffer-other-window)

;; Switch statement indentation
(c-set-offset 'case-label '+)

;;===========================================================

;; Emacs backup saves
(setq backup-directory-alist '(("." . "~/.emacs_saves")))

;; Org Mode
;; (require 'org)
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)

;; Smex
;; Switch smex to my extended command tool
(global-set-key (kbd "C-c M-x") 'smex)
(global-set-key (kbd "M-x") 'execute-extended-command)

;; ==========================================================
;; CC Mode

;; Auto Complete
;; Configure Auto Complete to use default behaviour

(defun my-c-mode-common-hook ()
  (ac-config-default)
  (setq ac-auto-start nil)
  (setq ac-ignore-case 'smart)
  (setq c-default-style "bsd" c-basic-offset 4))

(setq c-default-style "bsd" c-basic-offset 4)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; CSharp mode
;; (defun my-csharp-mode-setup ()

;;   (use-package omnisharp
;; 	:after company
;; 	:config
;; 	(omnisharp-mode)
;; 	(add-to-list 'company-backends 'company-omnisharp))
  
  ;; (omnisharp-mode)
  ;; (company-mode)
  ;; (flycheck-mode)
  ;; (ac-config-default)

  ;; (define-key omnisharp-mode-map (kbd ".") 'omnisharp-add-dot-and-auto-complete)
  ;; (define-key omnisharp-mode-map (kbd "<C-SPC>") 'omnisharp-auto-complete)

  ;; (setq indent-tabs-mode nil)
  ;; (setq c-syntactic-indentation t)
  ;; (c-set-style "ellemtel")
  ;; (setq c-basic-offset 4)
  ;; (setq truncate-lines t)
  ;; (setq tab-width 4)
  ;; (setq evil-shift-width 4)

  ;; (setq ac-auto-start t)
  ;; (setq ac-ignore-case 'smart)

  ;; (omnisharp-mode)
  ;; (omnisharp-start-omnisharp-server)

;; (global-set-key (kbd "C-.") 'omnisharp-go-to-definition-other-window)
  ;;csharp-mode README.md recommends this too
  ;;(electric-pair-mode 1)       ;; Emacs 24
  ;;(electric-pair-local-mode 1) ;; Emacs 25
  ;; (global-auto-complete-mode t)
  ;; (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  ;; (local-set-key (kbd "C-c C-c") 'recompile))
  
;; (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)


;; GOLANG
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (setq c-default-style "bsd" c-basic-offset 4))

(add-hook 'go-mode-hook 'my-go-mode-hook)
                          




;; (setq c-default-style "k&r" c-basic-offset 4)))
;; ==========================================================

;; Auto Jump+ mode
(global-set-key (kbd "C->") 'ace-jump-mode)



;; Python mode
(add-hook 'python-mode-hook
	    (lambda ()
		    (setq-default indent-tabs-mode t)
		    (setq-default tab-width 4)
		    (setq-default py-indent-tabs-mode t)
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))



;; **********************************************************************
;; Custom Casey Config

(require 'cc-mode)

; Bright-red TODOs
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
	   ;; ("\\<\\(DEBUG\\)" 1 'font-lock-debug-face t))))
	  fixme-modes)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-link-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-remove-face "Dark Red" nil nil t nil t nil nil)
;; (modify-face 'font-lock-debug-face "Green Yellow" nil nil t nil t nil nil)

(setq tab-width 4 indent-tabs-mode nil)


;; TODO(afb) :: NEED TO EDIT AND SETHOW I
;; WANT IT
; Compilation
(require 'compile)

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
(define-key global-map "\em" 'make-without-asking)


;; Testing grep and ack
;; (grep-apply-setting 'grep-command "ack --with-filename --nofilter --nogroup ")
(defvar ack-history nil
  "History for the `ack' command.")

;; (defun ack (command-args)
;;   (interactive
;;   (let ((ack-command "ack --nofilter --nogroup --with-filename "))
;;     (list (read-shell-command "Run ack (like this): " ack-command 'ack-history))))
;; (let ((compilation-disable-input t))
;;     (compilation-start (concat command-args " < " null-device) 'grep-mode)))
(defvar ack-command "ack --nogroup --nocolor ")
(defvar ack-history nil)
(defvar ack-host-defaults-alist nil)
(defun ack ()
  "Like grep, but using ack-command as the default"
  (interactive)
  ; Make sure grep has been initialized
  (if (>= emacs-major-version 22)
      (require 'grep)
    (require 'compile))
  ; Close STDIN to keep ack from going into filter mode
  (let ((null-device (format "< %s" null-device))
        (grep-command ack-command)
        (grep-history ack-history)
        (grep-host-defaults-alist ack-host-defaults-alist))
    (call-interactively 'grep)
    (setq ack-history             grep-history
          ack-host-defaults-alist grep-host-defaults-alist)))
(put 'upcase-region 'disabled nil)
