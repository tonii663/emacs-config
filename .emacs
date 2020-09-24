(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'ido)
(ido-mode t)

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
 '(package-selected-packages (quote (smex)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
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
;; Recommended
;; * Nlinim
;; * Undo Tree
;; * Switch Window

;; Set theme
;;(load-theme 'monokai)

;; Basic Initialization and UI customization
(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-hl-line-mode)
(set-face-background 'hl-line "midnight blue")
(scroll-bar-mode 0)
(show-paren-mode)
(set-cursor-color "#fff000")
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Font size
(set-face-attribute 'default nil :height 115)

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
  (delete-other-windows)
  (split-window-right)
  (ido-find-file-other-window))


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


;; Emacs backup saves
(setq backup-directory-alist '(("." . "~/.emacs_saves"))) 

;; Smex
;; Switch smex to my extended command tool
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Auto Complete
;; Configure Auto Complete to use default behaviour
(ac-config-default)

;;Auto Pair
(autopair-global-mode)

;; Auto Jump mode
(global-set-key (kbd "C->") 'ace-jump-mode)


;; **********************************************************************
;; Custom Casey Config

(require 'cc-mode)

; Bright-red TODOs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	 mode
	 '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
	fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(setq tab-width 4
      indent-tabs-mode nil)



;; TODO(afb) :: NEED TO EDIT AND SETHOW I
;; WANT IT
; Compilation
(require 'compile)

(setq compilation-context-lines 0)
(setq casey-makescript "build.bat")

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p casey-makescript) t
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
  (if (find-project-directory-recursive) (compile casey-makescript))
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)
