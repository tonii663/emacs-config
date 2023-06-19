(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(straight-use-package 'use-package) ; Base package for package configuration
(use-package straight
  :custom
  (straight-use-package-by-default t "Auto install package use in the configuration"))


;; Configure package.el to include MELPA.
;; (require 'package)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
;; (when (not (package-installed-p 'use-package))
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; Quelpa
;; (use-package quelpa
;;   :custom
;;   (quelpa-update-melpa-p nil "Don't update the MELPA git repo"))
;; (use-package quelpa-use-package)

(setq use-package-always-defer t)


(defvar rebuild-config nil "If true rebuilds the config org file otherwise use the exisiting config.el")

(if rebuild-config
	(progn
	  (message "Rebuilt")
	  (org-babel-load-file (expand-file-name "config-new.org" user-emacs-directory)))
  (progn
	(message "Load exisiting config")
	(load-file "~/.emacs.d/config-new.el")))

;; (org-babel-load-file (expand-file-name "config-new.org" user-emacs-directory))

;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)
;; (load-file "~/.emacs.d/config-new.el")

;; (put 'narrow-to-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)
