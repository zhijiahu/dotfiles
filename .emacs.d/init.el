;; Add repository
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; Load theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; Load custom lisp scripts
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Show line number
(global-linum-mode t)

;; Disable bars
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Saving config
(setq backup-directory-alist
`((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))
(setq delete-by-moving-to-trash t)
(desktop-save-mode 1)

;; Default directories
(setq default-directory (concat (getenv "HOME") "/"))

;; Scroll settings
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;; Shell settings
(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)

;; Dire settings
(setq dired-dwim-target t)

;; C++ settings
(setq c-default-style "linux"
      c-basic-offset 4)

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

;; Use ido mode
(ido-mode 1)

;; Use sr speed bar
(require 'sr-speedbar)
(global-set-key (kbd "C-M-m") 'sr-speedbar-toggle)

;; Insert spaces for tabs
(setq-default indent-tabs-mode nil)
