;; Add repository
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'monokai-theme
                          'magit
                          'helm-projectile
                          'js2-mode,
                          'web-beautify)

;; activate installed packages
(package-initialize)

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
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

;; Insert spaces for tabs
(setq-default indent-tabs-mode nil)

;; Search with regex by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)

;; Mouse
(mouse-avoidance-mode 'animate)

;; Python settings
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq-default python-indent 4)))

(defun add-py-debug ()  
      "add debug code and move line down"  
    (interactive)  
    (move-beginning-of-line 1)  
    (insert "import pdb; pdb.set_trace();\n"))  

(local-set-key (kbd "<f9>") 'add-py-debug)

(defun remove-py-debug ()  
  "remove py debug code, if found"  
  (interactive)  
  (let ((x (line-number-at-pos))  
    (cur (point)))  
    (search-forward-regexp "^[ ]*import pdb; pdb.set_trace();")  
    (if (= x (line-number-at-pos))  
    (let ()  
      (move-beginning-of-line 1)  
      (kill-line 1)  
      (move-beginning-of-line 1))  
      (goto-char cur))))  

(local-set-key (kbd "M-<f9>") 'remove-py-debug)

;; Super + uppercase letter signifies a buffer/file
(global-set-key (kbd "C-c s")                       ;; scratch
                (lambda()(interactive)(switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c e")                       ;; .emacs
                (lambda()(interactive)(find-file "~/.emacs.d/init.el")))

;; Perforce settings
(require 'p4)

;; Revert buffer
(global-auto-revert-mode 1)

;; Helm settings
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x m") 'helm-imenu)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Projectile settings
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Use MingGW libraries if running on Windows
(if (eq system-type 'windows-nt)
    (setenv "PATH"
            (concat "C:\\MinGW\\msys\\1.0\\bin;" (getenv "PATH"))))

;; Toogle header/implementation file
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;; js2 settings
(js2-imenu-extras-mode)
