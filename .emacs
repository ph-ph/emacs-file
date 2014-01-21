(load "server")
(unless (server-running-p) (server-start))
(setq ns-pop-up-frames nil)
(setq dired-use-ls-dired nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; set tab width to 4 for all buffers

(global-auto-revert-mode t)

(setq-default org-startup-truncated nil)
(global-set-key [f5] 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c l") 'org-store-link)

; Move all backup files to one folder
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

; setup package installs
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; Allow to edit file permissions in dired mode
(setq wdired-allow-to-change-permissions 'advanced)

; Delete selection on edit (like any other sane editor does)
(delete-selection-mode 1)

; Use ssh for remote file opening
(setq tramp-default-method "ssh")

(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/lisp/progmodes/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/other/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/ess/lisp/")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/scala-emacs")
;; (require 'scala-mode-auto)

;; (add-hook 'scala-mode-hook
;;             '(lambda ()
;; 	       (scala-mode-feature-electric-mode)
;;                ))

;; (require 'scala-mode)
;; (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/ensime/src/main/elisp/")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(require 'ido)
(ido-mode t)
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(require 'inf-ruby)

; Install mode-compile to give friendlier compiling support!
(add-to-list 'load-path "~/.emacs.d/site-lisp/mode-compile/")
(autoload 'mode-compile "mode-compile"
   "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
 "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

; fancy js mode
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/js2-mode/")
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; js interpreter
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
 
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*[0-9]G" "..."
                     (replace-regexp-in-string ".*1G.*3G" ">" output)
)
)
)))

(add-hook 'js-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cc" 'js-send-buffer)
			    (local-set-key "\C-c\C-c" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;;(add-to-list 'grep-find-ignored-directories "lib/layouts/*")

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
      '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; require quack for better Scheme support
(require 'quack)

;; Enable git-gutter global mode
(global-git-gutter-mode +1)
(setq git-gutter:added-sign "+ ")
(setq git-gutter:deleted-sign "- ")
(setq git-gutter:modified-sign "= ")
(setq git-gutter:hide-gutter t)

;; Expand region for smarter block selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; ESS - to work with R
(require 'ess-site)

;; Configuration for multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Set font size to 14pt
(set-face-attribute 'default nil :height 140)
;; Display column in the status bar
(column-number-mode)
;; Set window margins to 0
;;(setq-default left-margin-width 0 right-margin-width 0)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(ido-enable-flex-matching t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
