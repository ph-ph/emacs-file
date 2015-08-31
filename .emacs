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
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; Configure stupid Haskell
(setq haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-program-name "ghci")

;; Configure column marker to be displayed in js mode
(require 'column-marker)
(add-hook 'js-mode-hook (lambda () (interactive) (column-marker-1 80)))


;; Pymacs craziness
;; (setenv "PYMACS_PYTHON" "/usr/local/bin/python2.7")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
;;(eval-after-load "pymacs"
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;; Install ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Set font size to 14pt
(set-face-attribute 'default nil :height 160)
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
 '(git-commit-fill-column 255)
 '(icicle-mode nil)
 '(ido-enable-flex-matching t)
 '(python-check-command
   "pylint  --msg-template=\"{path}:{line}: [{msg_id}({symbol}), {obj}] {msg}\"")
 '(scss-compile-at-save nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(web-mode-code-indent-offset 4)
 '(web-mode-markup-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:background "disabledControlTextColor"))))
 '(sp-pair-overlay-face ((t (:background "selectedTextBackgroundColor" :foreground "selectedTextColor")))))

;; Magit - better git
(require 'magit)
(global-set-key (kbd "C-x m") 'magit-status)

;; Jinja2 mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("/vamo/blueprints/content_editor/templates/" . web-mode))
(setq web-mode-engines-alist '(("jinja" . "/vamo/templates/.*\\.html$")) )
(add-to-list 'web-mode-engines-alist '("jinja" . "/vamo/blueprints/content_editor/templates/.*\\.html$"))

;; Web mode for all erb files
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'web-mode-engines-alist '("erb" . "\\.erb$"))

(defun my-python-check (command)
  "Check a Python file (default current buffer's file).
Runs COMMAND, a shell command, as if by `compile'.
See `python-check-command' for the default."
  (interactive
   (list (read-string "Check command: "
                      (or python-check-custom-command
                          (concat python-check-command " "
                                  (shell-quote-argument
                                   (or
                                    (let ((name (buffer-file-name)))
                                      (and name
                                           (file-name-nondirectory name)))
                                    "")))))))
  ;; (setq python-check-custom-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((process-environment (python-shell-calculate-process-environment))
        (exec-path (python-shell-calculate-exec-path)))
    (compilation-start command nil
                       (lambda (_modename)
                         "*Python check*"))))

(add-hook 'python-mode-hook '(lambda ()
                               (local-set-key "\C-c\C-v" 'my-python-check)
                               (local-set-key "\C-c\C-b" 'toggle-breakpoint)))

;; Markdown mode - enable by default on vamo docs
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("/vamo/docs/" . markdown-mode))

;; Undo tree mode - undo tree visualization
(require 'undo-tree)
(global-undo-tree-mode)

;; Use iPython as default Python interpreter
(when (executable-find "ipython")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; Custom command to run ipython with vamo venv
(defun run-vamo-ipython ()
  (interactive)
  (let (old-buffer-name python-shell-buffer-name)
    (setq python-shell-buffer-name "Python")
    (run-python "bash -c \"source ~/.bash_profile && workon vamo && cd /Users/dzmitry/src/vamo && foreman run ipython --profile vamo\"" nil t)
    (setq python-shell-buffer-name old-buffer-name))
  )
(global-set-key (kbd "C-x r p") 'run-vamo-ipython)

;; mode for editing scss files
(require 'scss-mode)

;; delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; highlight lines with python breakpoints
(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)
(put 'upcase-region 'disabled nil)

;; function to toggle Python breakpoint
(defun toggle-breakpoint()
  "Set/unset Python breakpoint on the current line"
  (interactive)
  (goto-char (line-beginning-position))
  (if (string-match "set_trace" (thing-at-point 'line))
      (progn
       (kill-line)
       (kill-line))
    (open-line 1)
    (insert "import ipdb; ipdb.set_trace()")
    (indent-for-tab-command))
  )

;; function to close bury other window
(defun hide-opposite-window()
  (interactive)
  (save-excursion
    (other-window 1)
    (quit-window)))

(global-set-key (kbd "C-c q") 'hide-opposite-window)

;; enable groovy mode for gradle config files
(add-to-list 'auto-mode-alist '("*.gradle" . groovy-mode))

;; icicles - package that is supposed to improve autocomplete by a lot
;; documentation: http://www.emacswiki.org/emacs/Icicles
;; Well, maybe not a by a lot - it's really complicated.
;; (require 'icicles)

;; autocomplete package: display completions where they should be
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'ruby-mode)
(add-to-list 'ac-modes 'web-mode)
;; add custom sources for autocomplete
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))
    ("django" . (ac-source-words-in-buffer))))

;; enhanced ruby mode
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

;; smartparens - because closing parentheses is too hard
(require 'smartparens-config)
(require 'smartparens-ruby)
(require 'smartparens-html)
(smartparens-global-mode t)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode web-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>")
  (sp-local-pair "{% " " %}")
  (sp-local-pair "{{ " " }}"))

;; making sure web mode doesn't conflict with smartparens
(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(defun sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))
(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; ag search
(require 'ag)
(setq ag-reuse-buffers t)
(setq ag-highlight-search t)
(defun ag-project-files-regexp (string file-type)
  "Search using ag for a given search STRING,
limited to files that match FILE-TYPE. STRING defaults to the
symbol under point.
The only difference from ag-project-files is that it treats the string
as regex.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")
                     (ag/read-file-type)))
  (apply 'ag/search string (ag/project-root default-directory) :regexp t file-type))

(defun ag-files-regexp (string file-type directory)
  "Search using ag in a given DIRECTORY for a given search STRING,
limited to files that match FILE-TYPE. STRING defaults to
the symbol under point.
The only difference from ag-files is that it treats the string
as regex.

If called with a prefix, prompts for flags to pass to ag."
  (interactive (list (ag/read-from-minibuffer "Search string")
                     (ag/read-file-type)
                     (read-directory-name "Directory: ")))
  (apply #'ag/search string directory :regexp t file-type))

(global-set-key (kbd "M-r") 'ag-project-files-regexp)

;; Projectile, to make working with projects easier
(projectile-global-mode)
(setq prjectile-enable-caching t)
(require 'grizzl)
(setq projectile-completion-system 'grizzl)
;; including flx-ido per Projectile docs suggestions
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
