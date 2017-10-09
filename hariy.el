(defun configure-package-sources (&optional unstable noimage protocol)
  "Configure package sources.

* [Elpa](https://elpa.gnu.org/)
* [Melpa](https://melpa.org/)
* [Org](https://orgmode.org/elpa.html)
"
  (let* ((elpa               '("gnu" . "http://elpa.gnu.org/packages/"))
	 (elpa-image         '("gun" . "http://elpa.emacs-china.org/gnu/"))
	 (org                '("org" . "http://orgmode.org/elpa.html"))
	 (org-image          '("org" . "http://elpa.emacs-china.org/org/"))
	 (melpa-stable       '("melpa-stable" . "http://stable.melpa.org/packages/"))
	 (melpa-stable-image '("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/"))
	 (melpa              '("melpa" . "http://melpa.org/packages/"))
	 (melpa-image        '("melpa" . "http://elpa.emacs-china.org/melpa/"))
	 (env-noimage  "EMACS_PACKAGE_NOT_USE_IMAGE")
	 (env-unstable "EMACS_PACKAGE_USE_UNSTABLE")
	 (is-noimage  (or noimage (getenv env-noimage)))
	 (is-unstable (or unstable (getenv env-unstable))))
    (if (not is-noimage)
	(list elpa
	      (if is-unstable melpa melpa-stable))
      (list elpa-image
	    (if is-unstable melpa-image melpa-stable-image)))))

(defun reset-package-source ()
  "Reset package source."
  (interactive)
  (setq package-archives (configure-package-sources))
  (package-initialize))

(defun require-or-install (feature &optional filename)
  "Install package from sources before require failed."
  (unless (funcall 'require feature filename t)
    (progn
      (package-install feature)
      (funcall 'require feature filename))))


;;; Package

(require 'package)
(reset-package-source)
(global-set-key (kbd "C-c p l") 'package-list-packages-no-fetch)
(global-set-key (kbd "C-c p r") 'reset-package-source)
(global-set-key (kbd "C-c p i") 'package-install)
;; TODO

;;; Preload
;; dash-2.12.0
(require-or-install 'dash)
(eval-after-load 'dash (dash-enable-font-lock))

;; edit && editorconfig
(require-or-install 'editorconfig)
(editorconfig-mode 1)
(setq require-final-newline t)
(setq visible-bell t)
(setq load-prefer-newer t)

;; auto save && auto backup
(global-auto-revert-mode 1)
(setq make-backup-files nil)
(setq save-interprogram-paste-before-kill t)

;; undo && redo
(global-set-key (kbd "M-/") 'hippie-expand)
(setq kill-ring-max 200)

;; unique buffer names
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; yes/no => y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; maker && region && parens
(require-or-install 'expand-region)
(require-or-install 'multiple-cursors)
(require-or-install 'smartparens)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-?") 'mc/mark-all-like-this)
(global-set-key (kbd "C-=") 'er/expand-region)
(show-smartparens-global-mode 1)
(smartparens-global-mode 1)
(show-paren-mode 1)
(setq show-paren-style 'parentheses)


;; fast move && jump
(require-or-install 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-.") 'imenu)

;; buffer && minibuffer
(require-or-install 'smex)
(require-or-install 'ido-vertical-mode)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(ido-mode 1)
(ido-everywhere 1)
(setq-default ido-enable-flex-matching t)
(ido-vertical-mode 1)  
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; unicode
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq-default file-name-coding-system 'utf-8)

;; ui
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq frame-title-format "emacs@%b")
(mouse-avoidance-mode 'animate)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(require-or-install 'fill-column-indicator)
(setq default-fill-column 60)
(setq-default fci-rule-column 80)

;;; Javascript
(defun bind-nodejs-repl-keymap ()
  ""
  (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
  (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

(defun lang-javascript ()
  ""
  (require-or-install 'nodejs-repl)
  (add-hook 'js-mode-hook
	    (lambda ()
	      (bind-nodejs-repl-keymap)
	      ))
  )
(run-with-timer 0 nil 'lang-javascript)
