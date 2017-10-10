

;;; Utils

(defmacro deftask (name &optional docs &rest body)
  "Run tasks."
  `(progn ,@body))

(defun ms (num)
  "Make millisec number"
  (timer-duration (concat (number-to-string num) "millisec")))

(defun configure-package-sources (&optional unstable noimage protocol)
  "Configure package sources.

* [Elpa](https://elpa.gnu.org/) - Default
* [Melpa](https://melpa.org/)   - Popular
"
  (let* ((elpa               '("gnu" . "http://elpa.gnu.org/packages/"))
	 (elpa-image         '("gun" . "http://elpa.emacs-china.org/gnu/"))
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
  "Install package when require failed."
  (unless (funcall 'require feature filename t)
    (progn
      (package-install feature)
      (funcall 'require feature filename))))



;;;; Package

(defun configure-package ()
  "Configure"
  (require 'package)
  (reset-package-source)
  (global-set-key (kbd "C-c C-p l") 'package-list-packages-no-fetch)
  (global-set-key (kbd "C-c C-p r") 'reset-package-source)
  (global-set-key (kbd "C-c C-p i") 'package-install)
  ;; TODO fetch-timeout
  )

(deftask initial-package
  "Apply package ocnfigs."
  (configure-package))



;;;; Preload

(defun configure-dash ()
  "Configure dash-2.12.0"
  (require-or-install 'dash)
  (eval-after-load 'dash (dash-enable-font-lock)))

(deftask preload-library
  "Preload utils library"
  (configure-dash)
  (require-or-install 's)
  (require-or-install 'f))


;; edit && editorconfig
(require-or-install 'editorconfig)
(require-or-install 'editorconfig-custom-majormode)
(editorconfig-mode 1)
(add-hook 'editorconfig-custom-hook 'editorconfig-custom-majormode)
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
(autoload 'zap-up-to-char "misc" "Kill up" t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

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

;; fast move && jump && search
(require-or-install 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-.") 'imenu)
(require-or-install 'ivy)
(require-or-install 'swiper)
(require-or-install 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; indent && whitespace
(setq-default indent-tabs-mode nil)

;; buffer && minibuffer
(require-or-install 'ibuffer-vc)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(require-or-install 'smex)
(require-or-install 'ido-vertical-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq enable-recursive-minibuffers t)
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
(setq default-fill-column 80)
(setq-default fci-rule-column 80)
(fci-mode 1)



;;; Projects

(defun configure-project ()
  ""
  (require-or-install 'projectile)
  (require-or-install 'ag))

(deftask project
  "Apply project configs."
  (run-with-timer (ms 100) nil 'configure-project))



;;; Javascript

(defun configure-nodejs-repl ()
  "Configure Nodejs repl"
  (let* ((command "node")
	 (arg-version (concat command " --version"))
	 (version (s-chomp (shell-command-to-string arg-version)))
	 (prompt (concat "nodejs" "(" version ")" "> ")))
    (require-or-install 'nodejs-repl)
    (setq nodejs-repl-prompt prompt)
    (global-set-key (kbd "C-c C-r js") 'nodejs-repl)    
    (add-hook 'js-mode-hook 'binding-nodejs-keymaps)))
;; TODO crash when type "TAB"

(defun binding-nodejs-keymaps ()
  "Nodejs-repl keybindings."
  (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
  (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
  (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
  (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))

(defun configure-json-mode ()
  "Configure json mode."
  (require-or-install 'json-mode)
  (add-to-list 'auto-mode-alist '("\\.babelrc" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.eslintrc" . json-mode)))

(defun lang-javascript ()
  "Configure javascript mode."
  ;; Nodejs-repl
  (configure-nodejs-repl)
  ;; Json
  (configure-json-mode)
  ;; Templates
  ;;()
  )

(deftask javascript
  "Apply javascript-IDE configs."
  (run-with-timer (ms 100) nil 'lang-javascript))



;;; Http

(defun lang-http ()
  ""
  (require-or-install 'httprepl)
  )

(deftask javascript
  "Apply HTTP utils configs."
  (run-with-timer (ms 100) nil 'lang-http))


