;;; hairy-package.el --- manange packages  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 by Rabbit

;; Author: HairyRabbit
;; URL: https://github.com/HairyRabbit/hairy
;; Version: 0.0.1

;;; Code:

(require 'package)

;;;###autoload
(defun require-or-install (feature &optional filename)
  "Install package first when require failed."
  (unless (funcall 'require feature filename t)
    (progn
      (package-install feature nil)
      (funcall 'require feature filename))))

(defvar hairy-package-source-melpa-usestable nil
  "Use MELPA stable library.")

(defvar hairy-package-source-use-image t
  "Use package source images.")

(defvar hairy-package-source-protocol "http://"
  "Use https:// or http://")

(defun hairy-package-configure-sources ()
  "Configure package sources.

* [Elpa](https://elpa.gnu.org/) - default
* [Melpa](https://melpa.org/)   - popular
"
  (let* ((elpa               `("gnu" .
                               ,(concat hairy-package-source-protocol
                                        "elpa.gnu.org/packages/")))
	 (elpa-image         `("gun" .
                               ,(concat hairy-package-source-protocol
                                        "elpa.emacs-china.org/gnu/")))
	 (melpa-stable       `("melpa-stable" .
                               ,(concat hairy-package-source-protocol
                                        "stable.melpa.org/packages/")))
	 (melpa-stable-image `("melpa-stable" .
                               ,(concat hairy-package-source-protocol
                                        "elpa.emacs-china.org/melpa-stable/")))
	 (melpa              `("melpa" .
                               ,(concat hairy-package-source-protocol
                                        "melpa.org/packages/")))
	 (melpa-image        `("melpa" .
                               ,(concat hairy-package-source-protocol
                                        "elpa.emacs-china.org/melpa/"))))
    (if (not hairy-package-source-use-image)
	(list elpa
	      (if hairy-package-source-melpa-usestable
                  melpa-stable
                melpa))
      (list elpa-image
	    (if hairy-package-source-melpa-usestable
                melpa-stable-image
              melpa-image)))))

(defun hairy-package-reset ()
  "Reset package source."
  (interactive)
  (setq package-archives (hairy-package-configure-sources))
  (package-initialize)
  (unless (or (package-installed-p 'dash)
              (package-installed-p 'projectile))
    (package-refresh-contents)))

(defun hairy-package-bind-keymaps ()
  "Bind package keymaps"
  (global-set-key (kbd "C-c C-p l") 'package-list-packages-no-fetch)
  (global-set-key (kbd "C-c C-p r") 'hairy-package-reset)
  (global-set-key (kbd "C-c C-p i") 'package-install))

(defun hairy-package-preload ()
  "Preload useful librarys

  - dash
  - s
  - f

And add font lock to dash."
  (require-or-install 'dash)
  (require-or-install 's)
  (require-or-install 'f)
  (eval-after-load 'dash (dash-enable-font-lock)))

(defun hairy-package-upgrade ()
  "@TODO Upgrade librarys."
  (interactive)
  (package-refresh-contents t)
  (with-current-buffer (get-buffer-create "*HairyPackage*")
    (package-menu-mode)
    (package-menu-refresh)
    (package-menu--generate nil t)
    (package-menu-mark-upgrades)))

(eval-after-load "hairy-package"
  (progn
    (hairy-package-reset)
    (hairy-package-bind-keymaps)
    (hairy-package-preload)))

(provide 'hairy-package)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hairy-package.el ends here
