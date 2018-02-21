;;; hairy-yasnippet.el --- configure yasnippet  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 by Rabbit

;; Author: HairyRabbit
;; URL: https://github.com/HairyRabbit/hairy
;; Version: 0.0.1
;; Keywords: Hairy
;; Package-Requires: ((emacs "25.3"))

;;; Code:

(require 's)

(defun hairy-yasnippet-get-env ()
  "Get env variable 'YASNIPPET_PATHS', return nil if not found anything."
  (getenv "YASNIPPET_PATHS"))

(defun hairy-yasnippet-set-dirs (paths)
  "Set path as `yas-snippet-dirs' values."
  (setq yas-snippet-dirs paths))

(defun hairy-yasnippet-split-path (paths)
  "Split paths to list"
  (s-split ";" paths))

(defun hairy-yasnippet-sync ()
  "@TODO Sync from github"
  (interactive)

  )

(eval-after-load "hairy-yasnippet"
  (run-with-timer
   "100millisec" nil
   (lambda ()
     (let ((paths (hairy-yasnippet-get-env)))
       (require-or-install 'yasnippet)
       (setq yas-new-snippet-default nil
             yas-triggers-in-field t)
       (when paths
         (hairy-yasnippet-set-dirs (hairy-yasnippet-split-path paths)))
       (yas-global-mode 1)))))

(provide 'hairy-yasnippet)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hairy-yasnippet.el ends here
