;;; hairy.el --- Hairy Rabbit  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 by Rabbit

;; Author: Rabbit
;; URL: https://github.com/HairyRabbit/hairy
;; Version: 0.0.1
;; Keywords: Hairy
;; Package-Requires: ((emacs "25.3"))

;;; Code:

;;;###autoload
(defun hairy-ui (args)
  "Configure default emacs ui."
  ;; title bar
  (setq frame-title-format "emacs@%b")
  ;; mouse
  (mouse-avoidance-mode 'animate)
  )
