;;; hairy-company.el --- configure company  -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 by Rabbit

;; Author: HairyRabbit
;; URL: https://github.com/HairyRabbit/hairy
;; Version: 0.0.1

;;; Code:

(defun hairy-company-bind-keymaps ()
  "Binding company mode keymaps."
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous))

(defun hairy-company-bind-hippie-expand-keymaps ()
  "Binding hippie expand to M-/"
  (global-set-key (kbd "M-/") 'hippie-expand))

(eval-after-load "hairy-company"
  (run-with-timer
   "100millisec" nil
   (lambda ()
     (let ()
       (hairy-company-bind-hippie-expand-keymaps)
       (require-or-install 'company)
       (setq company-require-match nil)
       (setq company-auto-complete t)
       (hairy-company-bind-keymaps)
       (global-company-mode)))))

(provide 'hairy-company)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hairy-company.el ends here
