;;; hairy-dashboard.el --- Hairy Dashboard  -*- lexical-binding: t -*-

(require 's)

(defvar hairy-dashboard-buffer-name "*Hairy*"
  "Hairy dashboard buffer name.")

(defvar hairy-dashboard-hr-length 10
  "Hairy dashboard hr length.")

(defvar hairy-dashboard-small-text "echo"
  "Hairy dashboard small text.")

(defface hairy-dashboard-symbol-face
  '((t (:foreground "LightGray")))
  "Hairy logo text '[' and ']' color.")

(defface hairy-dashboard-text-H-face
  '((t (:foreground "SlateBlue")))
  "Hairy logo text 'H' color.")

(defface hairy-dashboard-text-A-face
  '((t (:foreground "DeepSkyBlue")))
  "Hairy logo text 'A' color.")

(defface hairy-dashboard-text-I-face
  '((t (:foreground "DodgerBlue")))
  "Hairy logo text 'I' color.")

(defface hairy-dashboard-text-Y-face
  '((t (:foreground "plum")))
  "Hairy logo text 'Y' color.")

(defface hairy-dashboard-text-R-face
  '((t (:foreground "purple")))
  "Hairy logo text 'R' color.")

(defface hairy-dashboard-text-B-face
  '((t (:foreground "salmon")))
  "Hairy logo text 'B' color.")

(defface hairy-dashboard-small-text-face
  '((t (:foreground "DodgerBlue")))
  "Hairy small text color.")

(defface hairy-dashboard-action-face-1
  '((t (:foreground "purple")))
  "Hairy small action text color 1.")

(defface hairy-dashboard-action-face-2
  '((t (:foreground "plum")))
  "Hairy small action text color 2.")

(defface hairy-dashboard-action-face-3
  '((t (:foreground "SlateBlue")))
  "Hairy small action text color 3.")

(defface hairy-dashboard-action-face-4
  '((t (:foreground "DodgerBlue")))
  "Hairy small action text color 4.")

(defface hairy-dashboard-action-face-5
  '((t (:foreground "DodgerBlue")))
  "Hairy small action text color 5.")

(defface hairy-dashboard-action-face-6
  '((t (:foreground "DodgerBlue")))
  "Hairy small action text color 6.")


;;; Todos

(defvar hairy-dashboard-action-todos
  (list :text "todos"
        :face 'hairy-dashboard-action-face-1
        :handle 'hairy/start-todos)
  "Open todos layout.")

(defun hairy/start-todos ()
  "Open todos layout."
  42)

;;; Projects

(defvar hairy-dashboard-action-projects
  (list :text "projects"
        :face 'hairy-dashboard-action-face-2
        :handle 'hairy/start-projects)
  "Open projects layout.")

(defun hairy/start-projects ()
  "Open projects layout."
  42)

(defvar hairy-dashboard-action-blog
  (list :text "blog"
        :face 'hairy-dashboard-action-face-3
        :handle 'hairy/start-blog)
  "Open blog layout.")

(defun hairy/start-blog ()
  "Open blog layout."
  42)

(defun set-font-color (str color)
  "Set font color."
  (propertize str 'face `((:foreground ,color))))

;;; Render header.
(defun hairy-dashboard/make-banner ()
  "Make hairy dashboard banner text."
  (let ((char-\[ (propertize "[" 'face 'hairy-dashboard-symbol-face))
        (char-\] (propertize "]" 'face 'hairy-dashboard-symbol-face))
        (char-\H (propertize "H" 'face 'hairy-dashboard-text-H-face))
        (char-\A (propertize "A" 'face 'hairy-dashboard-text-A-face))
        (char-\I (propertize "I" 'face 'hairy-dashboard-text-I-face))
        (char-\Y (propertize "Y" 'face 'hairy-dashboard-text-Y-face))
        (char-\R (propertize "R" 'face 'hairy-dashboard-text-R-face))
        (char-\B (propertize "B" 'face 'hairy-dashboard-text-B-face)))
    (s-join "  " (list char-\[ char-\H char-\A "I" "R" char-\Y char-\]
                       " "
                       char-\R "A" "B" char-\B char-\I "T" ))))

(defun hairy-dashboard/make-hr ()
  "Make hairy split-line text below the banner."
  (s-pad-left 39 " "
              (propertize (s-repeat hairy-dashboard-hr-length "_")
                          'face 'hairy-dashboard-symbol-face)))

(defun hairy-dashboard/make-small-text ()
  "Make hairy small text."
  (s-pad-left 40 " "
              (propertize hairy-dashboard-small-text
                          'face 'hairy-dashboard-small-text-face)))

(defun hairy-dashboard/render-header (max-width max-height)
  "Render hairy header."
  (let ((banner (hairy-dashboard/make-banner))
        (hr     (hairy-dashboard/make-hr))
        (small  (hairy-dashboard/make-small-text)))
    (newline (- (/ max-height 2) 1))
    (insert (s-center max-width banner))
    (newline)
    (insert (s-center max-width hr))
    (newline)
    (insert (s-center max-width small))))

(defun hairy-dashboard/render-nav-item (action)
  "Render Navigator item."
  (let* ((str    (plist-get action :text))
         (face   (plist-get action :face))
         (handle (plist-get action :handle))
         (arr    (s-split "" str t))
         (fst    (s-wrap (car arr) "[" "]"))
         (tails  (s-join "" (cdr arr))))
    (insert-text-button (propertize str 'face face)
                        'action handle)
    (insert tails)))

(defun hairy-dashboard/render-nav ()
  "Render navigator."
  (render-nav-item hairy-dashboard-action-todos)
  (insert (s-repeat 6 " "))
  (render-nav-item hairy-dashboard-action-projects)
  (insert (s-repeat 6 " "))
  (render-nav-item hairy-dashboard-action-blog))

(defun hairy-dashboard/create-buffer (window)
  "Create *Hairy* buffer."
  (let ((w (window-body-width))
        (h (window-body-height)))
    (with-current-buffer (get-buffer-create hairy-dashboard-buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      (hairy/render-header w h)
      (newline 4)
      (insert (s-repeat (/ (- w 36) 2) " "))
      (render-nav)
      (newline)
      (hairy-mode))))

(defun switch-to-hairy ()
  "Switch to Hairy."
  (interactive)
  (maximize-restore-emacs)
  (neotree-hide)
  (if (window-live-p layout-emacs-window)
      (delete-other-windows layout-emacs-window)
    (progn
      (delete-other-windows)
      (setq layout-emacs-window (selected-window))))
  (when (not (get-buffer hairy-dashboard-buffer-name))
    (hairy-dashboard/create-buffer layout-emacs-window))
  (set-window-vscroll layout-emacs-window 0)
  (set-window-hscroll layout-emacs-window 0)
  (set-window-buffer layout-emacs-window hairy-dashboard-buffer-name))

(defvar hairy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'layout-project)
    (global-set-key (kbd "C-c C-q") 'switch-to-hairy)
    map)
  "hairy-mode keymaps.")

(define-derived-mode hairy-mode special-mode "Hairy"
  "Hairy Rabbit emacs greeting."
  (setq mode-line-format nil
        buffer-read-only t
        indent-tabs-mode nil
        font-lock-mode nil))

(provide 'hairy-dashboard)
