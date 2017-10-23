;;; hairy.el --- Hairy Rabbit  -*- lexical-binding: t -*-

;; Copyright (C) 2017 by Rabbit

;; Author: Rabbit
;; URL: https://github.com/yuffiy/hairy
;; Version: 0.0.1
;; Keywords: Hairy
;; Package-Requires: ((emacs "25.3"))

;;; Code:

;;;; Application
(defconst hairy/app-name "Hairy"
  "App name.")

(defconst hairy/app-version "0.0.0"
  "App version.")

(defconst hairy/app-init-time (current-time)
  "App start time.")

;;;; Tasks
(defvar hairy/task-list '()
  "Task list.")

(defun hairy/during (start-time)
  "Compute during between start-time and current-time."
  (float-time (time-subtract (current-time) start-time)))

(defmacro deftask (name &optional docs &rest body)
  "Run tasks."
  (let ((start-time (current-time))
        (task-name (symbol-name name)))
    `(progn ,@body
            (let ((during (hairy/during ',start-time)))
              (add-to-list 'hairy/task-list
                           '(:name ,task-name :cost during))
              (message "Task %s const %.3fs" ,task-name during)))))

(defmacro deftask-delay (name &optional docs &rest body)
  "Run tasks in timer, delay 100ms."
  (let ((start-time (current-time))
        (task-name (symbol-name name)))
    `(run-with-timer
      "100millisec" nil
      (lambda ()
        (progn ,@body
               (let ((during (hairy/during ',start-time)))
                 (add-to-list 'hairy/task-list
                              '(:name ,task-name :cost during))
                 (message "Task %s const %.3fs" ,task-name during)))))))

(defun ms (num)
  "Make millisec number"
  (timer-duration (concat (number-to-string num) "millisec")))

;;;; Packages
(defvar package-source-melpa-usestable nil
  "Use MELPA stable library.")

(defvar package-source-use-image t
  "Use package source images.")

(defvar package-source-protocol "http://"
  "Use https:// or http://")

(defun configure-package-sources ()
  "Configure package sources.

* [Elpa](https://elpa.gnu.org/) - Default
* [Melpa](https://melpa.org/)   - Popular
"
  (let* ((elpa               `("gnu" .
                               ,(concat package-source-protocol
                                        "elpa.gnu.org/packages/")))
	 (elpa-image         `("gun" .
                               ,(concat package-source-protocol
                                        "elpa.emacs-china.org/gnu/")))
	 (melpa-stable       `("melpa-stable" .
                               ,(concat package-source-protocol
                                        "stable.melpa.org/packages/")))
	 (melpa-stable-image `("melpa-stable" .
                               ,(concat package-source-protocol
                                        "elpa.emacs-china.org/melpa-stable/")))
	 (melpa              `("melpa" .
                               ,(concat package-source-protocol
                                        "melpa.org/packages/")))
	 (melpa-image        `("melpa" .
                               ,(concat package-source-protocol
                                        "elpa.emacs-china.org/melpa/"))))
    (if (not package-source-use-image)
	(list elpa
	      (if package-source-melpa-usestable melpa-stable melpa))
      (list elpa-image
	    (if package-source-melpa-usestable melpa-stable-image melpa-image)))))

(defun reset-package-source ()
  "Reset package source."
  (interactive)
  (setq package-archives (configure-package-sources))
  (package-initialize))

(defun require-or-install (feature &optional filename)
  "Install package when require failed."
  (unless (funcall 'require feature filename t)
    (progn
      (package-install feature nil)
      (funcall 'require feature filename))))

;;;; Commmands
(defvar emacs-maximize-p nil
  "Maximized emacs.")

(defun maximize-or-restore-emacs ()
  "Maximize emacs."
  (interactive)
  (if (not emacs-maximize-p)
      (maximize-emacs)
    (maximize-restore-emacs)))

(defun maximize-emacs ()
  "Maximize emacs."
  (interactive)
  (w32-send-sys-command 61488)
  (setq emacs-maximize-p t))

(defun maximize-restore-emacs ()
  "Maximize emacs."
  (interactive)
  (w32-send-sys-command 61728)
  (setq emacs-maximize-p nil))

(defun set-font-color (str color)
  "Set font color."
  (propertize str 'face `((:foreground ,color))))


;;;; Package manager
;; TODO check package upgrade
;; TODO fetch timeout
(deftask hairy/configure-package
  "Apply package configs."
  (require 'package)
  (reset-package-source)
  (setq package-check-signature nil)
  (unless (or (package-installed-p 'dash)
              (package-installed-p 'projectile))
    (package-refresh-contents))
  (global-set-key (kbd "C-c C-p l") 'package-list-packages-no-fetch)
  (global-set-key (kbd "C-c C-p r") 'reset-package-source)
  (global-set-key (kbd "C-c C-p i") 'package-install))

;; (deftask-delay hairy/upgrade-library
;;   "Upgrade library."
;;   (package-refresh-contents t)
;;   (with-current-buffer (get-buffer-create "*Packages*")
;;     (package-menu-mode)
;;     (package-menu-refresh)
;;     (package-menu--generate nil t)
;;     (package-menu-mark-upgrades)))

;;;; Preload librarys.
(deftask preload-library
  "Preload utils library"
  (require-or-install 'dash)
  (require-or-install 's)
  (require-or-install 'f)
  (eval-after-load 'dash (dash-enable-font-lock)))

;;;; Default base
(defun hairy/text-scaled-size (str face)
  "Compute scaled size in buffer base on default font size."
  (let ((face-size (float (face-attribute face :height)))
        (default-size (float (face-attribute 'default :height))))
    (ceiling (* (string-width str)
                (/ face-size default-size)))))

(defun hairy/insert-center (width str lines &optional face)
  "Insert text to buffer and center the text."
  (if (not face) (insert (s-center width str))
    (let* ((str-width (hairy/text-scaled-size str face))
           (len (string-width str))
           (compute-width (- (1- width)
                             (- str-width len))))
      (insert (s-center compute-width str))))
  (when lines (newline lines)))

;;;; Hairy workspace
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
  (interactive)
  "Open todos layout."
  42)

;;; Projects
(defvar hairy-dashboard-action-projects
  (list :text "projects"
        :face 'hairy-dashboard-action-face-2
        :handle 'hairy/start-repo)
  "Open projects layout.")

(defvar hairy-dashboard-action-blog
  (list :text "blog"
        :face 'hairy-dashboard-action-face-3
        :handle 'hairy/start-blog)
  "Open blog layout.")

(defun hairy/start-blog ()
  (interactive)
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
  (s-pad-left
   39 " "
   (propertize (s-repeat hairy-dashboard-hr-length "_")
               'face 'hairy-dashboard-symbol-face)))

(defun hairy-dashboard/make-small-text ()
  "Make hairy small text."
  (s-pad-left
   40 " "
   (propertize hairy-dashboard-small-text
               'face 'hairy-dashboard-small-text-face)))

(defun hairy-dashboard/render-header (max-width max-height)
  "Render hairy header."
  (let ((banner (hairy-dashboard/make-banner))
        (hr     (hairy-dashboard/make-hr))
        (small  (hairy-dashboard/make-small-text)))
    (newline (- (/ max-height 2) 2))
    (insert (s-center max-width banner))
    (newline)
    (insert (s-center max-width hr))
    (newline)
    (insert (s-center max-width small))))

(defun hairy-dashboard/render-action (action)
  "Render Navigator item."
  (let* ((str    (plist-get action :text))
         (face   (plist-get action :face))
         (handle (plist-get action :handle))
         (arr    (s-split "" str t))
         (fst    (s-wrap (car arr) "[" "]"))
         (tails  (s-join "" (cdr arr))))
    (insert-text-button
     (propertize fst 'face face)
     'action (lambda (btn) (handle)))
    (insert tails)))

(defun hairy-dashboard/render-nav ()
  "Render navigator."
  (hairy-dashboard/render-action hairy-dashboard-action-todos)
  (insert (s-repeat 6 " "))
  (hairy-dashboard/render-action hairy-dashboard-action-projects)
  (insert (s-repeat 6 " "))
  (hairy-dashboard/render-action hairy-dashboard-action-blog))

(defun hairy-dashboard/create-buffer (window)
  "Create *Hairy* buffer."
  (let ((w (window-body-width))
        (h (window-body-height)))
    (with-current-buffer (get-buffer-create hairy-dashboard-buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      (hairy-dashboard/render-header w h)
      (newline 4)
      (insert (s-repeat (/ (- w 36) 2) " "))
      (hairy-dashboard/render-nav)
      (newline)
      (hairy-mode))))

(defun hairy-dashboard/switch-to-buffer ()
  "Switch to Hairy."
  (interactive)
  (maximize-restore-emacs)
  (neotree-hide)
  (if (window-live-p layout-emacs-window)
      (delete-other-windows-internal layout-emacs-window)
    (progn
      (delete-other-windows-internal)
      (setq layout-emacs-window (selected-window))))
  (when (not (get-buffer hairy-dashboard-buffer-name))
    (hairy-dashboard/create-buffer layout-emacs-window))
  (set-window-vscroll layout-emacs-window 0)
  (set-window-hscroll layout-emacs-window 0)
  (set-window-buffer layout-emacs-window hairy-dashboard-buffer-name))

(defvar hairy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'hairy/start-repo)
    (global-set-key (kbd "C-c C-q") 'hairy-dashboard/switch-to-buffer)
    map)
  "hairy-mode keymaps.")

(define-derived-mode hairy-mode special-mode "Hairy"
  "Hairy Rabbit emacs greeting."
  (setq mode-line-format nil
        buffer-read-only t
        indent-tabs-mode nil
        font-lock-mode nil))

(defun hairy-dashboard/get-buffer ()
  "Get dashboard buffer."
  (get-buffer hairy-dashboard-buffer-name))

(deftask hairy/greeting
  "Reset startup screen."
  (hairy-dashboard/create-buffer (selected-window))
  ;; (add-hook 'window-size-change-functions 'hairy-dashboard/create-buffer)
  (setq inhibit-startup-screen t
        initial-buffer-choice 'hairy-dashboard/get-buffer))

;;;; Repository layout
(defvar hairy-repo/sidebar-buffer-name "*Repositories*"
  "Repository layout sidebar buffer name")

(defvar hairy-repo/main-buffer-name "*Repository Info*"
  "Repository layout info buffer name")

(defface hairy-repo-header-face
  '((t (:foreground "LightGray" :height 200)))
  "Repository layout header face.")

(defun hairy-repo/get-repo-type (project-root)
  "Get projectile type of repo."
  (gethash project-root projectile-project-type-cache))

(defun hairy-repo/get-repos ()
  "Get repository info from projectile."
  (require-or-install 'projectile)
  (let* ((projects (projectile-load-known-projects)))
    (-map (lambda (project-root)
            (list :name (f-filename project-root)
                  :path (f-dirname project-root)
                  :root project-root
                  :vc (projectile-project-vcs project-root)
                  :type (hairy-repo/get-repo-type project-root)))
          projects)))

(defun hairy-repo/render-repo-info (buf repo)
  "Render repo details to main buffer when click sidebar repo."
  (let* ((name (plist-get repo :name))
         (vc   (plist-get repo :vc))
         (root (plist-get repo :root)))
    (with-current-buffer buf
      (delete-region body-point (buffer-end 1))
      (insert "Name")
      (newline)
      (insert name)
      (insert-button "Open Project")
      )))

(defvar hairy-repo-main-buffer-renderer-p nil
  "Hairy repo main buffer was renderer.")

(defun hairy-repo/render-default-view (buf)
  "Render repo info default view."
  (with-current-buffer buf
    (let* ((win (selected-window))
           (w (window-body-width win))
           (h (window-body-height win))
           (header (propertize "Repositories"
                               'face 'hairy-repo-header-face)))
      (newline (- (/ h 2) 16))
      (hairy/insert-center w header t 'hairy-repo-header-face)
      (hairy/insert-center w "--------------------" 4)
      (setq-local body-point (point))
      (hairy/insert-center w "Move to next project       Ctrl-N   " 2)
      (hairy/insert-center w "Move to prev project       Ctrl-P   " 2)
      (hairy/insert-center w "Open project main file     Ctrl-O   " 2)
      (hairy/insert-center w "Remove project from cache  Ctrl-R   " 2)
      (hairy/insert-center w "Add a project              Drop     " 2))
    (setq hairy-repo-main-buffer-renderer-p t)
    (switch-to-buffer buf)))

;;; Repo layout sidebar window
(defface hairy-repo-sidebar-header-face
  '((t (:foreground "#9F9BB9" :height 180)))
  "Repository layout sidebar header face.")

(defface hairy-repo-sidebar-small-face
  '((t (:foreground "grey55" :height 120)))
  "Repository layout sidebar small face.")

(defvar hairy-repo-sidebar-header-text "PROJECTILE LIST"
  "Repository layout sidebar header text.")

(defvar hairy-repo-sidebar-small-text "workspace"
  "Repository layout sidebar small text.")

(defun hairy-repo/render-sidebar-headers ()
  "Render repo layout sidebar headers."
  (let ((header (propertize hairy-repo-sidebar-header-text
                            'face 'hairy-repo-sidebar-header-face))
        (small (propertize hairy-repo-sidebar-small-text
                           'face 'hairy-repo-sidebar-small-face)))
    (newline 2)
    (insert header)
    (newline)
    (insert small)
    (newline 3)))

(defun hairy-repo/render-sidebar-empty-view ()
  "Render repo layout sidebar empty view."
  (insert "+ Add a new repository"))

(defface hairy-repo-sidebar-item-name-face
  '((t (:foreground "Black" :height 130)))
  "Repository layout sidebar small face.")

(defface hairy-repo-sidebar-item-branch-face
  '((t (:foreground "#9F9BB9" :height 100)))
  "Repository layout sidebar small face.")

(defface hairy-repo-sidebar-item-root-face
  '((t (:foreground "gray45" :height 90)))
  "Repository layout sidebar small face.")

(defun hairy-repo/render-sidebar-list-view (repos main-buf)
  "Render repo layout sidebar list view."
  (require-or-install 'all-the-icons)
  (setq inhibit-compacting-font-caches t)
  (-each repos
    (lambda (repo)
      (let* ((name (plist-get repo :name))
             (root (plist-get repo :root)))
        (insert (all-the-icons-octicon "repo"
                                       :height 1.4
                                       :face '(:foreground "#372B68")))
        (insert " ")
        (insert-text-button
         (propertize (s-upper-camel-case name)
                     'face 'hairy-repo-sidebar-item-name-face)
         'action (lambda (btn)
                   (hairy-repo/render-repo-info main-buf repo)
                   ))
        (insert " ")
        (insert (propertize "(master)"
                            'face 'hairy-repo-sidebar-item-branch-face))
        (newline)
        (insert "   ")
        (insert (propertize root
                            'face 'hairy-repo-sidebar-item-root-face))
        (newline 2)))))

(defface hairy-repo-sidebar-default-face
  '((t (:background "#F5EBDD")))
  "Repository layout sidebar default buffer face.")

(defvar hairy-repo-sidebar-buffer-renderer-p nil
  "Hairy repo sidebar was buffer renderer.")

(defun hairy-repo/render-sidebar (buf main-buf)
  "Render repo layout sidebar."
  (with-current-buffer buf
    (let ((repos (hairy-repo/get-repos)))
      (setq buffer-face-mode-face 'hairy-repo-sidebar-default-face)
      (buffer-face-mode)
      (hairy-repo/render-sidebar-headers)
      (setq-local body-point (point))
      (if (not repos)
          (hairy-repo/render-sidebar-empty-view)
        (hairy-repo/render-sidebar-list-view repos main-buf))
      (setq hairy-repo-sidebar-buffer-renderer-p t))))

(defun hairy/start-repo ()
  "Open projects layout."
  (interactive)
  (maximize-emacs)
  ;; Need delay a few seconds to fetch window size.
  (run-with-timer
   0.1 nil
   (lambda ()
     (let* ((side-buf (get-buffer-create hairy-repo/sidebar-buffer-name))
            (main-buf (get-buffer-create hairy-repo/main-buffer-name)))
       ;; Make sidebar buffer
       (unless hairy-repo-sidebar-buffer-renderer-p
         (hairy-repo/render-sidebar side-buf main-buf))
       (setq-local hairy-repo/sidebar-window
                   (display-buffer-in-side-window side-buf `((side . left))))
       (set-window-margins hairy-repo/sidebar-window 2 0)
       (set-window-fringes hairy-repo/sidebar-window 0 0)
       ;; Set window width
       (enlarge-window-horizontally
        (- (window-width hairy-repo/sidebar-window) 25))
       (with-current-buffer side-buf
         (setq window-size-fixed 'width))
       ;; Make main buffer
       (unless hairy-repo-main-buffer-renderer-p
         (hairy-repo/render-default-view main-buf))
       (display-buffer main-buf)))))

;;;; Projectile
(defmacro defproject (name &optional doc &rest args)
  ""
  )
(defun hairy-projectile/create-project (root template)
  ;; (f-write)
  )

;;;; Projectile react
(defconst hairy-projectile/template-.gitignore "\
# Logs
logs
*.log
npm-debug.log*

# Runtime data
pids
*.pid
*.seed

# Directory for instrumented libs generated by jscoverage/JSCover
lib-cov

# Coverage directory used by tools like istanbul
coverage

# nyc test coverage
.nyc_output

# Grunt intermediate storage (http://gruntjs.com/creating-plugins#storing-task-files)
.grunt

# node-waf configuration
.lock-wscript

# Compiled binary addons (http://nodejs.org/api/addons.html)
build/Release

# Dependency directories
node_modules
jspm_packages

# Optional npm cache directory
.npm

# Optional REPL history
.node_repl_history

# Flowtype
flow-bin
flow-typed/npm

# Webpack Config
webpack.config.js
.cache-loader

# Emacs tmp file
\#*\#

# Project
tmp
dist
log
"
  "Git ignore file '.gitignore' template.")

(defconst hairy-projectile/template-.editorconfig "\
root = true

[*]
end_of_line = lf
insert_final_newline = true
charset = utf-8
indent_style = space
indent_size = 2

[{*.json,.babelrc,.eslintrc,.stylelintrc,travis.yml}]
indent_size = 4

[{.babelrc, .eslintrc, .stylelintrc}]
emacs_mode = json
"
  "Editorconfig file.")

(defconst hairy-projectile/template-.eslintrc "\
{
    \"env\": {
        \"browser\": true,
        \"es6\": true,
        \"node\": true,
        \"worker\": true,
        \"jest\": true
    },
    \"extends\": [
        \"eslint:recommended\",
        \"plugin:flowtype/recommended\",
        \"plugin:react/recommended\"
    ],
    \"parserOptions\": {
        \"ecmaVersion\": 8,
        \"ecmaFeatures\": {
            \"sourceType\": \"module\",
            \"impliedStrict\": true,
            \"experimentalObjectRestSpread\": true,
            \"jsx\": true
        },
        \"sourceType\": \"module\"
    },
    \"parser\": \"babel-eslint\",
    \"plugins\": [
        \"react\",
        \"flowtype\"
    ],
    \"rules\": {
        \"indent\": \"off\",
        \"linebreak-style\": [
            \"error\",
            \"unix\"
        ],
        \"quotes\": [
            \"error\",
            \"single\",
            {
                \"allowTemplateLiterals\": true
            }
        ],
        \"semi\": [
            \"error\",
            \"never\"
        ],
        \"no-console\": [
            \"off\"
        ],
        \"no-unused-vars\": [
            \"warn\"
        ]
    }
}
"
  "React Eslint config file.")

(defconst hairy-projectile/template-.stylelintrc "\
{
    \"rules\": {

    }
}
"
  "React Stylelint config file.")

(defconst hairy-projectile/template-.babelrc "\
{
    \"presets\": [\"react\", [\"env\", {
        \"target\": {
            \"browsers\": [\"last 1 Chrome versions\"]
        },
        \"modules\": false,
        \"loose\": true
    }]],
    \"plugins\": [
        [\"transform-object-rest-spread\", { \"useBuiltIns\": true } ],
        \"syntax-dynamic-import\",
        \"transform-class-properties\",
        \"lodash\"
    ],
    \"env\": {
        \"test\": {
            \"plugins\": [\"transform-es2015-modules-commonjs\"]
        }
    }
}
"
  "React babel config file.")

(defconst hairy-projectile/template-jest.config.js "\
module.exports = {
    \"verbose\": true,
    \"moduleNameMapper\": {
        \"\\.css$\": \"identity-obj-proxy\",
        \"\\.(webp|jpg|png|svg)$\": \"<rootDir>/scripts/fileTransformer.js\"
    }
}
"
  "React Jest config file.")

(defconst hairy-projectile/template-postcss.config.js "\
module.exports = (ctx) => {
  return {
    plugins: [
      require('postcss-cssnext')(),
      require('stylelint')(),
      require('postcss-short')(),
      require('postcss-easings')(),
      require('postcss-strip-inline-comments')()
    ],
    syntax: require('postcss-scss')
  }
}
"
  "React Postcss config file.")

(defconst hairy-projectile/template-webpack.config.js "\
const path = require('path')
const webpack = require('webpack')
const pkg = require('./package.json')
const HTMLWebpackPlugin = require('html-webpack-plugin')
const HTMLWebpackTemplate = require('html-webpack-template')

module.exports = function (env) {
  const src = path.resolve('src')
  const tmp = path.resolve('tmp')
  const images = path.resolve('public/images')
  const fonts = path.resolve('public/fonts')

  const entry = {
    app: path.resolve(src, 'boot.js')
  }

  const output = {
    path: tmp,
    publicPath: '/',
    filename: '[name].js'
  }

  const module = {
    rules: [
      // JavaScripts
      { test: /\.js$/, enforce: 'pre', use: 'eslint-loader' },
      { test: /\.js$/, use: 'babel-loader' },

      // Styles
      { test: /\.css$/, use: [
	      'style-loader?sourceMap',
	      'css-loader?sourceMap&module&modules&importLoaders=1&camelCase',
	      'postcss-loader?sourceMap'
      ]},

      // Images
      { test: /\.(webp|png|jpg|svg|eot|ttf|woff|woff2)$/, use: 'url-loader' }
    ]
  }

  const resolve = {
    alias: {}
  }

  const plugins = [
    new HTMLWebpackPlugin({
      title: 'webpack',
      inject: true,
      template: HTMLWebpackTemplate
    })
  ]

  const devtool = 'source-map'

  const devServer = {
    publicPath: '/',
    contentBase: tmp,
    hot: true,
    host: '0.0.0.0',
    port: '8080'
  }

  const cache = true

  return {
    entry,
    output,
    module,
    resolve,
    plugins,
    devtool,
    devServer,
    cache
  }
}
"
  "React Webpack config file.")

(defconst hairy-projectile/template-package.json "\
{
    \"name\": \"eth-web\",
    \"version\": \"0.0.1\",
    \"main\": \"index.js\",
    \"license\": \"Unlicense\",
    \"repository\": \"http://yuffiy@git.meiyouka.com/Ethereum/eth-www.git\",
    \"author\": [
        \"YF.W <631190613@qq.com>\"
    ],
    \"scripts\": {
        \"start\": \"cross-env NODE_ENV=development node scripts/start.js\"
    }
}
"
  "React package.json.")

(defproject react
  "The react project."
  :files '((config . '(".gitignore"
                       ".editorconfig"
                       ".eslintrc"
                       ".stylelintrc"
                       ".babelrc"
                       ".flowconfig"
                       "package.json"
                       "jest.config.js"
                       "postcss.config.js"
                       "webpack.config.js"))
           (directory . '("src/"
                          "src/components/"
                          "src/helpers/"
                          "src/view/"
                          "src/core/"
                          "tmp/"
                          "dist/"
                          "scripts/"
                          "public/"
                          "public/images/"
                          "public/fonts/"
                          "config/"
                          "log/"))
           (file . '("src/index.js"
                     "src/boot.js"
                     "src/core/index.js"
                     "src/core/example/index.js"
                     "src/core/example/types/index.js"
                     "src/core/example/types/example.js"
                     "src/core/example/states/index.js"
                     "src/core/example/states/example.js"
                     "src/view/index.jsx"
                     "src/view/example/index.jsx"
                     "src/view/example/style.css"))
           )
  :start "yarn start"
  :test "yarn test"
  :build "yarn build"
  :deploy "yarn deploy"
  )

;;;; Editor
(defun configure-editorconfig ()
  "Configure editorconfig"
  (require-or-install 'editorconfig)
  (require-or-install 'editorconfig-custom-majormode)
  (add-hook 'editorconfig-custom-hook 'editorconfig-custom-majormode)
  (editorconfig-mode 1))

(defun configure-auto-file ()
  "Auto file save, backup, read"
  (global-auto-revert-mode 1)
  (setq make-backup-files nil)
  (setq save-interprogram-paste-before-kill t)
  (setq load-prefer-newer t))

(defun configure-buffer ()
  "Configure Buffer and MiniBuffer"
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
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun configure-conding-system ()
  "Use utf-8 coding system."
  ;; (setq utf-translate-cjk-mode nil)
  ;; (prefer-coding-system 'utf-8)
  ;; (set-language-environment "UTF-8")
  ;; (set-default-coding-systems 'utf-8)
  ;; (set-buffer-file-coding-system 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (set-selection-coding-system 'utf-8)
  ;; (setq locale-coding-system 'utf-8)
  ;; (setq coding-system-for-read 'utf-8)
  ;; (setq coding-system-for-write 'utf-8)
  ;; (setq-default buffer-file-coding-system 'utf-8-unix)
  ;; (setq-default file-name-coding-system 'utf-8-unix)
  ;; (require-or-install 'cnfonts)
  ;; (cnfonts-enable)
  (require-or-install 'unicode-fonts)
  (unicode-fonts-setup)
  (setq inhibit-compacting-font-caches t)
  (set-fontset-font "fontset-default" nil
                    (font-spec :size 20 :name "Symbola"))
  )

(defun configure-frame-default ()
  "Configure default ui and frame size."
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (set-frame-width (selected-frame) 90)
  (set-frame-height (selected-frame) 33))

(defun configure-ui ()
  "Configure default UI."
  (setq frame-title-format "emacs@%b")
  (mouse-avoidance-mode 'animate)
  (setq column-number-mode t)
  (set-face-attribute 'default nil
                      :font "Consolas 12"
                      :foreground "#372620"
                      :background "FloralWhite")
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-foreground 'vertical-border "#F7F2E9")
  (require-or-install 'fill-column-indicator)
  (fci-mode 1)
  (setq fill-column 80)
  (setq-default fci-rule-column 80)
  (setq-default indent-tabs-mode nil))

;; (defun neotree-projectile ()
;;   "Open neotree with projectile as root and open node for current file.
;; If projectile unavailable or not in a project, open node at file path.
;; If file path is not available, open $HOME."
;;   (interactive)
;;   (if (neo-global--window-exists-p)
;;       (call-interactively 'neotree-hide)
;;     (let ((file-name (buffer-file-name)))
;;       (if (and (not file-name)
;;                (let ((buffer-name (buffer-name)))
;;                  (cond
;;                   ((equal buffer-name "*cider-repl server*") nil)
;;                   (t t))))
;;           (neotree-dir "~/")
;;         (let ((dir-name (if (and (fboundp 'projectile-project-p)
;;                                  (projectile-project-p))
;;                             (projectile-project-root)
;;                           (file-name-directory file-name))))
;;           (neotree-dir dir-name)
;;           (when file-name
;;             (neo-buffer--select-file-node file-name)))))))


;; (defvar endless/popup-frame-parameters
;;   '((name . "MINIBUFFER")
;;     (minibuffer . only)
;;     (height . 1)
;;     ;; Ajust this one to your preference.
;;     (top . 200))
;;   "Parameters for the minibuffer popup frame.")

;; (defvar endless/minibuffer-frame
;;   (let ((mf (make-frame endless/popup-frame-parameters)))
;;     (iconify-frame mf) mf)
;;   "Frame holding the extra minibuffer.")

;; (defvar endless/minibuffer-window
;;   (car (window-list endless/minibuffer-frame t))
;;   "")

;; (defmacro with-popup-minibuffer (&rest body)
;;   "Execute BODY using a popup minibuffer."
;;   (let ((frame-symbol (make-symbol "selected-frame")))
;;     `(let* ((,frame-symbol (selected-frame)))
;;        (unwind-protect
;;            (progn
;;              (make-frame-visible endless/minibuffer-frame)
;;              (when (fboundp 'point-screen-height)
;;                (set-frame-parameter
;;                 endless/minibuffer-frame
;;                 'top (point-screen-height)))
;;              (select-frame-set-input-focus endless/minibuffer-frame 'norecord)
;;              ,@body)
;;          (select-frame-set-input-focus ,frame-symbol)))))

;; (defun use-popup-minibuffer (function)
;;   "Rebind FUNCTION so that it uses a popup minibuffer."
;;   (interactive)
;;   (let* ((back-symb (intern (format "endless/backup-%s" function)))
;;          (func-symb (intern (format "endless/%s-with-popup-minibuffer"
;;                                     function)))
;;          (defs `(progn
;;                   (defvar ,back-symb (symbol-function ',function))
;;                   (defun ,func-symb (&rest rest)
;;                     ,(format "Call `%s' with a poupup minibuffer." function)
;;                     ,@(list (interactive-form function))
;;                     (with-popup-minibuffer
;;                      (apply ,back-symb rest))))))
;;     (message "%s" defs)
;;     (when (and (boundp back-symb) (eval back-symb))
;;       (error "`%s' already defined! Can't override twice" back-symb))
;;     (eval defs)
;;     (setf (symbol-function function) func-symb)))

;;; Try at own risk.
;; (use-popup-minibuffer 'read-from-minibuffer)
;;; This will revert the effect.
;; (setf (symbol-function #'read-from-minibuffer) endless/backup-read-from-minibuffer)
;; (setq endless/backup-read-from-minibuffer nil)

;;;
;;
;; +----+-------+-------+------+
;; |    |       |       |      |
;; | w1 |       |       |  w6  |
;; |    |  w3   |   w4  |      |
;; |    |       |       +------+
;; +----+       |       |      |
;; |    |       |       |      |
;; | w2 +-------+-------+  w7  |
;; |    |      w5       |      |
;; +----+---------------+------+
;;
;; w1: explorer
;; w2:
;;
;;;
(defvar layout-emacs-window nil)
(defvar layout-project-window-sidebar nil)
(defvar layout-project-window-body nil)
(defvar layout-project-window-project nil)
(defvar layout-project-window-workspace nil)
(defvar layout-project-window-explorer nil)
(defvar layout-project-window-code nil)
(defvar layout-project-window-code1 nil)
(defvar layout-project-window-code2 nil)
(defvar layout-project-window-terminal nil)
(defvar layout-project-window-scm nil)
(defvar layout-project-window-other nil)

(defface project-explorer-default-face
  '((t (:background "#F7F2E9" :height 100)))
  "Project layout explorer default face."
  :group 'layout-project)

;; (require 'json)
(defun workspace-findall ()
  "Find all projects."
  (require-or-install 'projectile)
  (let* ((projects (projectile-load-known-projects)))
    (-map (lambda (project-root)
            (let* ((project-type (gethash project-root
                                          projectile-project-type-cache))
                   (marker-file (plist-get (gethash project-type
                                                    projectile-project-types)
                                           'marker-files))
                   (version (if (and marker-file
                                     (string= (f-ext (car marker-file)) "json"))
                                (let* ((marker-file-path (concat project-root
                                                                 (car marker-file))))
                                  (alist-get 'version
                                             (json-read-file marker-file-path)))
                              nil)))
              `(:name ,(f-filename project-root)
                      :path ,(f-dirname project-root)
                      :vc ,(projectile-project-vcs project-root)
                      :type ,project-type
                      :version ,version
                      )))
          projects)))

(defun workspace-render ()
  "Render projects infomation."
  (let* ((projects (workspace-findall))

         )
    ))

;; (workspace-findall)
(defun neo-display-fn (buffer _alist)
  "Display BUFFER to the left or right of the root window.
The side is decided according to `neo-window-position'.
The root window is the root window of the selected frame.
_ALIST is ignored."
  (let ((window-pos (if (eq neo-window-position 'left) 'left 'right)))
    ;; (display-buffer-in-side-window buffer `((side . ,window-pos)))
    (display-buffer-pop-up-window buffer '())
    ))

(defun configure-neotree ()
  "Configure neotree expolorer."
  (require-or-install 'neotree)
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 35
        neo-show-updir-line nil
        neo-theme 'nerd ; fallback
        ;; neo-theme 'icons
        ;; neo-display-action 'neo-display-fn
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))

  (add-hook 'neo-after-create-hook
            (lambda (_window)
              (set-window-fringes neo-global--window 0 0)
              (setq buffer-face-mode-face 'project-explorer-default-face)
              (buffer-face-mode)
              (set-face-foreground 'vertical-border "#F7F2E9")
              (set-face-background 'fringe "#F7F2E9")
              ))
  (neotree)
  )

(defun layout-project ()
  "Render default project layout, when press 'project' link."
  (interactive)
  (let* ((buf-name "*Empty Code Layout*")
         (window (selected-window))
         (window-width (window-body-width))
         (window-height (window-body-height)))
    (maximize-or-restore-emacs)
    (save-current-buffer
      (generate-new-buffer buf-name)
      (switch-to-buffer (get-buffer-create buf-name))
      (setq test1 (split-window window 30))
      ;; (require 'eshell)
      (require-or-install 'projectile)
      (select-window test1)
      ;; (eshell)
      ;; (projectile-run-eshell)
      (configure-neotree)
      (buffer-face-mode)
      ;; (split-window neo-global--window 20)
      ;; (set-window-dedicated-p neo-global--window nil)
      (print (window-list))
      )
    ))

;; (require 'ansi-color)

;; (defadvice display-message-or-buffer (before ansi-color activate)
;;   "Process ANSI color codes in shell output."
;;   (let ((buf (ad-get-arg 0)))
;;     (and (bufferp buf)
;;          (string= (buffer-name buf) "*Shell Command Output*")
;;          (with-current-buffer buf
;;            (ansi-color-apply-on-region (point-min) (point-max))))))

(defun configure-restart-emacs ()
  "Restart emacs"
  (require-or-install 'restart-emacs)
  (setq restart-emacs-restore-frames t)
  ;; (setq restart-emacs--args "-q --load f:\\hairy\\hairy.el")
  ;; TODO
  )

(deftask editor
  "Reset editor."
  (configure-editorconfig)
  (configure-auto-file)
  (configure-buffer)
  (configure-conding-system)
  (configure-frame-default)
  (configure-ui)
  ;; (hairy/configure-greeting)
  ;; (configure-restart-emacs)
  (setq visible-bell t))

(defun configure-emms ()
  "Play music."
  (require-or-install 'emms)
  (require 'emms-setup)
  (require 'emms-player-simple)
  (require 'emms-player-mplayer)
  (emms-standard)
  (emms-default-players)
  (define-emms-simple-player mplayer '(file url)
    (regexp-opt '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                  ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                  ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
    "mplayer" "-slave" "-quiet" "-really-quiet" "-fullscreen")
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-player-list '(emms-player-mplayer))
  (setq emms-stream-default-action "play")
  (emms-player-for '(*track* (type . file) (name . "d:/MPlayer/test.mp3")))
  )


;;;; Fast coding
(defun configure-undo-redo ()
  "Configure undo and redo."
  (setq kill-ring-max 200))

(defun configure-region ()
  "Fast make region or parens."
  (require-or-install 'expand-region)
  (require-or-install 'multiple-cursors)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-?") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun configure-parens ()
  "Highlight or auto fill parens."
  (require-or-install 'smartparens)
  (show-smartparens-global-mode 1)
  (smartparens-global-mode 1)
  (show-paren-mode 1)
  (setq show-paren-style 'parentheses)
  ;; TODO auto fill parens in block
  )

(defun configure-insert-delete ()
  "Fast insert/delete line word and anything."
  (autoload 'zap-up-to-char "misc" "Kill up" t)
  (global-set-key (kbd "M-z") 'zap-up-to-char))

(defun bind-company-keymaps ()
  "Binding company mode keymaps."
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous))

(defun configure-complition ()
  "Fast complite code.
1. Company
2. Yasnippet
3. Abbr
"
  (global-set-key (kbd "M-/") 'hippie-expand)
  (require-or-install 'yasnippet)
  (yas-global-mode 1)
  (require-or-install 'company)
  (global-company-mode)
  ;;(eval-)
  (eval-after-load 'company
    '(progn
       (bind-company-keymaps)
       (setq company-require-match nil)
       (setq company-auto-complete t))))

(defun configure-jump ()
  "Fast move and jump."
  (require-or-install 'mwim)
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)
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
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
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
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(defun configure-whitespace ()
  "Highlight whitespace."
  (require 'whitespace)
  ;; (global-whitespace-mode 1)
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing lines-tail))
  ;; (setq-default show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(deftask fastcoding
  "Fast coding basic set"
  (configure-undo-redo)
  (configure-whitespace)
  (configure-region)
  (configure-parens)
  (configure-insert-delete))

(deftask fastcoding-delay
  ""
  (run-with-timer (ms 100) nil 'configure-complition)
  (run-with-timer (ms 100) nil 'configure-jump))


;;; Projects
(defun configure-icons ()
  "Configure icon font"
  (require-or-install 'all-the-icons)
  ;;(setq inhibit-compacting-font-caches t)
  )

(defun configure-perspective ()
  "Configure perspective."
  ;;()
  )

(defun create-eshell-alias (&rest args)
  "Define eshell alias."
  (let ((name (car args)))
    (unless (eshell-command-aliased-p name)
      (apply 'eshell/alias args))))

(defun configure-eshell ()
  (require 'eshell)
  (require-or-install 'eshell-fixed-prompt)
  (require-or-install 'eshell-prompt-extras)
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)

  (with-eval-after-load 'em-alias
    ;; commons
    (create-eshell-alias "l" "ls")
    (create-eshell-alias "la" "ls -lAFh")
    (create-eshell-alias "lr" "ls -tRFh")
    (create-eshell-alias "lt" "ls -ltFh")
    (create-eshell-alias "ll" "ls -l")
    (create-eshell-alias "ldot" "ls -ld .*")
    (create-eshell-alias "lart" "ls -1FSsh")
    (create-eshell-alias "lart" "ls -1Fcart")
    (create-eshell-alias "lrt" "ls -1Fcrt")
    ;; git
    (create-eshell-alias "g" "git")
    (create-eshell-alias "ga" "git add")
    (create-eshell-alias "gaa" "git add --all")
    (create-eshell-alias "gau" "git add --update")
    (create-eshell-alias "gb" "git branch")
    (create-eshell-alias "gba" "git branch -a")
    (create-eshell-alias "gbd" "git branch -d")
    (create-eshell-alias "gbda" "git branch --no-color --merged | command grep -vE \"^(\*|\s*(master|develop|dev)\s*$)\" | command xargs -n 1 git branch -d")
    ))

(defun repl-open ()
  "Open REPL buffer"
  (interactive)
  (let ((buf-name "*REPL*"))
    (eshell)
    ))

(defun configure-repl ()
  "Configure REPL
1. eshell/cmd/powershell/bashOnWindows/gitbash/cygwin
2. lang layer, nodejs python3 etc..
"
  (global-set-key (kbd "C-M-'") 'repl-open)
  )

(defun configure-project ()
  ""
  ;; (configure-perspective)
  ;; (configure-icons)
  (configure-eshell)
  (require-or-install 'projectile)
  (require-or-install 'neotree)
  (require-or-install 'ag)
  (configure-repl)
  ;; (setq projectile-cache-file t)
  )

(deftask project
  "Apply project configs."
  ;; (setq SHELL "/bin/bash emacs")
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

(defun configure-electric-operator ()
  "Configure electric operator."
  (require-or-install 'electric-operator)
  (electric-operator-add-rules-for-mode 'js-mode
                                        (cons "let"   "let ")
                                        (cons "const" "const ")
                                        (cons "var"   "var ")
                                        ;; (cons "if" "if ")
                                        ;; (cons "for" "for ")
                                        ;; (cons "while" "while ")
                                        (cons "switch" "switch ")
                                        (cons "case" "case ")
                                        (cons "new" "new ")
                                        (cons "type" "type ")
                                        (cons "interface" "interface ")
                                        )
  (add-hook 'js-mode-hook 'electric-operator-mode)
  ;; (add-hook 'js-mode-hook 'electric-layout-mode)
  (add-hook 'js-mode-hook 'electric-pair-mode))

(defun lang-javascript ()
  "Configure javascript mode."
  ;; Nodejs-repl
  (configure-nodejs-repl)
  ;; Json
  (configure-json-mode)
  (configure-electric-operator)
  ;; Templates
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



;;; AutoHotKey

(defun lang-ahk ()
  ""
  (require-or-install 'ahk-mode)
  )

(deftask javascript
  "Apply javascript-IDE configs."
  (run-with-timer (ms 100) nil 'lang-ahk))


;; + ac-alchemist (ELPA)
;; + ace-link (ELPA)
;; + ace-window (ELPA)
;; + alchemist (ELPA)
;; + all-the-icons (ELPA)
;; + anaconda-mode (ELPA)
;; + android-mode (ELPA)
;; + async (ELPA)
;; + auctex (ELPA)
;; + auth-password-store (ELPA)
;; + auto-compile (ELPA)
;; + auto-yasnippet (ELPA)
;; + avy (ELPA)
;; + centered-window-mode (ELPA)
;; + circe (ELPA)
;; + circe-notifications (ELPA)
;; + cmake-mode (ELPA)
;; + coffee-mode (ELPA)
;; + command-log-mode (ELPA)
;; + company (ELPA)
;; + company-anaconda (ELPA)
;; + company-auctex (ELPA)
;; + company-dict (ELPA)
;; + company-ghc (ELPA)
;; + company-glsl (QUELPA)
;; + company-go (ELPA)
;; + company-inf-ruby (ELPA)
;; + company-irony (ELPA)
;; + company-irony-c-headers (ELPA)
;; + company-lua (ELPA)
;; + company-php (ELPA)
;; + company-quickhelp (ELPA)
;; + company-racer (ELPA)
;; + company-restclient (ELPA)
;; + company-shell (ELPA)
;; + company-sourcekit (ELPA)
;; + company-statistics (ELPA)
;; + company-tern (ELPA)
;; + company-web (ELPA)
;; + counsel (ELPA)
;; + counsel-css (QUELPA)
;; + counsel-projectile (ELPA)
;; + crystal-mode (QUELPA)
;; + csharp-mode (ELPA)
;; + cuda-mode (ELPA)
;; + demangle-mode (ELPA)
;; + dired-k (ELPA)
;; + disaster (ELPA)
;; + dockerfile-mode (ELPA)
;; + doom-themes (ELPA)
;; + dumb-jump (ELPA)
;; + editorconfig (ELPA)
;; + eldoc-eval (ELPA)
;; + elfeed (ELPA)
;; + elfeed-org (ELPA)
;; + elixir-mode (ELPA)
;; + elm-mode (ELPA)
;; + emmet-mode (ELPA)
;; + ensime (ELPA)
;; + eslintd-fix (ELPA)
;; + evil (ELPA)
;; + evil-anzu (ELPA)
;; + evil-args (ELPA)
;; + evil-commentary (ELPA)
;; + evil-easymotion (ELPA)
;; + evil-embrace (ELPA)
;; + evil-escape (ELPA)
;; + evil-exchange (ELPA)
;; + evil-goggles (ELPA)
;; + evil-indent-plus (ELPA)
;; + evil-ledger (ELPA)
;; + evil-matchit (ELPA)
;; + evil-mc (ELPA)
;; + evil-multiedit (ELPA)
;; + evil-numbers (ELPA)
;; + evil-snipe (ELPA)
;; + evil-surround (ELPA)
;; + evil-textobj-anyblock (ELPA)
;; + evil-vimish-fold (ELPA)
;; + evil-visualstar (ELPA)
;; + expand-region (ELPA)
;; + f (ELPA)
;; + flycheck (ELPA)
;; + flycheck-cask (ELPA)
;; + flycheck-elm (ELPA)
;; + flycheck-irony (ELPA)
;; + flycheck-ledger (ELPA)
;; + flycheck-perl6 (ELPA)
;; + flycheck-plantuml (ELPA)
;; + flycheck-pos-tip (ELPA)
;; + flycheck-rust (ELPA)
;; + flyspell-correct (ELPA)
;; + flyspell-correct-ivy (ELPA)
;; + fringe-helper (ELPA)
;; + gist (ELPA)
;; + git-gutter-fringe (ELPA)
;; + git-link (ELPA)
;; + git-timemachine (ELPA)
;; + gitconfig-mode (ELPA)
;; + gitignore-mode (ELPA)
;; + glsl-mode (ELPA)
;; + go-eldoc (ELPA)
;; + go-guru (ELPA)
;; + go-mode (ELPA)
;; + gorepl-mode (ELPA)
;; + groovy-mode (ELPA)
;; + gxref (ELPA)
;; + haml-mode (ELPA)
;; + haskell-mode (ELPA)
;; + haxor-mode (ELPA)
;; + help-fns+ (ELPA)
;; + highlight-indentation (ELPA)
;; + highlight-numbers (ELPA)
;; + highlight-quoted (ELPA)
;; + hindent (ELPA)
;; + hl-todo (ELPA)
;; + htmlize (ELPA)
;; + hy-mode (ELPA)
;; + hydra (ELPA)
;; + imenu-anywhere (ELPA)
;; + imenu-list (ELPA)
;; + impatient-mode (ELPA)
;; + inf-ruby (ELPA)
;; + intero (ELPA)
;; + irony (ELPA)
;; + irony-eldoc (ELPA)
;; + ivy (ELPA)
;; + ivy-bibtex (ELPA)
;; + ivy-hydra (ELPA)
;; + js2-mode (ELPA)
;; + js2-refactor (ELPA)
;; + json-mode (ELPA)
;; + julia-mode (ELPA)
;; + ledger-mode (ELPA)
;; + less-css-mode (ELPA)
;; + lua-mode (ELPA)
;; + macrostep (ELPA)
;; + magit (ELPA)
;; + makefile-executor (ELPA)
;; + markdown-mode (ELPA)
;; + markdown-toc (ELPA)
;; + meghanada (ELPA)
;; + merlin (ELPA)
;; + mips-mode (ELPA)
;; + modern-cpp-font-lock (ELPA)
;; + moonscript (ELPA)
;; + mu4e-maildirs-extension (ELPA)
;; + multi-term (ELPA)
;; + nasm-mode (ELPA)
;; + nav-flash (ELPA)
;; + neotree (ELPA)
;; + nlinum (ELPA)
;; + nlinum-hl (ELPA)
;; + nlinum-relative (ELPA)
;; + nodejs-repl (ELPA)
;; + nose (ELPA)
;; + ob-go (ELPA)
;; + ob-mongo (ELPA)
;; + ob-redis (ELPA)
;; + ob-restclient (ELPA)
;; + ob-rust (QUELPA)
;; + ob-sql-mode (ELPA)
;; + ob-translate (ELPA)
;; + omnisharp (ELPA)
;; + opencl-mode (ELPA)
;; + org-bullets (QUELPA)
;; + org-download (ELPA)
;; + org-plus-contrib (QUELPA)
;; + org-tree-slide (ELPA)
;; + overseer (ELPA)
;; + ox-pandoc (ELPA)
;; + ox-reveal (ELPA)
;; + pass (ELPA)
;; + password-store (ELPA)
;; + pcre2el (ELPA)
;; + perl6-mode (ELPA)
;; + persp-mode (ELPA)
;; + php-boris (ELPA)
;; + php-extras (QUELPA)
;; + php-mode (ELPA)
;; + php-refactor-mode (ELPA)
;; + phpunit (ELPA)
;; + pip-requirements (ELPA)
;; + plantuml-mode (ELPA)
;; + prodigy (ELPA)
;; + projectile (ELPA)
;; + psc-ide (ELPA)
;; + pug-mode (ELPA)
;; + purescript-mode (ELPA)
;; + quickrun (ELPA)
;; + racer (ELPA)
;; + rainbow-delimiters (ELPA)
;; + rainbow-mode (ELPA)
;; + rake (ELPA)
;; + restclient (ELPA)
;; + rjsx-mode (ELPA)
;; + rotate-text (QUELPA)
;; + rspec-mode (ELPA)
;; + ruby-refactor (ELPA)
;; + rust-mode (ELPA)
;; + s (ELPA)
;; + sass-mode (ELPA)
;; + sbt-mode (ELPA)
;; + scala-mode (ELPA)
;; + shackle (ELPA)
;; + shader-mode (ELPA)
;; + shrink-path (ELPA)
;; + skewer-mode (ELPA)
;; + slime (ELPA)
;; + smart-forward (ELPA)
;; + smartparens (ELPA)
;; + smex (ELPA)
;; + solaire-mode (ELPA)
;; + ssh-deploy (ELPA)
;; + stripe-buffer (ELPA)
;; + stylus-mode (ELPA)
;; + swift-mode (ELPA)
;; + swiper (ELPA)
;; + tern (ELPA)
;; + tide (ELPA)
;; + toc-org (ELPA)
;; + toml-mode (ELPA)
;; + tuareg (ELPA)
;; + twittering-mode (ELPA)
;; + typescript-mode (ELPA)
;; + undo-tree (ELPA)
;; + vi-tilde-fringe (ELPA)
;; + vimrc-mode (ELPA)
;; + visual-fill-column (ELPA)
;; + web-beautify (ELPA)
;; + web-mode (ELPA)
;; + wgrep (ELPA)
;; + which-key (ELPA)
;; + xref-js2 (ELPA)
;; + yaml-mode (ELPA)
;; + yard-mode (ELPA)
;; + yasnippet (ELPA)

;;; hairy.el ends here


;; (f-write (symbol-value (intern-soft "hairy-projectile/template-.gitignore"))
;;          'utf-8
;;          "f:/hairy/projtest/.gitignore")

;; func: goto-config
;; func: goto-view
;; func: goto-core
;; func: restart-server
;; feature: auto restart server when config changed.
;; feature: list deps
;; (start-process "webpack-dev-server" "WebpackDevServer" "yarn" "webpack-dev-server" "--hot")
