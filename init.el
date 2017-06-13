;;; init.el --- Where all the magic begins
;;

;;; Commentary:
;;
;; This is a starter kit for smax. This package provides a
;; customized setup for emacs that we use daily for scientific
;; programming and publication.
;;

;;; Code:
;; this makes garbage collection less frequent, which speeds up init by about 2 seconds.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)

(setq global-mark-ring-max 256
      mark-ring-max 256
      kill-ring-max 256)

(defun minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun minibuffer-exit-hook ()
  (setq gc-cons-threshold 1048576))

(add-hook 'minibuffer-setup-hook #'minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'minibuffer-exit-hook)

(when (version< emacs-version "24.4")
  (warn "You probably need at least Emacs 24.4. You should upgrade. You may need to install leuven-theme manually."))

;; remember this directory
(defconst smax-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where the smax is installed.")

(defvar user-dir (expand-file-name "user" smax-dir)
  "User directory for personal code.")

(defvar conf-dir (expand-file-name "smax-lisp" smax-dir)
  "Directory for general configurations.")

(setq user-emacs-directory user-dir)

(setq package-user-dir (expand-file-name "elpa"  smax-dir))

;; we load the user/preload.el file if it exists. This lets users define
;; variables that might affect packages when they are loaded, e.g. key-bindings,
;; etc... setup autoupdate, .

(let ((preload (expand-file-name "user/preload.el" smax-dir)))
  (when (file-exists-p preload)
    (load preload)))

(defvar smax-load-user-dir t
  "Controls if the user directory is loaded.")

(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(add-to-list
 'package-archives
 '("org"         . "http://orgmode.org/elpa/")
 t)

(add-to-list 'load-path smax-dir)
(add-to-list 'load-path conf-dir)
(dolist (f (directory-files conf-dir))
  (let ((name (concat conf-dir "/" f)))
    (when (and (file-directory-p name)
               (not (equal f ".."))
               (not (equal f ".")))
      (add-to-list 'load-path name))))
(add-to-list 'load-path user-dir)

(let ((default-directory smax-dir))
  (shell-command "git submodule update --init"))

(require 'smax-bootstrap)
(require 'smax-packages)
(require 'smax-finish)
(provide 'init)

;;; init.el ends here
