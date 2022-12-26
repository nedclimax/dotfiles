;;; For performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(add-hook 'after-it-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000)))

;;; Offload custom-set-variable to a seperate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;;; Put auto-save and backup file to /tmp/ or C:/Temp
(defconst emacs-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 backup-directory-alist `((".*" . ,emacs-tmp-dir)))

;; Disable lockfiles
(setq create-lockfiles nil)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless package--initialized (package-initialize))

(defvar rc-package-contents-refreshed nil)

(defun rc-package-refresh-contents-once ()
  (when (not rc-package-contents-refreshed)
    (setq rc-package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc-require-one-package (package)
  (when (not (package-installed-p package))
    (rc-package-refresh-contents-once)
    (package-install package)))

(defun rc-require (&rest packages)
  (dolist (package packages)
    (rc-require-one-package package)))
