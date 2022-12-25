;;; For performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000)))

;;; Offload custom-set-variable to a seperate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;;; Put auto-save and backup file to /tmp/ or C:/Temp
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
 backup-directory-alist `((".*" . ,emacs-tmp-dir)))

(setq create-lockfiles nil)
