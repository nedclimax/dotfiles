(require 'ansi-color)

(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-c p") 'find-file-at-point)
(global-set-key (kbd "C-c i b") 'ibuffer)
(global-set-key (kbd "C-c i m") 'imenu)
(global-set-key (kbd "C-z") 'ignore)

(setq ring-bell-fonction 'ignore
	  inhibit-startup-screen 1
      visible-bell 1
	  tab-width 4
 	  indent-tabs-mode nil
	  compilation-scroll-output 1)

(defun rc-reload-config ()
  "Reload configuration file"
  (interactive)
  (load-file (expand-file-name ".emacs" (getenv "HOME"))))

(global-set-key (kbd "C-c r") 'rc-reload-config)

(defun rc-backwards-kill-word ()
  "Kill word without copying it to your clipboard"
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(global-set-key (kbd "M-DEL") 'rc-backward-kill-word)
(global-set-key (kbd "C-DEL") 'rc-backward-kill-word)

(defun rc-duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc-duplicate-line)

(defun rc-start-python-http-server ()
  "Start a simple python http server"
  (interactive)
  (shell-command "python -m http.server"))

(global-set-key (kbd "C-c s") 'rc-start-python-http-server)

(setq confirm-kill-emacs 'y-or-n-p)
