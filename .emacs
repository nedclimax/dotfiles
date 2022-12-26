(load "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(rc-require 'gruber-darker-theme)
(load-theme 'gruber-darker t)

(set-frame-font "JetBrains Mono 10" nil t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

;;; IDO
(rc-require 'smex)
(ido-mode 1)
(ido-everywhere 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; C-mode
(setq-default c-basic-offset 4
	          c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))

;; Magit
(rc-require 'cl-lib)
(rc-require 'magit)

(setq magit-auto-revert-mode nil)

(global-set-key (kbd "C-c m s") 'magit-status)
(global-set-key (kbd "C-c m l") 'magit-log)

;; Multiple cursors
(rc-require 'multiple-cursors)

(multiple-cursors-mode 1)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-w") 'mc/mark-next-like-this-word)

;; Yasnippet
(rc-require 'yasnippet)

(setq yas/triggers-in-field nil)
(setq yas-snippet-dirs '("~/.emacs.snippets/"))

(yas-global-mode 1)

;; Powershell
(rc-require 'powershell)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))
