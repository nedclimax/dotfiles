;; -*- coding: utf-8; lexical-binding: t -*-

;; Very crappy and messy emacs config

;; TODO for .emacs.d/init.el
;; - fix switch/case indentation for C/C++
;; - make emacs able to find pairs of .c/.h file in C/C++ modes
;; - Replace isearch with something like `swiper'
;; - Refactor packages section with `use-package'

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq warning-minimum-level :emergency)

(define-key prog-mode-map (kbd "S-<tab>") 'indent-for-tab-command)
(define-key prog-mode-map (kbd "<tab>") 'dabbrev-expand)

;; NOTE: System detection
(defconst ned-windows (eq system-type 'windows-nt))
(defconst ned-linux (eq system-type 'gnu-linux))
(defconst ned-mac (eq system-type 'darwin))
(defconst ned-bsd (not (or ned-windows ned-linux ned-mac)))

(when ned-windows
  (setq w32-use-native-image-API t
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)
        w32-get-true-file-attributes nil))

(if ned-windows
    (setq ned-buildscript "build.bat")
  (setq ned-buildscript "build.sh"))

(find-function-setup-keys)

(global-auto-revert-mode 1)

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; NOTE: Languages
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

(autoload 'hlsl-mode "hlsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fx\\'" . hlsl-mode))
(add-to-list 'auto-mode-alist '("\\.hlsl\\'" . hlsl-mode))

(autoload 'odin-mode "odin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(autoload 'markdown-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'powershell-mode "powershell-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm\\'" . powershell-mode))
;; End Languages

;; NOTE: hex editor for binary files
(add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.exe\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.pdb\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.dll\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.a\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.la\\'" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.so\\'" . hexl-mode))

(require 'dimmer)

(setq dimmer-fraction 0.3)
(dimmer-mode t)

;;; Packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))


;; NOTE: multiple cursors
(unless (package-installed-p 'multiple-cursors)
  (package-refresh-contents)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
(setq mc/always-run-for-all t)
(define-key mc/keymap (kbd "<return>") nil)
(multiple-cursors-mode 1)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-c") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C-SPC") 'mc/edit-lines)
;; End multiple cursors


;; NOTE: auto completion pop up
(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))
(require 'company)
(global-company-mode 1)
;; End auto completion pop up

;; NOTE: LSP intergration
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))
(require 'eglot)
(define-key eglot-mode-map (kbd "<tab>") 'company-complete)
(defun do-nothing(&rest params))
(fset 'eglot-format 'do-nothing)
(fset 'eglot-format-buffer 'do-nothing)
(add-to-list 'eglot-server-programs
             `((c++-mode c-mode)
               . ("clangd"
                  "--clang-tidy"
                  "--header-insertion=never"
                  "--inlay-hints=false")))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((odin-mode) "ols"))
(add-hook 'odin-mode-hook 'eglot-ensure)
;; (add-to-list 'eglot-server-programs '((zig-mode) "zls"))
;; (add-hook 'zig-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((glsl-mode) "glsl_analyzer"))
(add-hook 'glsl-mode-hook 'eglot-ensure)
(add-to-list 'eglot-server-programs '((latex-mode) "texlab"))
(add-hook 'latex-mode-hook 'eglot-ensure)
;; End LSP integration

;; NOTE: Project management
(unless (package-installed-p 'projectile)
  (package-refresh-contents)
  (package-install 'projectile))
(require 'projectile)
;; TODO: Register custom project types
;; (projectile-register-project-type nil '("build.bat" "build.sh")
;;                                   :compile "build"
;;                                   :run "build run")
(projectile-mode t)
;; End Project management


;; NOTE: Git integration
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))
;; End Git integration


;; NOTE: AucTeX
(unless (package-installed-p 'auctex)
  (package-refresh-contents)
  (package-install 'auctex))
(require 'latex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-engine-alist '((default
                          "Tectonic"
                          "tectonic -X compile -f plain %T"
                          "tectonic -X watch"
                          nil)))
(setq LaTeX-command-style '(("" "%(latex)")))
(setq TeX-process-asynchronous t
      TeX-check-TeX nil
      TeX-engine 'default)
(let ((tex-list (assoc "TeX" TeX-command-list))
      (latex-list (assoc "LaTeX" TeX-command-list)))
  (setf (cadr tex-list) "%(tex)"
        (cadr latex-list) "%l"))
;; End Auctex

;;; End Packages

(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-$") 'hs-toggle-hiding)

(require 'cc-mode)

;; NOTE: UTF-8 as default encoding
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; NOTE: fixes terminal output on Windows
(set-terminal-coding-system 'utf-8-unix)

;; NOTE: Colors & UI
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
(column-number-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; Fullscreen on startup

(set-fringe-mode '(1 . 1)) ;; Remove fringes

(cond
 ((find-font (font-spec :name "Iosevka Mono"))
  (set-face-attribute 'default nil :font "Iosevka Mono" :height 100))
 ((find-font (font-spec :name "Consolas"))
  (set-face-attribute 'default nil :font "Consolas" :height 100 :weight 'medium)))
(set-face-attribute 'default nil :font "Consolas" :height 100 :weight 'medium)
(global-hl-line-mode 0)
(set-face-background 'hl-line "#252535")

(set-face-attribute font-lock-builtin-face nil :foreground "#A85E1D")
(set-face-attribute font-lock-comment-face nil :foreground "gray50")
(set-face-attribute font-lock-doc-face nil :foreground "gray50")
(set-face-attribute font-lock-keyword-face nil :foreground "darkgoldenrod3")
(set-face-attribute font-lock-function-name-face nil :foreground "burlywood")
(set-face-attribute font-lock-string-face nil :foreground "olivedrab")
(set-face-attribute font-lock-constant-face nil :foreground "#A85E1D")
(set-face-attribute font-lock-type-face nil :foreground "darkgoldenrod")
(set-face-attribute font-lock-preprocessor-face nil :foreground "darkgoldenrod")

(set-face-background 'mode-line "burlywood3")
(set-face-background 'mode-line-inactive "gray25")
(set-face-foreground 'mode-line-inactive "#ffffff")

(set-background-color "#161616")
(set-foreground-color "burlywood3")
(set-cursor-color "green")

;; Color TODOs and NOTEs
(make-face 'font-lock-todo-face)
(make-face 'font-lock-hack-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-important-face)

(setq hl-modes '(c++-mode
                 c-mode
                 emacs-lisp-mode
                 odin-mode
                 python-mode
                 lua-mode
                 glsl-mode
                 hlsl-mode))

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
           ("\\<\\(HACK\\)" 1 'font-lock-hack-face t)
           ("\\<\\(COPYPASTA\\)" 1 'font-lock-hack-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
           ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
           ("\\<\\(FIXME\\)" 1 'font-lock-todo-face t)
           )))
      hl-modes)

(modify-face 'font-lock-todo-face "red" nil nil t nil t nil nil)
(modify-face 'font-lock-hack-face "orange" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "darkgreen" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "yellow" nil nil t nil t nil nil)

(setq blink-cursor-blinks 0
      frame-title-format '("emacs@" system-name)
      inhibit-splash-screen t
      truncate-lines t
      redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      ;; NOTE: Fix weird mouse wheel movement
      mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t)
;; End Colors & UI

;; Hooks
(add-hook 'window-setup-hook (lambda ()
                               (interactive)
                               (toggle-truncate-lines t)
                               (split-window-horizontally)))

(add-hook 'prog-mode-hook (lambda ()
                            (make-local-variable 'truncate-lines)
                            (make-local-variable 'indent-tabs-mode)
                            (setq indent-tabs-mode t)
                            (setq truncate-lines t)))

;; HACK: whitespace-cleanup replaces 4 spaces with 2 tabs
;; when saving a file which causes python to freak out.
(add-hook 'python-mode-hook (lambda ()
                              (make-local-variable 'indent-tabs-mode)
                              (setq indent-tabs-mode nil)
                              (add-hook 'before-save-hook nil)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (make-local-variable 'indent-tabs-mode)
                                  (setq indent-tabs-mode nil)))

(add-hook 'org-mode-hook (lambda ()
                           (make-local-variable 'truncate-lines)
                           (setq truncate-lines nil)
                           (visual-line-mode 1)))
;; End Hooks

(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; NOTE: Put Emacs auto-save and backup files to /tmp/ or C:/Temp/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-list-file-prefix emacs-tmp-dir
      auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
      backup-directory-alist `((".*" . ,emacs-tmp-dir)))

(fset 'yes-or-no-p 'y-or-n-p)

(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq backward-delete-char-untabify-method nil)

(require 'ido)
(fido-mode 1)
(fido-vertical-mode 1)

(require 'compile)
(setq compilation-scroll-output t)
(add-hook 'compilation-mode-hook (lambda ()
                                   (make-local-variable 'truncate-lines)
                                   (setq truncate-lines nil)))
(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

(electric-pair-mode 1)
(electric-indent-mode 1)
(electric-layout-mode 1)

;; NOTE: this converts normal quotes into fancy quotes
;; (electric-quote-mode 0)

(show-paren-mode 1)

;; TODO: make emacs able to find pairs of .c/.h file in C/C++ modes
;; (setq find-sibling-rules ("\\([^/]+\\)\\.c\\'" "\\1.h"))

(setq ring-bell-function 'ignore)

(setq create-lockfiles nil)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "linux")
                        (other . "k&r")))

; TODO: Fix switch/case indentation
;(add-to-list c-hanging-braces-alist )

(load "smart-tabs.el" t t)
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)

(setq dabbrev-case-replace t)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-distinction t)
(setq dabbrev-upcase-means-case-search t)

(abbrev-mode 1)

(require 'text-edit)

(global-set-key (kbd "C-,") 'other-window)

(global-set-key (kbd "C-M-,") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-M-f") 'find-file-other-window)
(global-set-key (kbd "C-M-b") 'switch-to-buffer-other-window)
(global-set-key (kbd "C-p") 'pop-to-mark-command)
(global-set-key (kbd "C-n") 'recenter)

(global-set-key (kbd "M-f") 'write-file)

;; NOTE: Disable unused keybindings
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "C-M-n"))
(global-unset-key (kbd "C-M-p"))

(global-set-key [(insert)] nil) ;; Insert is stupid

(setq compile-command 'ned-buildscript)

(setq shift-select-mode nil)

(global-set-key (kbd "M-RET") 'recompile)

(global-set-key (kbd "C-l") 'duplicate-line-or-region-below)
(global-set-key (kbd "C-M-l") 'duplicate-line-or-region-above)

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

(global-set-key (kbd "C-<return>") 'open-newline-below)
(global-set-key (kbd "C-M-<return>") 'open-newline-above)

(global-set-key (kbd "M-<f4>") 'save-buffers-kill-emacs)

(global-set-key (kbd "M-b") 'mark-line)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(require 'gcmh)
(gcmh-mode 1)

(autoload 'diminish "diminish" nil t)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'gcmh-mode)
(diminish 'hs-minor-mode)

;;;;
;; the end
;;;;
