;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;; GccEmacs (native-comp) stuff
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;;; Settings without corresponding packages

(use-package emacs
  :preface
  (defvar ian/indent-width-standard 4)
  (defvar ian/indent-width-compact 2)
  (defvar ian/ellipsis " ▶ ...")
  ;; Ref: https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/
  (defun ian/set-buffer-local-ellipsis (ellipsis)
    (let* ((display-table (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value (vconcat (mapcar (lambda (c)
                                     (+ face-offset c))
                                   (string-trim-right ellipsis)))))
      (set-display-table-slot display-table 'selective-display value)
      (setq buffer-display-table display-table)))
  (defun ian/maybe-set-default-browser ()
    "When in WSL Emacs, open links in default Windows 11 browser."
    (when (and (eq system-type 'gnu/linux)
               (string-match "Linux.*microsoft.*Linux"
                             (shell-command-to-string "uname -a")))
      (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
            browse-url-generic-args '("/c" "start" "")
            browse-url-browser-function 'browse-url-generic)))
  (defun ian/split-window-below-and-follow ()
    "Split window below and follow, keeping both windows aligned
with the original view (no horizontal/vertical jump)."
    (interactive)
    (let ((win-start (window-start))
          (win-hscroll (window-hscroll))
          (curr-window (selected-window)))
      (split-window-below)
      (other-window 1)
      (let ((new-window (selected-window)))
        (set-window-start curr-window win-start t)
        (set-window-hscroll curr-window win-hscroll)
        (set-window-start new-window win-start t)
        (set-window-hscroll new-window win-hscroll))))
  (defun ian/split-window-right-and-follow ()
    "Split window to the right and follow, keeping both windows aligned
with the original view (no horizontal/vertical jump)."
    (interactive)
    (let ((win-start (window-start))
          (win-hscroll (window-hscroll))
          (curr-window (selected-window)))
      (split-window-right)
      (other-window 1)
      (let ((new-window (selected-window)))
        (set-window-start curr-window win-start t)
        (set-window-hscroll curr-window win-hscroll)
        (set-window-start new-window win-start t)
        (set-window-hscroll new-window win-hscroll))))
  :config
  (setq user-full-name "Ian Y.E. Pan")
  (setq frame-title-format '("Emacs " emacs-version))
  (setq ring-bell-function 'ignore)
  (setq-default default-directory "~/")
  (setq frame-resize-pixelwise t)
  (setq scroll-conservatively 101) ; > 100
  (setq scroll-preserve-screen-position t)
  (setq auto-window-vscroll nil)
  (setq hscroll-step 1)
  (setq scroll-step 1)
  (setq hscroll-margin 0)
  (setq load-prefer-newer t)
  (setq inhibit-compacting-font-caches t)
  (setq echo-keystrokes 0.4)
  (setq kill-buffer-query-functions nil)
  (setq delete-by-moving-to-trash t)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (global-set-key (kbd "C-x 2") #'ian/split-window-below-and-follow)
  (global-set-key (kbd "C-x 3") #'ian/split-window-right-and-follow)
  (global-set-key (kbd "C-s")   #'save-buffer)
  (unless (display-graphic-p)
    (global-set-key (kbd "C-h") #'backward-kill-word))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 8)
  (setq initial-scratch-message "")
  (global-unset-key (kbd "M-`")) ; conflict with Zellij

  ;; Some performance tweaks and QOL changes
  ;; Ref: https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t)
  (setq redisplay-skip-fontification-on-input t)
  (setq read-process-output-max (* 4 1024 1024)) ; 4MB
  (setq-default cursor-in-non-selected-windows nil)
  (setq window-combination-resize t)
  (advice-add 'save-place-find-file-hook :after
              (lambda (&rest _)
                (when buffer-file-name (ignore-errors (recenter)))))

  ;;; Fix annoying vertical window splitting.
  ;;; Ref: https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
  (with-eval-after-load "window"
    (fmakunbound #'split-window-sensibly)
    (defun split-window-sensibly (&optional window)
      (setq window (or window (selected-window)))
      (cond ((window-splittable-p window t)
             (split-window window nil 'right))
            ((window-splittable-p window)
             (split-window window nil 'below))
            ((and (eq window (frame-root-window (window-frame window)))
                  (not (window-minibuffer-p window)))
             ;; If WINDOW is the only window in its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil 'right)))))))
  (setq-default split-height-threshold  4
                split-width-threshold   160) ; the reasonable limit for horizontal splits

  (setq max-specpdl-size 10000)
  (setq max-lisp-eval-depth 10000)
  (set-default 'truncate-lines t)
  (ian/maybe-set-default-browser)
  (setq jit-lock-defer-time 0)
  (setq fast-but-imprecise-scrolling t)
  (xterm-mouse-mode +1)
  (bind-key* (kbd "<f4>") #'(lambda ()
                              (interactive)
                              (if (one-window-p)
                                  (kill-current-buffer)
                                (kill-buffer-and-window)))))

(use-package uniquify
  :ensure nil
  :config
  (setq-default uniquify-buffer-name-style 'forward))

;; Overriding built-in function
(defun use-fancy-splash-screens-p ()
  "Never display splash screen with Emacs PNG logo."
  nil)

;;; Built-in packages

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "to-be-dumped.el")))

(use-package simple
  :ensure nil
  :config
  (column-number-mode +1))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :preface
  (defun ian/find-file-sudo-root ()
    "Open a file as the root user.
Reference: https://www.emacswiki.org/emacs/TrampMode#h5o-19"
    (interactive)
    (require 'tramp)
    (let* ((name (or buffer-file-name default-directory))
           (tramp (and (tramp-tramp-file-p name)
                       (tramp-dissect-file-name name)))
           path dir file)
      (when tramp ; If called from a "root" file, we need to fix up the path.
        (setq path (tramp-file-name-localname tramp)
              dir (file-name-directory path)))
      (when (setq file (read-file-name "Find file (sudo): " dir path))
        (find-file (concat "/sudo:root@localhost:" file)))))
  :config
  (remove-hook 'find-file-hook 'vc-refresh-state) ; makes open files faster
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (setq make-backup-files nil)
  (setq revert-without-query '(".*"))
  (global-set-key (kbd "C-c C-f") #'ian/find-file-sudo-root)
  (global-set-key (kbd "<f5>") #'(lambda ()
                                   (interactive)
                                   (revert-buffer)
                                   (message "Refreshing buffer...done"))))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers nil)
  (setq auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode -1)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.4))

(use-package js
  :ensure nil
  ;; :mode ("\\.jsx?\\'" . js-jsx-mode)
  :config
  (setq js-indent-level ian/indent-width-compact)
  (add-hook 'flycheck-mode-hook
            #'(lambda ()
                (let* ((root (locate-dominating-file
                              (or (buffer-file-name) default-directory)
                              "node_modules"))
                       (eslint (and root (expand-file-name "node_modules/.bin/eslint" root))))
                  (when (and eslint (file-executable-p eslint))
                    (setq-local flycheck-javascript-eslint-executable eslint))))))

(use-package cc-vars
  :ensure nil
  :mode ("\\.inl\\'" . c++-mode)
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (c++-mode  . "bsd")
                          (c-mode    . "bsd")
                          (other     . "k&r")))
  (setq-default c-basic-offset ian/indent-width-standard))

(use-package cc-mode
  :ensure nil
  :config
  (define-key c++-mode-map ":" nil)
  (add-hook 'c-mode-hook (lambda () (setq comment-start "//"
                                          comment-end   ""))))

(use-package python
  :ensure nil
  :mode ("\\.gyp\\'" . python-mode)
  :config
  (setq python-indent-offset ian/indent-width-standard)
  (setq python-shell-interpreter "python3"))

(use-package ruby-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package css-mode ; inherited by less-css-mode
  :ensure nil
  :config
  (setq css-indent-offset ian/indent-width-compact))

(use-package sh-script
  :ensure nil
  :config
  (with-eval-after-load 'company
    (add-hook 'sh-mode-hook #'(lambda () (company-mode -1)))))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :preface
  (defconst small-fonts-list '("Consolas" "Ubuntu Mono" "Fixedsys Excelsior" "Inconsolata"))
  (defconst tight-fonts-list '("Consolas" "Ubuntu Mono" "Monaco" "Comic Mono"))
  (defun ian/set-default-fonts (english-font chinese-font font-size font-weight)
    "Set the default Latin and CJK font families, as well as the line height."
    (interactive)
    (when (member english-font small-fonts-list)
      (setq font-size (round (* font-size 1.1))))
    (when (member english-font (font-family-list))
      (set-face-attribute 'default nil :family english-font :height font-size :weight font-weight))
    (when (member chinese-font (font-family-list))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family chinese-font
                                             :size (* (/ font-size 10) 1.0)))))
    (setq-default line-spacing (if (member english-font tight-fonts-list) 2 1)))
  (defun ian/set-big-fonts ()
    (interactive)
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 95 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 95 :weight 'normal)))
  (defun ian/set-small-fonts ()
    (interactive)
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 85 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 0.9 :weight 'normal)))
  :ensure nil
  :config
  (setq default-frame-alist (append (list '(width . 74) '(height . 35) '(internal-border-width . 2))))
  (if (display-graphic-p)
      (add-to-list 'default-frame-alist '(inhibit-double-buffering . t)))
  (blink-cursor-mode +1)
  (setq blink-cursor-blinks -1) ; blink forever
  (ian/set-big-fonts))

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(use-package flyspell
  :ensure nil
  :hook ((markdown-mode . flyspell-mode)
         (org-mode      . flyspell-mode))
  :config
  (setq ispell-program-name "/usr/bin/aspell")
  (add-hook 'git-commit-setup-hook 'git-commit-setup-flyspell))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :config
  (setq electric-pair-preserve-balance nil))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "~/.local/lib/.*"))
  (add-to-list 'recentf-exclude (format "%s/.local/lib/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/usr/lib/.*")
  (add-to-list 'recentf-exclude "/usr/include/.*")
  (recentf-mode +1))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (yaml-mode . display-line-numbers-mode))
  :config
  (setq-default display-line-numbers-width 3))

(use-package xref
  :ensure nil
  :preface
  (defun ian/xref-recenter-quarter-top-in-new-buffer-a (func &rest args)
    "When xref opens a new buffer, reposition the cursor at 1/4 window height from top.
This follows the UX design of Visual Studio Code."
    (let ((original-buf (current-buffer)))
      (apply func args)
      (unless (eq (current-buffer) original-buf)
        (recenter-top-bottom (/ (window-body-height) 4)))))
  :config
  (advice-add 'xref-find-definitions :around #'ian/xref-recenter-quarter-top-in-new-buffer-a)
  (setq xref-search-program 'ripgrep)
  (setq xref-after-jump-hook '(xref-pulse-momentarily))
  (setq xref-after-return-hook '(xref-pulse-momentarily))
  (setq xref-prompt-for-identifier nil))

(use-package zone
  :ensure nil
  :preface
  (defun ian/zone-choose-pgm (pgm)
    "Choose a zone program from a hand-picked list."
    (interactive (list (completing-read "Program: " '(zone-pgm-putz-with-case
                                                      zone-pgm-rotate-LR-lockstep
                                                      zone-pgm-drip
                                                      zone-pgm-whack-chars))))
    (let ((zone-programs (vector (intern pgm))))
      (zone))))

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1))

(use-package gdb-mi
  :ensure nil
  :config
  (setq gdb-many-windows t))

(use-package whitespace
  :ensure nil
  :config
  (delete 'lines whitespace-style)
  (delete 'newline-mark whitespace-style)
  (bind-key* (kbd "C-c w") #'(lambda ()
                               (interactive)
                               (whitespace-mode 'toggle))))

(use-package advice
  :ensure nil
  :init
  (setq ad-redefinition-action 'accept))


;;; Third-party Packages

;; GUI enhancements

;; (use-package vscode-dark-plus-theme
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; (use-package spacemacs-theme
;; :custom-face
;; (region ((t (:extend nil))))
;; :config
;; (load-theme 'spacemacs-light t))

;; (use-package doom-themes
;;   :custom-face
;;   (region                         ((t (:extend nil))))
;;   (highlight-symbol-face          ((t (:background "#254256" :distant-foreground "#bbbbbb"))))
;;   (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
;;   (link                           ((t (:foreground "#3794ff"))))
;;   (evil-ex-substitute-replacement ((t (:strike-through nil))))
;;   (vertical-border                ((t (:foreground "black" :background nil))))
;;   (fringe                         ((t (:background nil))))
;;   :config
;;   (setq doom-themes-enable-bold nil)
;;   (setq doom-themes-enable-italic nil)
;;   (load-theme 'doom-gruvbox t))

(use-package emacs
  :ensure nil
  :preface
  (defconst ian/light-theme 'modus-operandi)
  (defconst ian/dark-theme 'modus-vivendi)
  (defun ian/toggle-theme ()
    "Toggle between light and dark themes."
    (interactive)
    (let* ((current (car custom-enabled-themes))
           (next-theme (if (eq current ian/dark-theme)
                           ian/light-theme
                         ian/dark-theme)))
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme next-theme t)
      (setq frame-background-mode
            (if (eq next-theme ian/dark-theme) 'dark 'light))
      (frame-set-background-mode (selected-frame))
      (message "Switched to %s" next-theme)))
  :custom-face
  (region             ((t (:extend nil))))
  (mode-line          ((t (:underline nil))))
  (mode-line-active   ((t (:underline nil))))
  (mode-line-inactive ((t (:underline nil))))
  :config
  (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
  (setq frame-background-mode 'dark)
  (frame-set-background-mode (selected-frame))
  (load-theme ian/dark-theme t)
  (global-set-key (kbd "C-c t") #'ian/toggle-theme))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Vi keybindings

(use-package undo-fu
  :config
  (setq undo-fu-ignore-keyboard-quit t))

(use-package evil
  :after undo-fu
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-shift-width ian/indent-width-compact)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  :hook (after-init . evil-mode)
  :preface
  (defun ian/paste-with-ctrl-shift-v ()
    "Paste with Ctrl-Shift-v, as inspired by Windows Terminal shortcut."
    (interactive)
    (evil-normal-state nil)
    (evil-paste-after 1)
    (evil-insert-state nil)
    (right-char))
  (defun ian/pulse-line ()
    "Flash highlight the current line with region face"
    (interactive)
    (pulse-momentary-highlight-one-line (point) 'region))
  :config
  (setq evil-normal-state-tag   " -- NORMAL --  "
        evil-insert-state-tag   " -- INSERT --  "
        evil-visual-state-tag   " -- VISUAL --  "
        evil-replace-state-tag  " -- REPLACE -- "
        evil-motion-state-tag   " -- MOTION --  "
        evil-operator-state-tag " -- OPERATOR --"
        evil-emacs-state-tag    " -- EMACS --   ")
  (setq evil-mode-line-format '(after . mode-line-buffer-identification))
  (setq evil-insert-state-cursor '(bar . 1))
  (setq evil-kill-on-visual-paste nil)
  (setq-default evil-symbol-word-search t)
  (define-key evil-normal-state-map (kbd "C-w C-o") #'(lambda () (interactive) (neotree-hide) (delete-other-windows)))
  (define-key evil-normal-state-map (kbd "C-o") #'(lambda () (interactive) (evil-jump-backward) (ian/pulse-line)))
  (define-key evil-normal-state-map (kbd "C-i") #'(lambda () (interactive) (evil-jump-forward) (ian/pulse-line)))
  (unless (display-graphic-p) ;; TAB and C-i are indistinguishable in the terminal, hence the following workaround.
    (evil-define-key '(motion normal) profiler-report-mode-map (kbd "TAB") #'profiler-report-toggle-entry)
    (evil-define-key '(motion normal) org-mode-map (kbd "TAB") #'org-cycle)
    (evil-define-key '(motion normal) markdown-mode-map (kbd "TAB") #'markdown-cycle))
  (bind-key* (kbd "M-<up>") #'(lambda () (interactive) (scroll-down 2)))
  (bind-key* (kbd "M-<down>") #'(lambda () (interactive) (scroll-up 2)))
  (bind-key* (kbd "M-<left>") #'(lambda () (interactive) (scroll-right 2)))
  (bind-key* (kbd "M-<right>") #'(lambda () (interactive) (scroll-left 2)))
  (if (display-graphic-p)
      (define-key evil-normal-state-map (kbd "z <return>") #'evil-scroll-line-to-top)
    (define-key evil-normal-state-map (kbd "z RET") #'evil-scroll-line-to-top))
  (define-key evil-insert-state-map (kbd "C-n") nil) ; avoid conflict with company tooltip selection
  (define-key evil-insert-state-map (kbd "C-p") nil) ; avoid conflict with company tooltip selection
  (define-key evil-normal-state-map (kbd "C-S-c") #'evil-yank)
  (define-key evil-insert-state-map (kbd "C-S-v") #'ian/paste-with-ctrl-shift-v)
  (define-key evil-normal-state-map (kbd "u") #'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") #'undo-fu-only-redo)
  (define-key evil-normal-state-map (kbd "C-u") #'(lambda () (interactive) (evil-scroll-up 0) (recenter)))
  (define-key evil-normal-state-map (kbd "C-d") #'(lambda () (interactive) (evil-scroll-down 0) (recenter)))
  (define-key evil-normal-state-map (kbd "ZZ") nil)
  (define-key evil-normal-state-map (kbd "ZQ") nil)
  (evil-define-key '(motion normal) prog-mode-map (kbd "gd")
    #'(lambda ()
        (interactive)
        (evil-set-jump)
        (call-interactively #'xref-find-definitions)
        (ian/pulse-line)))
  (evil-define-key '(motion normal) prog-mode-map (kbd "gD")
    (lambda ()
      (interactive)
      (evil-set-jump)
      (call-interactively #'xref-find-references)
      (ian/pulse-line)))
  ;; (evil-define-key 'motion prog-mode-map (kbd "gd") #'lsp-bridge-find-def)
  ;; (evil-define-key 'motion prog-mode-map (kbd "gD") #'lsp-bridge-find-references)
  (with-eval-after-load 'magit
    ;; Entering Magit COMMIT_EDITMSG does not correctly set evil cursor to (bar . 1)
    ;; Temporary workaround until evil fixes the cursor refresh and display timing upstream.
    (add-hook 'git-commit-setup-hook
              (lambda ()
                (setq-local cursor-type (evil-state-property evil-state :cursor t)))
              t))
  (with-eval-after-load 'lsp-ui
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (define-key evil-normal-state-local-map (kbd "gD") #'lsp-ui-peek-find-references)))))
  (evil-ex-define-cmd "q" #'kill-current-buffer)
  (evil-ex-define-cmd "wq" #'(lambda () (interactive) (save-buffer) (kill-current-buffer)))
  (evil-set-leader '(motion normal) (kbd "SPC"))
  (evil-define-key '(motion normal) 'global
    (kbd "<leader>d")     #'dired
    (kbd "<leader>g")     #'magit-status
    (kbd "<leader>b")     #'ivy-switch-buffer
    (kbd "<leader>s")     #'counsel-grep-or-swiper
    (kbd "<leader>w")     #'save-buffer
    (kbd "<leader>f")     #'project-find-file
    (kbd "<leader>F")     #'ian/counsel-project-rg-or-rg
    (kbd "<leader>r")     #'dirvish
    (kbd "<leader>o")     #'other-window
    (kbd "<leader>0")     #'delete-window
    (kbd "<leader>1")     #'delete-other-windows
    (kbd "<leader>2")     #'ian/split-window-below-and-follow
    (kbd "<leader>3")     #'ian/split-window-right-and-follow
    (kbd "<leader>a")     #'evil-buffer
    (kbd "<leader>/")     #'avy-goto-word-1
    (kbd "<leader><tab>") #'ian/lsp-execute-code-action
    (kbd "<leader>TAB")   #'ian/lsp-execute-code-action
    (kbd "<leader>e")     #'ian/neotree-project-toggle))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (setq evil-collection-ielm-move-cursor-back t)
  (setq evil-collection-repl-submit-state 'insert)
  (setq evil-collection-key-blacklist '("SPC"))
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode +1))

(use-package evil-matchit
  :hook ((web-mode
          html-mode
          mhtml-mode
          js-mode
          typescript-mode
          ) . turn-on-evil-matchit-mode))

(use-package evil-surround
 :after evil
 :config
 (global-evil-surround-mode +1))

;; Git integration

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame-addition))
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  (local-unset-key (kbd "f"))
  (mapatoms
   (lambda (sym)
     (when (and (boundp sym)
                (keymapp (symbol-value sym))
                (string-match-p "\\`magit-.*-mode-map\\'" (symbol-name sym)))
       (define-key (symbol-value sym) (kbd "SPC") nil))))
  (setq magit-revision-insert-related-refs nil)
  (with-eval-after-load 'transient
    (add-to-list 'transient-values
                 '(magit-log:magit-log-mode "--decorate" "-n256"))
    (add-to-list 'transient-values
                 '(magit-log:magit-log-select-mode "--decorate" "-n256"))
    (add-to-list 'transient-values
                 '(magit-pull "--rebase")))
  (define-key magit-mode-map (kbd "<f5>") #'(lambda ()
                                              (interactive)
                                              (magit-refresh)
                                              (message "Refreshing Magit...done"))))

(use-package git-gutter
  :if (display-graphic-p)
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.05))

(use-package git-gutter-fringe
  :if (display-graphic-p) ;; Prefer git-gutter-fringe over diff-hl in GUI Emacs
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "..XXXXX."
    "..XXXXX."
    "..XXXXX."
    "..XXXXX.")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "..XXXXX."
    "..XXXXX."
    "..XXXXX."
    "..XXXXX.")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."
    "XXXXX..."
    "XXXXXX.."
    "XXXXXXX."
    "XXXXXXXX"
    "XXXXXXX."
    "XXXXXX.."
    "XXXXX..."
    "XXXX...."
    "XXX....."
    "XX......"
    "X......."))

(use-package diff-hl
  :unless (display-graphic-p) ;; Prefer diff-hl over git-gutter-fringe in TUI Emacs
  :custom-face
  (diff-hl-insert     ((t (:background unspecified :foreground "#81b88b" :slant normal))))
  (diff-hl-delete     ((t (:background unspecified :foreground "#ca4b51" :slant normal))))
  (diff-hl-change     ((t (:background unspecified :foreground "#66afe0" :slant normal))))
  :config
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (setq diff-hl-margin-symbols-alist '((insert  . "┃") ; U+02503 (box drawings heavy vertical)
                                       (delete  . "▶")
                                       (change  . "┃")
                                       (unknown . "┃")
                                       (ignored . "┃")))
  (diff-hl-margin-mode +1)
  (setq diff-hl-flydiff-delay 0.05) ; Need to be set before enabling diff-hl-flydiff-mode
  (diff-hl-flydiff-mode +1)
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1))

(use-package blamer
  :bind (("C-c g" . blamer-mode))
  :config
  (setq blamer-idle-time 0.05)
  (setq blamer-author-formatter "%s ")
  (setq blamer-datetime-formatter "[%s]")
  (setq blamer-commit-formatter ": %s")
  (setq blamer-max-commit-message-length 100)
  (setq blamer-min-offset 70))

;; Searching/sorting enhancements & project management

(use-package ivy
  :preface
  ;; Buffer-local to each minibuffer buffer: a list of [window start point]
  ;; vectors, snapshotted when that minibuffer opens.
  (defvar-local ian/pre-ivy-all-windows nil)
  :hook (after-init . ivy-mode)
  :config
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              ;; Runs with the minibuffer as `current-buffer', so `setq-local'
              ;; pins the snapshot to this specific minibuffer. Excluded commands
              ;; store nil, which makes the exit hook a no-op -- a symmetric guard.
              (setq-local ian/pre-ivy-all-windows
                          (unless (memq this-command '(evil-ex ivy-done ivy-alt-done))
                            (let (snapshot)
                              (dolist (w (window-list))
                                (unless (or (window-minibuffer-p w)
                                            (with-current-buffer (window-buffer w)
                                              (region-active-p)))
                                  (push (vector w (window-start w) (window-point w)) snapshot)
                                  ;; Park point at window-start so a tall minibuffer
                                  ;; can't scroll the window to keep point on screen.
                                  (set-window-point w (window-start w))))
                              snapshot)))))
  (add-hook 'minibuffer-exit-hook
            (lambda ()
              ;; Also runs with the minibuffer as `current-buffer'. The
              ;; `unwind-protect' clears the snapshot even if a `set-window-*'
              ;; errors (e.g. a window whose buffer was swapped mid-session), so a
              ;; failed restore can never leave a stale snapshot behind.
              (unwind-protect
                  (dolist (entry ian/pre-ivy-all-windows)
                    (let ((w (aref entry 0)))
                      (when (window-live-p w)
                        (set-window-start w (aref entry 1) t)
                        (set-window-point w (aref entry 2)))))
                (setq-local ian/pre-ivy-all-windows nil))))
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-height 15)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg        . ivy--regex-plus)
          (project-find-file . ivy--regex-plus)
          (swiper            . ivy--regex-plus)
          (t                 . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (if (display-graphic-p)
      (progn
        (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "<return>") #'ivy-alt-done)
        (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done))
    (progn
      (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-next-line)
      (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
      (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)))
  (define-key ivy-minibuffer-map (kbd "<backtab>") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-c m") #'ivy-mark)
  (define-key ivy-minibuffer-map (kbd "C-c u") #'ivy-unmark))

(use-package counsel
  :preface
  (defun ian/counsel-rg-mx-prompt-dir-a (orig-fun &rest args)
    "Force `counsel-rg` to always prompt for a directory when called with M-x."
    (if (called-interactively-p 'interactive)
        (let ((init-dir (read-directory-name "Search in directory: ")))
          (apply orig-fun (car args) init-dir (cddr args)))
      (apply orig-fun args))
      (pulse-momentary-highlight-one-line (point) 'region)
      (recenter (/ (window-body-height) 4)))
  (defun ian/counsel-project-rg-or-rg ()
    "Search the current project.el root using rg.
If not in a project, prompt user to enter initial dir.

No explicit pulse/recenter here: this calls `counsel-rg', which is
already advised by `ian/counsel-rg-mx-prompt-dir-a' to pulse and recenter
after the jump."
    (interactive)
    (let ((curr-project (project-current nil)))
      (if curr-project
          (counsel-rg nil (project-root curr-project))
        (let ((init-dir (read-directory-name "Search in directory: ")))
          (counsel-rg nil init-dir)))))
  :hook (ivy-mode . counsel-mode)
  :config
  (advice-add 'counsel-rg :around #'ian/counsel-rg-mx-prompt-dir-a)
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (setq counsel-fzf-cmd "fd -H -c never \"%s\"")
  (global-set-key (kbd "C-S-p") #'counsel-M-x))

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package project
  :config
  (setq project-vc-extra-root-markers '(".project.el" ".projectile" ".project")))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config
  (setq wgrep-auto-save-buffer t))

(use-package prescient
  :config
  (setq prescient-filter-method '(literal regexp initialism fuzzy))
  (setq prescient-sort-length-enable nil)
  (prescient-persist-mode +1))

(use-package ivy-prescient
  :after (prescient ivy counsel)
  :config
  (setq ivy-prescient-sort-commands
        '(:not swiper
               counsel-grep
               counsel-rg
               ian/counsel-project-rg-or-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; Programming support and utilities

;; (add-to-list 'load-path (concat user-emacs-directory "lsp-bridge/"))
;; (use-package yasnippet)
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
;; (define-key acm-mode-map (kbd "<tab>") #'acm-select-next)
;; (define-key acm-mode-map (kbd "TAB") #'acm-select-next)
;; (define-key acm-mode-map (kbd "<backtab>") #'acm-select-prev)

(use-package lsp-mode
  :preface
  (defun ian/lsp-execute-code-action ()
    "Execute code action with pulse-line animation."
    (interactive)
    (ian/pulse-line)
    (call-interactively 'lsp-execute-code-action))
  (defun ian/lsp-deferred-js-mode ()
    "Enable lsp-deferred for js-mode but not json-mode."
    (unless (derived-mode-p 'json-mode)
      (lsp-deferred)))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((c-mode          ; clangd
           c++-mode        ; clangd
           c-or-c++-mode   ; clangd
           java-mode       ; eclipse-jdtls
           js-jsx-mode     ; ts-ls (tsserver wrapper)
           typescript-mode ; ts-ls (tsserver wrapper)
           python-mode     ; pyright
           web-mode        ; ts-ls/HTML/CSS
           rust-mode       ; rust-analyzer
           go-mode         ; gopls
           lua-mode        ; lua-language-server
           ) . lsp-deferred)
         (js-mode . ian/lsp-deferred-js-mode) ; ts-ls (tsserver wrapper)
        )
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :commands lsp
  :config
  (global-unset-key (kbd "<f2>"))
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
  ;; BEGIN emacs-lsp-booster
  ;; Ref: https://github.com/blahgeek/emacs-lsp-booster
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  ;; END emacs-lsp-booster
  (setq lsp-use-plists t)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-links nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-keep-workspace-alive nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.25)
  (setq lsp-auto-execute-action nil)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (with-eval-after-load 'lsp-clangd
    (setq lsp-clients-clangd-args '("--header-insertion=never" "-j=4" "-background-index")))
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-sideline-global ((t (:italic t))))
  (lsp-ui-peek-highlight  ((t (:foreground unspecified :background unspecified :inherit isearch))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'lsp-ui-mode-hook
              (lambda ()
                (evil-define-key '(motion normal) 'local (kbd "K")
                  (lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line))))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-enhanced-markdown nil)
  (setq lsp-ui-doc-delay 0.01)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-text-scale-level -1.0)
    (setq lsp-ui-doc-max-width 80)
    (setq lsp-ui-doc-max-height 25)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
  (setq lsp-ui-sideline-enable nil)
  ;; (setq lsp-ui-sideline-diagnostic-max-line-length 80)
  ;; (setq lsp-ui-sideline-diagnostic-max-lines 2)
  ;; (setq lsp-ui-sideline-delay 0.05)
  (setq lsp-ui-peek-always-show t))

(use-package lsp-java
  :after lsp)

(use-package java
  :ensure nil
  :after lsp-java
  :bind (:map java-mode-map ("C-c i" . lsp-java-add-import)))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

;; (use-package tree-sitter
;;   :after tree-sitter-langs
;;   :custom-face
;;   (tree-sitter-hl-face:property         ((t (:slant normal))))
;;   (tree-sitter-hl-face:method.call      ((t (:inherit font-lock-function-name-face))))
;;   (tree-sitter-hl-face:function.call    ((t (:inherit font-lock-function-name-face))))
;;   (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
;;   (tree-sitter-hl-face:operator         ((t (:inherit default))))
;;   (tree-sitter-hl-face:type.builtin     ((t (:inherit font-lock-type-face))))
;;   (tree-sitter-hl-face:number           ((t (:inherit highlight-numbers-number))))
;;   :config
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)

(use-package pyvenv
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (add-hook 'pyvenv-post-activate-hooks
            #'(lambda ()
                (call-interactively #'lsp-workspace-restart)))
  (pyvenv-mode +1))

(use-package company
  :hook ((prog-mode . company-mode)
         (inferior-emacs-lisp-mode . company-mode))
  :config
  (setq company-idle-delay 0.0)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 7)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (unless (display-graphic-p)
    (define-key company-active-map (kbd "C-h") #'backward-kill-word)
    (define-key company-active-map (kbd "C-w") #'backward-kill-word))
  (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (if (display-graphic-p)
      (define-key company-active-map (kbd "<tab>") 'company-select-next)
    (define-key company-active-map (kbd "TAB") 'company-select-next))
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-box
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable nil)
  (setq company-box-scrollbar nil)
  (setq company-box-frame-behavior 'default))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))
  :custom-face
  (flycheck-error   ((t (:inherit error :underline t))))
  (flycheck-warning ((t (:inherit warning :underline t))))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-display-errors-delay 0.1)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-checker-error-threshold 1000)
  (setq flycheck-indication-mode nil)
  (define-key flycheck-mode-map (kbd "<f8>") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "S-<f8>") #'flycheck-previous-error)
  (flycheck-define-checker proselint
    "A linter for prose. Install the executable with `pipx install proselint'."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (markdown-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :custom-face
  (markdown-code-face ((t (:background unspecified :inherit lsp-ui-doc-background))))
  :config
  (add-hook 'markdown-mode-hook
            #'(lambda () (ian/set-buffer-local-ellipsis ian/ellipsis))))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (setq typescript-indent-level ian/indent-width-compact))

(use-package dts-mode)

(use-package kdl-mode)

(use-package meson-mode
  :config
  ;; Unbind f1 key and let f1 key fallthrough as <help> in global-map.
  (define-key meson-mode-map (kbd "<f1>") nil))

(use-package rust-mode
  :config
  (add-to-list 'auto-mode-alist '("Cargo\\.lock\\'" . conf-toml-mode)))

;; (use-package go-mode
;;   :config
;;   (with-eval-after-load 'evil
;;     (evil-define-key '(motion normal) go-mode-map (kbd "gd") #'xref-find-definitions)
;;     ;; (evil-define-key '(motion normal) go-mode-map (kbd "gd") #'lsp-bridge-find-def)
;;     (evil-define-key '(motion normal) go-mode-map (kbd "K") #'(lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line)))))

(use-package lua-mode)

(use-package json-mode)

(use-package vimrc-mode)

(use-package git-modes)

(use-package cmake-font-lock)

(use-package yaml-mode
  :config
  (setq yaml-block-literal-electric-alist '()))

;; (use-package haskell-mode)

;; (use-package rjsx-mode
;;   :mode ("\\.jsx?\\'" . rjsx-mode)
;;   :custom-face
;;   (js2-error   ((t (:inherit default :underscore nil))))
;;   (js2-warning ((t (:inherit default :underscore nil))))
;;   :config
;;   (define-key rjsx-mode-map "<" nil)
;;   (define-key rjsx-mode-map (kbd "C-d") nil)
;;   (define-key rjsx-mode-map ">" nil))

;; (use-package web-mode
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'"   . web-mode)
;;          ("\\.jsx?\\'"  . web-mode)
;;          ("\\.tsx?\\'"  . web-mode)
;;          ("\\.json\\'"  . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset ian/indent-width-compact)
;;   (setq web-mode-code-indent-offset ian/indent-width-compact)
;;   (setq web-mode-css-indent-offset ian/indent-width-compact)
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; (use-package emmet-mode
;;   :hook ((html-mode
;;           css-mode
;;           js-mode
;;           js-jsx-mode
;;           typescript-mode
;;           web-mode
;;           ) . emmet-mode)
;;   :config
;;   (setq emmet-insert-flash-time 0.001) ; effectively disabling it
;;   (add-hook 'js-jsx-mode-hook #'(lambda ()
;;                                   (setq-local emmet-expand-jsx-className? t)))
;;   (add-hook 'web-mode-hook #'(lambda ()
;;                                (setq-local emmet-expand-jsx-className? t))))

(use-package cpp-auto-include ; Copyright (C) 2015 by Syohei Yoshida / Ben Deane
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include/ensure-includes-for-file)))

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format region if active, otherwise format whole buffer."
    (interactive)
    (let ((windowstart (window-start)))
      (if (derived-mode-p 'prolog-mode)
          (prolog-indent-buffer)
        (format-all-region-or-buffer))
      (set-window-start (selected-window) windowstart)))
  (defalias 'format-document #'ian/format-code)
  :config
  (global-set-key (kbd "<f6>") #'ian/format-code)
  (global-set-key (kbd "C-M-l") #'ian/format-code)
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  (add-hook 'python-mode-hook #'(lambda ()
                                  (setq-local format-all-formatters '(("Python" yapf)))))
  (add-hook 'sql-mode-hook #'(lambda ()
                               (setq-local format-all-formatters '(("SQL" pgformatter))))))

(use-package rainbow-mode
  :config
  (bind-key* (kbd "C-c r") #'rainbow-mode))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :custom-face
  (hl-todo                        ((t (:inverse-video nil :italic t :bold nil))))
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
  (global-hl-todo-mode +1))

;; (use-package processing-mode
;;   :after company
;;   :preface
;;   (defvar processing-company--keywords
;;     (with-eval-after-load 'processing-mode
;;       (cons 'processing-mode (append processing-functions
;;                                      processing-builtins
;;                                      processing-constants))))
;;   (defun processing-company--init ()
;;     (setq-local company-backends '((company-keywords
;;                                     :with
;;                                     company-yasnippet
;;                                     company-dabbrev-code)))
;;     (make-local-variable 'company-keywords-alist)
;;     (add-to-list 'company-keywords-alist processing-company--keywords))
;;   :config
;;   (add-hook 'processing-mode-hook 'processing-company--init)
;;   (setq processing-sketchbook-dir (format "%s/Projects/Processing/sketchbooks" (getenv "HOME")))
;;   (setq processing-location (format "%s/processing-3.5.4/processing-java" (getenv "HOME"))))

;; Dired enhancements

(use-package dired
  :ensure nil
  :hook (dired-mode . hl-line-mode)
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package wdired
  :ensure nil
  :config
  (setq wdired-allow-to-change-permissions 'advanced))

(use-package dirvish
  :preface
  ;; NOTE: workaround dirvish bug: With `dirvish-hide-cursor', a
  ;; pre-redisplay hook snaps point to the start of the filename after
  ;; every command, which sits before the search match.  Forward
  ;; `evil-search-next' then keeps re-finding the current line.
  ;;
  ;; Skip to end-of-line first so the search advances to the next
  ;; file's match.
  (defun ian/dirvish-evil-search-next ()
    "Like `evil-search-next', but reliable under `dirvish-hide-cursor'."
    (interactive)
    (end-of-line)
    (evil-search-next)
    (dired-move-to-filename))
  ;; NOTE: workaround dirvish bug: `find-file' (C-x C-f) bypasses
  ;; dirvish's open path, so the fullframe's dedicated dired window
  ;; forces the new file into a cramped side window, and the dirvish
  ;; session is never torn down (its stale `dv-winconf' then makes
  ;; later `dirvish' calls restore the old layout instead of opening
  ;; fresh).  Keep dirvish visible during the prompt, then once a file
  ;; is chosen, quit the session (via its still-live dired window) and
  ;; show the file fullscreen.
  (defun ian/dirvish-find-file ()
    "Create/visit a file, keeping Dirvish visible during the prompt, then quit
Dirvish and visit the file.  Honors `counsel-find-file' or any other `find-file' remaps."
    (interactive)
    (let ((dv (dirvish-curr)))
      (call-interactively (or (command-remapping #'find-file) #'find-file))
      (let ((buf (current-buffer))
            (win (and dv (get-buffer-window (cdr (dv-index dv))))))
        (when (window-live-p win)
          (with-selected-window win (dirvish-quit)))
        (switch-to-buffer buf))))
  :custom-face
  (dirvish-hl-line-inactive ((t (:inherit dirvish-hl-line))))
  :config
  ;; NOTE: workaround dirvish bug: Dirvish's file preview sets
  ;; `buffer-file-name' on a throwaway buffer parked at point
  ;; 1. Killing it runs `save-place-to-alist', which deletes the
  ;; file's saved position (saveplace forgets point-1 visits) -- so
  ;; previewing a file wipes its saveplace entry.
  ;;
  ;; TODO: Open one-liner PR for dirvish. dirvish maintainer currently
  ;; inactive, holding off PR.
  (advice-add 'dirvish--kill-buffer :around
              (lambda (orig buffer)
                (cl-letf (((symbol-function 'save-place-to-alist) #'ignore))
                  (funcall orig buffer))))
  (define-key dirvish-mode-map (kbd "C-x C-f") #'ian/dirvish-find-file)
  (with-eval-after-load 'evil
    (evil-define-key '(motion normal) dirvish-mode-map (kbd "o") #'dirvish-quicksort)
    (evil-define-key '(motion normal) dirvish-mode-map (kbd "q") #'dirvish-quit)
    (evil-define-key '(motion normal) dirvish-mode-map (kbd "n") #'ian/dirvish-evil-search-next)
    (evil-define-key '(motion normal) dirvish-mode-map (kbd "h") #'dired-up-directory)
    (evil-define-key '(motion normal) dirvish-mode-map (kbd "l") #'dired-find-file))
  (setq dirvish-default-layout '(1 0.167 0.5))
  (setq dirvish-input-throttle 0.02)
  (setq dirvish-reuse-session nil)
  (setq dirvish-hide-cursor t)
  (dirvish-override-dired-mode +1))

;; (use-package ranger
;;   :after dired
;;   :preface
;;   ;; Workaround for a ranger bug affecting dired mark commands (d, u, m)
;;   ;; on the topmost file in the ranger buffer.
;;   ;;
;;   ;; Root cause: ranger omits the directory header line that vanilla dired
;;   ;; normally places at the top of the buffer. As a result, the first file
;;   ;; entry starts at buffer position 1 with no preceding newline, causing
;;   ;; `line-beginning-position' to return 1 when point is on that line.
;;   ;; `dired-get-subdir' searches backward from point for a subdir header;
;;   ;; when it hits the buffer boundary without finding one, it erroneously
;;   ;; returns the current directory path (a truthy value) instead of nil.
;;   ;; This causes `dired-mark' to take the "mark all subdir files" branch,
;;   ;; flagging every file in the directory instead of just the one at point.
;;   ;;
;;   ;; Fix: wrap affected dired commands with a temporary override of
;;   ;; `dired-get-subdir' to always return nil via `cl-letf', forcing
;;   ;; `dired-mark' into the correct single-file branch.
;;   ;; Opened bug report at https://github.com/punassuming/ranger.el/issues/256
;;   (defun ianpan/dired-no-subdir (fn)
;;     (lambda (arg)
;;       (interactive "p")
;;       (cl-letf (((symbol-function 'dired-get-subdir) (lambda () nil)))
;;         (funcall fn arg))))
;;   :config
;;   (setq ranger-width-preview 0.5)
;;   (setq ranger-width-parents 0.167)
;;   (setq ranger-preview-delay 0.02)
;;   (setq ranger-show-hidden t)
;;   (setq ranger-cleanup-eagerly t)
;;   (define-key ranger-mode-map (kbd "H") #'evil-window-top)
;;   (define-key ranger-mode-map (kbd "L") #'evil-window-bottom)
;;   (define-key ranger-mode-map (kbd "?") #'evil-search-backward)
;;   (define-key ranger-mode-map (kbd "d") (ianpan/dired-no-subdir #'dired-flag-file-deletion))
;;   (define-key ranger-mode-map (kbd "u") (ianpan/dired-no-subdir #'dired-unmark))
;;   (define-key ranger-mode-map (kbd "U") #'dired-unmark-all-marks)
;;   (define-key ranger-mode-map (kbd "x") #'dired-do-flagged-delete)
;;   (define-key ranger-mode-map (kbd "i") #'dired-toggle-read-only)
;;   (define-key ranger-mode-map (kbd "m") (ianpan/dired-no-subdir #'dired-mark))
;;   (define-key ranger-mode-map (kbd "R") #'dired-do-rename)
;;   (define-key ranger-mode-map (kbd "C") #'dired-do-copy)
;;   (define-key ranger-mode-map (kbd "C-h") nil))

;; Misc

(use-package gcmh
  ;; :hook (emacs-startup-hook . gcmh-mode)
  :demand t
  :config
  (setq gcmh-low-cons-threshold (* 16 1024 1024))
  (gcmh-mode +1))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode +1))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :config
  (evil-terminal-cursor-changer-activate))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner
        (if (display-graphic-p) 'official "~/.emacs.d/logos/emacs-artistic-text-logo.txt"))
  (setq dashboard-banner-logo-title "( E M A C S )")
  (setq dashboard-init-info "")
  (setq dashboard-items nil)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon "")
  (setq dashboard-footer-messages '("😈 Happy hacking! "))
  (define-key dashboard-mode-map (kbd "<f5>") #'(lambda ()
                                                  (interactive)
                                                  (dashboard-refresh-buffer)
                                                  (message "Refreshing Dashboard...done"))))

(use-package avy
  :custom-face
  (avy-lead-face   ((t (:background "#ffdd63" :foreground "#000000" :bold t))))
  (avy-lead-face-0 ((t (:background "#ffdd63" :foreground "#000000" :bold t))))
  (avy-lead-face-1 ((t (:background "#ffdd63" :foreground "#000000" :bold t))))
  (avy-lead-face-2 ((t (:background "#ffdd63" :foreground "#000000" :bold t))))
  :config
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-background nil)
  (setq avy-all-windows nil))

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c .") #'emojify-insert-emoji)) ; override binding in any mode

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package helpful
  :after counsel
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  :config
  (define-key global-map [remap describe-key]     #'helpful-key)
  (define-key global-map [remap describe-command] #'helpful-command)
  (define-key help-map "F" #'helpful-function))

(use-package minions
  :preface
  (defvar-local ian--cached-project-data nil
    "Cached cons cell of '(last-checked-directory . project-name) for mode-line efficiency.")
  (defun ian/curr-project-name ()
    "Return the current project name, using a cache when appropriate."
    (if (and ian--cached-project-data
             (string= default-directory (car ian--cached-project-data)))
        (cdr ian--cached-project-data)
      (let* ((curr-project (project-current nil))
             (project-dir (and curr-project (project-root curr-project)))
             (project-name (if project-dir
                               (file-name-nondirectory (directory-file-name project-dir))
                             "-")))
        (setq-local ian--cached-project-data (cons default-directory project-name))
        project-name)))
  (defun ian/project-name-refresh-cache ()
    "Refresh cached project data of current file."
    (interactive)
    (setq-local ian--cached-project-data nil)
    (vc-file-clearprops default-directory)
    (when (buffer-file-name)
      (vc-file-clearprops (buffer-file-name)))
    (vc-refresh-state)
    (force-mode-line-update))
  :config
  (setq minions-mode-line-lighter "")
  (setq-default mode-line-buffer-identification '("%b [" (:eval (ian/curr-project-name)) "]"))
  (minions-mode +1))

(use-package neotree
  :preface
  (defun ian/neotree-project-toggle ()
    "Open NeoTree, using the project root if within a project."
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let* ((curr-project (project-current nil))
             (project-dir (if curr-project (project-root curr-project) nil))
             (curr-file-name (buffer-file-name)))
        (neotree-show)
        (if project-dir
            (progn
              (neotree-dir project-dir)
              (neotree-find curr-file-name))
          (neotree-find curr-file-name)
          (message "Not in a project... default to current directory.")))))
  :custom-face
  (neo-dir-link-face  ((t (:inherit variable-pitch))))
  (neo-header-face    ((t (:inherit variable-pitch))))
  (neo-banner-face    ((t (:inherit variable-pitch))))
  (neo-root-dir-face  ((t (:inherit variable-pitch))))
  (neo-file-link-face ((t (:inherit variable-pitch))))
  :config
  (add-hook 'neotree-mode-hook (lambda ()
                                 (hl-line-mode +1)
                                 (with-eval-after-load 'evil
                                   (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                                   (define-key evil-normal-state-local-map (kbd "H") 'evil-window-top))
                                 (setq-local line-spacing 1)))
  (setq neo-theme 'nerd)
  (setq neo-autorefresh t) ; In neotree.el: change delay to (run-with-idle-timer 0.1 ...)
  (setq neo-show-hidden-files t)
  (setq neo-window-width 50))

(use-package outline
  :ensure nil
  :config
  (add-hook 'outline-minor-mode-hook
            #'(lambda () (ian/set-buffer-local-ellipsis ian/ellipsis))))

(use-package outline-indent
  ;; The following keybindings work with evil-collection OOTB:
  ;; - zo: open fold
  ;; - zO: open folds recursively
  ;; - zc: close fold
  ;; - za: toggle fold
  ;; - zr: open all folds
  ;; - zm: close all folds
  :config
  (setq outline-indent-ellipsis ian/ellipsis)
  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode))

;; Org and LaTeX export
;;;; Ubuntu needs to have these installed:
;;;; 1. texlive-latex-extra
;;;; 2. texlive-fonts-extra
;;;; Arch needs to have this installed:
;;;; 1. texlive-most

(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda ()
                       (setq-local evil-auto-indent nil))))
  :config
  (require 'org-tempo)
  (setq org-ellipsis ian/ellipsis)
  (setq org-link-descriptive nil)
  (setq org-startup-folded nil)
  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (setq org-html-checkbox-type 'html))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package ox
  :ensure nil
  :config
  (setq org-export-with-smart-quotes t))

(use-package ox-latex
  :ensure nil
  :config
  (define-key org-mode-map (kbd "<f9>") #'org-latex-export-to-pdf)
  (setq org-latex-packages-alist '(("margin=1in" "geometry" nil)
                                   ;; ("bitstream-charter" "mathdesign" nil)
                                   ;; ("default, light" "roboto" nil)
                                   "\\hypersetup{colorlinks=true,linkcolor=blue,urlcolor=blue}"
                                   ("scale=0.9" "inconsolata" nil)))
  (setq org-latex-pdf-process
        '("/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

(provide 'init)
;;; init.el ends here
