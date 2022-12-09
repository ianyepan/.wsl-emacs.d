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
  (defvar ian/indent-width 2)
  (defun ian/maybe-set-default-browser ()
    "When in WSL Emacs, open links in default Windows 11 browser."
    (cond
     ((eq system-type 'gnu/linux)
      (when (string-match "Linux.*microsoft.*Linux"
                          (shell-command-to-string "uname -a"))
        (setq browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
              browse-url-generic-args '("/c" "start" "")
              browse-url-browser-function 'browse-url-generic)))))
  (defun ian/edit-config ()
    "Open Emacs settings."
    (interactive)
    (find-file (concat user-emacs-directory "init.el")))
  (defun ian/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun ian/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
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
  (setq echo-keystrokes 0.02)
  (setq kill-buffer-query-functions nil)
  (setq delete-by-moving-to-trash t)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (global-set-key (kbd "C-x 2") #'ian/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'ian/split-and-follow-vertically)
  (global-set-key (kbd "C-s")   #'save-buffer)
  (setq-default indent-tabs-mode nil)
  (setq initial-scratch-message "")
  (setq split-width-threshold 150)
  (setq max-specpdl-size 10000)
  (setq max-lisp-eval-depth 10000)
  (set-default 'truncate-lines t)
  (ian/maybe-set-default-browser)
  (setq jit-lock-defer-time 0)
  (setq fast-but-imprecise-scrolling t)
  (xterm-mouse-mode +1)
  (bind-key* (kbd "C-,") #'ian/edit-config))

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
  (setq js-indent-level ian/indent-width)
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
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode  . "awk")
                          (c++-mode  . "bsd")
                          (c-mode    . "bsd")
                          (other     . "k&r")))
  (setq-default c-basic-offset ian/indent-width)
  (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode)))

(use-package cc-mode
  :ensure nil
  :config
  (define-key c++-mode-map ":" nil))

(use-package python
  :ensure nil
  :mode ("\\.gyp\\'" . python-mode)
  :config
  (setq python-indent-offset ian/indent-width)
  (setq python-shell-interpreter "python3"))

(use-package ruby-mode
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package css-mode ; inerited by less-css-mode
  :ensure nil
  :config
  (setq css-indent-offset ian/indent-width))

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
  (defconst tight-fonts-list '("Consolas" "Ubuntu Mono" "Inconsolata" "Monaco"))
  (defun ian/set-default-fonts (english-font chinese-font font-size font-weight)
    "Set the default Latin and CJK font families, as well as the line height."
    (interactive)
    (defvar is-using-undersized-font nil)
    (if (member english-font small-fonts-list)
        (progn
          (setq font-size (round (* font-size 1.1)))
          (setq is-using-undersized-font t))
      (setq is-using-undersized-font nil))
    (when (member english-font (font-family-list))
      (set-face-attribute 'default nil :family english-font :height font-size :weight font-weight))
    (when (member chinese-font (font-family-list))
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family chinese-font
                                             :size (*(/ font-size 10) 1.0)))))
    (setq-default line-spacing (if (member english-font tight-fonts-list) 3 1)))
  (defun ian/set-big-fonts ()
    (interactive)
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 150 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 140 :weight 'normal)))
  (defun ian/set-small-fonts ()
    (interactive)
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 70 'normal)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'fixed-pitch nil :family "Inconsolata" :height 1.0))
    (when (member "Segoe UI Variable Static Small" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI Variable Static Small" :height 0.9 :weight 'normal)))
  :ensure nil
  :config
  (setq default-frame-alist (append (list '(width . 74) '(height . 35) '(internal-border-width . 2))))
  (blink-cursor-mode -1)
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
  (setq ispell-program-name "/usr/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode +1))

(use-package recentf
  :ensure nil
  :config
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/early-init.el" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/init.el" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.emacs.d/workspace/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude (format "%s/\\.local/lib/python3.9/site-packages/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude "/usr/lib/.*")
  (recentf-mode +1))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-width 3))

(use-package xref
  :ensure nil
  :preface
  (defun ian/xref-reposition-in-new-buffer (func &rest args)
    "When xref opens a new buffer, reposition the cursor at 1/4 window height from top.
This follows the UX design of Visual Studio Code."
    (let ((original-buf (current-buffer)))
      (apply func args)
      (unless (eq (current-buffer) original-buf)
        (recenter-top-bottom (/ (window-body-height) 4)))))
  :config
  (advice-add 'xref-find-definitions :around #'ian/xref-reposition-in-new-buffer)
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
    (let ((zone-programs (list (intern pgm)))) (zone))))

;;; Third-party Packages

;; GUI enhancements


;; (use-package vscode-dark-plus-theme
;;   :config
;;   (load-theme 'vscode-dark-plus t))

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'eclipse t)

;; (use-package doom-themes
;;   :custom-face
;;   (region                         ((t (:extend nil))))
;;   (font-lock-comment-face         ((t (:italic t))))
;;   (highlight-symbol-face          ((t (:background "#355266" :distant-foreground "#bbbbbb"))))
;;   (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
;;   (link                           ((t (:foreground "#3794ff"))))
;;   (evil-ex-substitute-replacement ((t (:strike-through nil))))
;;   (vertical-border                ((t (:foreground "black" :background "black"))))
;;   (fringe                         ((t (:background nil))))
;;   :config
;;   (setq doom-themes-enable-bold nil)
;;   (setq doom-gruvbox-dark-variant "medium")
;;   (setq doom-solarized-dark-brighter-text t)
;;   (load-theme 'doom-Iosvkem t))

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

(use-package evil-leader
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width ian/indent-width
        evil-undo-system 'undo-fu)
  :config
  (global-evil-leader-mode +1)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "s" #'counsel-grep-or-swiper
    "w" #'save-buffer
    "f" #'counsel-projectile-find-file
    "F" #'projectile-ripgrep
    "o" #'other-window
    "r" #'ranger
    "<tab>" #'ian/lsp-execute-code-action
    "TAB" #'ian/lsp-execute-code-action
    "e" #'ian/neotree-project-toggle))

(use-package evil
  :after (undo-fu evil-leader)
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
  (setq evil-insert-state-cursor '(bar . 1))
  (define-key evil-motion-state-map (kbd "C-w C-o") #'(lambda () (interactive) (neotree-hide) (delete-other-windows)))
  (define-key evil-motion-state-map (kbd "C-o") #'(lambda () (interactive) (evil-jump-backward) (ian/pulse-line)))
  (define-key evil-motion-state-map (kbd "C-i") #'(lambda () (interactive) (evil-jump-forward) (ian/pulse-line)))
  (global-set-key (kbd "M-<up>") #'(lambda () (interactive) (scroll-down 2)))
  (global-set-key (kbd "M-<down>") #'(lambda () (interactive) (scroll-up 2)))
  (global-set-key (kbd "M-<left>") #'(lambda () (interactive) (scroll-right 2)))
  (global-set-key (kbd "M-<right>") #'(lambda () (interactive) (scroll-left 2)))
  (define-key evil-normal-state-map (kbd "z <return>") #'evil-scroll-line-to-top)
  (define-key evil-insert-state-map (kbd "C-n") nil) ; avoid conflict with company tooltip selection
  (define-key evil-insert-state-map (kbd "C-p") nil) ; avoid conflict with company tooltip selection
  (define-key evil-normal-state-map (kbd "C-S-c") #'evil-yank)
  (define-key evil-insert-state-map (kbd "C-S-v") #'ian/paste-with-ctrl-shift-v)
  (define-key evil-normal-state-map "u" #'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" #'undo-fu-only-redo)
  (evil-define-key 'motion prog-mode-map (kbd "gd") #'xref-find-definitions)
  (evil-define-key 'motion prog-mode-map (kbd "<f12>") #'xref-find-definitions)
  (evil-define-key 'motion prog-mode-map (kbd "gD") #'xref-find-references)
  (with-eval-after-load 'lsp-ui
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (define-key evil-motion-state-local-map (kbd "gD") #'lsp-ui-peek-find-references)))))
  (evil-ex-define-cmd "q" #'kill-current-buffer)
  (evil-ex-define-cmd "wq" #'(lambda () (interactive) (save-buffer) (kill-current-buffer)))
  (bind-key* (kbd "<f4>") #'(lambda ()
                              (interactive)
                              (if (one-window-p)
                                  (kill-current-buffer)
                                (kill-buffer-and-window)))))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
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

;; Git integration

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
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
  :if (display-graphic-p)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package diff-hl
  :unless (display-graphic-p)
  :custom-face
  (diff-hl-insert ((t (:background nil :slant normal))))
  (diff-hl-delete ((t (:background nil :slant normal))))
  (diff-hl-change ((t (:background nil :slant normal))))
  :config
  (setq diff-hl-margin-symbols-alist '((insert  . "┃") ; U+02503 (box drawings heavy vertical)
                                       (delete  . "▶")
                                       (change  . "┃")
                                       (unknown . "┃")
                                       (ignored . "┃")))
  (diff-hl-margin-mode)
  (setq diff-hl-flydiff-delay 0.05)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

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
  :hook (after-init . ivy-mode)
  :config
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-height 15)
  (setq ivy-display-style nil)
  (setq ivy-re-builders-alist
        '((counsel-rg            . ivy--regex-plus)
          (counsel-projectile-rg . ivy--regex-plus)
          (swiper                . ivy--regex-plus)
          (t                     . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "TAB") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "<backtab>") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-c m") #'ivy-mark)
  (define-key ivy-minibuffer-map (kbd "C-c u") #'ivy-unmark))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (setq counsel-fzf-cmd "fd -H -c never \"%s\"")
  (global-set-key (kbd "C-S-p") #'counsel-M-x))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package swiper
  :after ivy
  :config
  (setq swiper-action-recenter t)
  (setq swiper-goto-start-of-match t))

(use-package projectile
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map))

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
               counsel-projectile-rg
               ivy-switch-buffer
               counsel-switch-buffer))
  (setq ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-mode +1))

(use-package company-prescient
  :after (prescient company)
  :config
  (company-prescient-mode +1))

;; Programming support and utilities

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          ;; java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          haskell-mode    ; haskell-language-server
          lua-mode        ; lua-language-server
          rust-mode       ; rust-analyzer
          ruby-mode       ; solargraph
          go-mode         ; gopls
          ) . lsp-deferred)
  :preface
  (defun ian/lsp-execute-code-action ()
    "Execute code action with pulse-line animation."
    (interactive)
    (ian/pulse-line)
    (call-interactively 'lsp-execute-code-action))
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :commands lsp
  :config
  (add-hook 'java-mode-hook #'(lambda () (when (eq major-mode 'java-mode) (lsp-deferred))))
  ;; (define-key lsp-mode-map (kbd "C-c l <tab>") #'ian/lsp-execute-code-action)
  (global-unset-key (kbd "<f2>"))
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-use-plists t)
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
  (with-eval-after-load 'lsp-clangd
    (add-to-list 'lsp-clients-clangd-args "--header-insertion=never"))
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background "#262829"))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'buffer-list-update-hook
              #'(lambda ()
                  (when (bound-and-true-p lsp-ui-mode)
                    (define-key evil-motion-state-local-map (kbd "K")
                      #'(lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line)))))))
  (custom-set-faces '(lsp-ui-sideline-global ((t (:italic t)))))
  (setq lsp-ui-doc-enable nil)
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-position 'at-point))
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-sideline-delay 0.05))

(use-package lsp-java
  :after lsp)

(use-package java
  :ensure nil
  :after lsp-java
  :bind (:map java-mode-map ("C-c i" . lsp-java-add-import)))

(use-package lsp-haskell)

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

;; (use-package tree-sitter
;;   :custom-face
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
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 7)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
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
    "A linter for prose. Install the executable with `pip3 install proselint'."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message) line-end))
    :modes (markdown-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint))

(use-package markdown-mode
  :hook (markdown-mode . auto-fill-mode)
  :config
  (set-face-attribute 'markdown-code-face nil :inherit 'org-block))

(use-package typescript-mode
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (setq typescript-indent-level ian/indent-width))

(use-package rust-mode)

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package go-mode
  :config
  (evil-define-key 'motion go-mode-map (kbd "gd") #'xref-find-definitions)
  (evil-define-key 'normal go-mode-map (kbd "gd") #'xref-find-definitions)
  (evil-define-key 'motion go-mode-map (kbd "K") #'(lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line)))
  (evil-define-key 'normal go-mode-map (kbd "K") #'(lambda () (interactive) (lsp-ui-doc-glance) (ian/pulse-line))))

(use-package lua-mode)

(use-package json-mode)

(use-package vimrc-mode)

(use-package cmake-font-lock)

(use-package yaml-mode)

(use-package haskell-mode)

(use-package rjsx-mode
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :custom-face
  (js2-error   ((t (:inherit default :underscore nil))))
  (js2-warning ((t (:inherit default :underscore nil))))
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))

;; (use-package web-mode
;;   :mode (("\\.html?\\'" . web-mode)
;;          ("\\.css\\'"   . web-mode)
;;          ("\\.jsx?\\'"  . web-mode)
;;          ("\\.tsx?\\'"  . web-mode)
;;          ("\\.json\\'"  . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset ian/indent-width)
;;   (setq web-mode-code-indent-offset ian/indent-width)
;;   (setq web-mode-css-indent-offset ian/indent-width)
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package emmet-mode
  :hook ((html-mode
          css-mode
          js-mode
          js-jsx-mode
          typescript-mode
          web-mode
          ) . emmet-mode)
  :config
  (setq emmet-insert-flash-time 0.001) ; effectively disabling it
  (add-hook 'js-jsx-mode-hook #'(lambda ()
                                  (setq-local emmet-expand-jsx-className? t)))
  (add-hook 'web-mode-hook #'(lambda ()
                               (setq-local emmet-expand-jsx-className? t))))

(use-package cpp-auto-include ; Copyright (C) 2015 by Syohei Yoshida / Ben Deane
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include/ensure-includes-for-file)))

(use-package format-all
  :preface
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (let ((windowstart (window-start)))
      (if (derived-mode-p 'prolog-mode)
          (prolog-indent-buffer)
        (format-all-buffer))
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

(use-package hl-todo
  :custom-face
  (hl-todo                        ((t (:inverse-video nil :italic t :bold nil))))
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
  (global-hl-todo-mode +1))

(use-package processing-mode
  :after company
  :preface
  (defvar processing-company--keywords
    (with-eval-after-load 'processing-mode
      (cons 'processing-mode (append processing-functions
                                     processing-builtins
                                     processing-constants))))
  (defun processing-company--init ()
    (setq-local company-backends '((company-keywords
                                    :with
                                    company-yasnippet
                                    company-dabbrev-code)))
    (make-local-variable 'company-keywords-alist)
    (add-to-list 'company-keywords-alist processing-company--keywords))
  :config
  (add-hook 'processing-mode-hook 'processing-company--init)
  (setq processing-sketchbook-dir (format "%s/Projects/Processing/sketchbooks" (getenv "HOME")))
  (setq processing-location (format "%s/processing-3.5.4/processing-java" (getenv "HOME"))))

;; Dired enhancements

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package ranger
  :after dired
  :config
  (setq ranger-width-preview 0.5)
  (setq ranger-width-parents 0.167)
  (setq ranger-preview-delay 0.02)
  (setq ranger-show-hidden t)
  (define-key ranger-mode-map (kbd "d") #'dired-flag-file-deletion)
  (define-key ranger-mode-map (kbd "u") #'dired-unmark)
  (define-key ranger-mode-map (kbd "U") #'dired-unmark-all-marks)
  (define-key ranger-mode-map (kbd "x") #'dired-do-flagged-delete)
  (define-key ranger-mode-map (kbd "i") #'dired-toggle-read-only)
  (define-key ranger-mode-map (kbd "m") #'dired-mark)
  (define-key ranger-mode-map (kbd "R") #'dired-do-rename)
  (define-key ranger-mode-map (kbd "C") #'dired-do-copy)
  (define-key ranger-mode-map (kbd "C-h") nil))

;; Terminal integration

;; (use-package vterm
;;   :hook (vterm-mode . (lambda ()
;;                         (setq-local global-hl-line-mode nil)
;;                         (setq-local line-spacing nil)))
;;   :preface
;;   (defun ian/new-vterm-instance ()
;;     (interactive)
;;     (vterm t))
;;   :config
;;   (setq vterm-disable-bold t)
;;   (setq vterm-timer-delay 0.01)
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'vterm-mode 'emacs))
;;   (define-key vterm-mode-map (kbd "C-l") #'(lambda ()
;;                                              (interactive)
;;                                              (vterm-clear)
;;                                              (vterm-clear-scrollback))))

;; (use-package vterm-toggle
;;   :after (projectile vterm evil)
;;   :config
;;   (setq vterm-toggle-fullscreen-p nil)
;;   (setq vterm-toggle-scope 'project)
;;   (add-to-list 'display-buffer-alist
;;                '((lambda(bufname _) (with-current-buffer bufname
;;                                       (or (equal major-mode 'vterm-mode)
;;                                           (string-prefix-p vterm-buffer-name bufname))))
;;                  (display-buffer-reuse-window display-buffer-at-bottom)
;;                  (direction . bottom)
;;                  (dedicated . t)
;;                  (reusable-frames . visible)
;;                  (window-height . 0.5)))
;;   (global-set-key (kbd "C-`") #'vterm-toggle))

;; Misc

(use-package xclip
  :unless (display-graphic-p)
  :config
  (xclip-mode +1))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :config
  (setq etcc-use-color t)
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (evil-terminal-cursor-changer-activate)))

(use-package dashboard
  :if (display-graphic-p)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-banner-logo-title "( E M A C S )")
  (setq dashboard-init-info "")
  (setq dashboard-items nil)
  (setq dashboard-set-footer t)
  (setq dashboard-footer-icon "")
  (setq dashboard-footer-messages '("😈 Happy hacking!   "
                                    "👽 Happy hacking!   "
                                    "👻 Happy hacking!   "
                                    "💀 Happy hacking!   "))
  (define-key dashboard-mode-map (kbd "<f5>") #'(lambda ()
                                                  (interactive)
                                                  (dashboard-refresh-buffer)
                                                  (message "Refreshing Dashboard...done"))))

(use-package olivetti
  :bind ("C-c w" . olivetti-mode)
  :config
  (setq-default olivetti-body-width fill-column))

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

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode +1))

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (setq-default mode-line-buffer-identification '("%b // " (:eval (projectile-project-name))))
  (minions-mode +1))

(use-package neotree
  :after projectile
  :preface
  (defun ian/neotree-project-toggle ()
    "Open NeoTree using the projectile root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (when (neo-global--window-exists-p)
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find projectile project root."))))
  :custom-face
  (neo-dir-link-face  ((t (:inherit variable-pitch))))
  (neo-header-face    ((t (:inherit variable-pitch))))
  (neo-banner-face    ((t (:inherit variable-pitch))))
  (neo-root-dir-face  ((t (:inherit variable-pitch))))
  (neo-file-link-face ((t (:inherit variable-pitch))))
  :config
  (add-hook 'neotree-mode-hook (lambda ()
                                 (hl-line-mode +1)
                                 (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                                 (setq-local line-spacing 1)))
  (setq neo-theme 'icons)
  (setq neo-autorefresh t) ; neotree.el: change delay to (run-with-idle-timer 0.1 ...)
  (setq neo-show-hidden-files t)
  (setq neo-window-width 30))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode +1))

;; (use-package pdf-tools
;;   :mode (("\\.pdf\\'" . pdf-view-mode))
;;   :config
;;   (pdf-loader-install))

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
                       (setq-local evil-auto-indent nil)
                       (setq-local olivetti-body-width (+ fill-column 5)))))
  :config
  (require 'org-tempo)
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
