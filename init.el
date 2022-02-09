;;; init.el --- -*- lexical-binding: t -*-
;;  Author: Ian Y.E. Pan
;;; Commentary:
;;  This is my personal Emacs configuration
;;; Code:

;; GccEmacs (native-comp) stuff
;; (setq comp-deferred-compilation t)
;; (setq comp-async-report-warnings-errors nil)

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

;;; Settings without corresponding packages

(use-package emacs
  :preface
  (defvar ian/indent-width 2)
  (defun ian/maybe-set-default-browser ()
    "When in WSL Emacs, open links in default Windows 10 browser."
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
  (setq hscroll-margin 0)
  (setq load-prefer-newer t)
  (setq inhibit-compacting-font-caches t)
  (setq echo-keystrokes 0.02)
  (setq kill-buffer-query-functions nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'scroll-right 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (setq-default indent-tabs-mode nil)
  (setq initial-scratch-message "")
  (setq split-width-threshold 150)
  (setq max-specpdl-size 10000)
  (setq max-lisp-eval-depth 10000)
  (set-default 'truncate-lines t)
  (ian/maybe-set-default-browser)
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
  :config
  (remove-hook 'find-file-hook 'vc-refresh-state) ; makes open files faster
  (setq confirm-kill-processes nil)
  (setq create-lockfiles nil) ; don't create .# files (crashes 'npm start')
  (setq make-backup-files nil)
  (setq revert-without-query '(".*"))
  (global-set-key (kbd "<f5>") #'(lambda ()
                                   (interactive)
                                   (revert-buffer)
                                   (message "Refreshing buffer...done"))))

(use-package autorevert
  :ensure nil
  :config
  (setq auto-revert-interval 2)
  (setq auto-revert-check-vc-info t)
  (setq global-auto-revert-non-file-buffers t)
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
  (setq-default c-basic-offset ian/indent-width))

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

(use-package css-mode ; inerited by less-css-mode
  :ensure nil
  :config
  (setq css-indent-offset ian/indent-width))

(use-package mwheel
  :ensure nil
  :config
  (setq mouse-wheel-scroll-amount '(4 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0)
  :config
  (show-paren-mode +1))

(use-package frame
  :preface
  (defconst small-fonts-list '("Consolas" "Ubuntu Mono" "Fixedsys Excelsior"))
  (defconst tight-fonts-list '("Consolas" "Ubuntu Mono" "Fixedsys Excelsior"))
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
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 100 'normal)
    (when (member "Segoe UI" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI" :height 105 :weight 'normal)))
  (defun ian/set-small-fonts ()
    (interactive)
    (ian/set-default-fonts "Consolas" "YaHei Consolas Hybrid" 70 'normal)
    (when (member "Segoe UI" (font-family-list))
      (set-face-attribute 'variable-pitch nil :family "Segoe UI" :height 70 :weight 'normal)))
  :ensure nil
  :config
  (setq default-frame-alist (append (list '(width . 74) '(height . 35) '(internal-border-width . 2))))
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
  (setq ispell-program-name "/usr/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (put 'dired-find-alternate-file 'disabled nil))

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

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; (use-package vscode-dark-plus-theme
;;   :config
;;   (load-theme 'vscode-dark-plus t))

;; (add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
;; (load-theme 'default-dark t)

(use-package doom-themes
  :custom-face
  (cursor                         ((t (:background "#eeeeee"))))
  (region                         ((t (:extend nil))))
  (font-lock-comment-face         ((t (:italic t))))
  (sml/modified                   ((t (:foreground "white" :bold t))))
  (hl-todo                        ((t (:inverse-video nil :italic t :bold t))))
  (highlight-symbol-face          ((t (:background "#355266" :distant-foreground "#bbbbbb"))))
  (show-paren-match               ((t (:foreground "#eeeeee" :background "#444444" :bold t))))
  (highlight                      ((t (:foreground "#4db2ff" :background nil :underline t)))) ; link hover
  (link                           ((t (:foreground "#3794ff"))))
  (evil-ex-substitute-replacement ((t (:strike-through nil))))
  (vertical-border                ((t (:foreground "black" :background "black"))))
  (fringe                         ((t (:background nil))))
  :config
  (setq doom-themes-enable-bold nil)
  (setq doom-gruvbox-dark-variant "hard")
  (setq doom-solarized-dark-brighter-text t)
  (load-theme 'doom-one t))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Vi keybindings

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil
        evil-shift-width ian/indent-width
        evil-undo-system 'undo-tree)
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
    "Flash highlight the current line with reigon face"
    (interactive)
    (pulse-momentary-highlight-one-line (point) 'region))
  :config
  (setq-default cursor-type  '(hbar . 5))
  (setq evil-emacs-state-cursor '(hbar . 5))
  (define-key evil-normal-state-map (kbd "C-w C-o") #'(lambda () (interactive) (neotree-hide) (delete-other-windows)))
  (define-key evil-normal-state-map (kbd "C-o") #'(lambda () (interactive) (evil-jump-backward) (ian/pulse-line)))
  (global-set-key (kbd "C-x 1") #'(lambda () (interactive) (neotree-hide) (delete-other-windows)))
  (global-set-key (kbd "M-<up>") #'(lambda () (interactive) (scroll-down 2)))
  (global-set-key (kbd "M-<down>") #'(lambda () (interactive) (scroll-up 2)))
  (define-key evil-normal-state-map (kbd "M-<left>") #'(lambda () (interactive) (scroll-right 3)))
  (define-key evil-normal-state-map (kbd "M-<right>") #'(lambda () (interactive) (scroll-left 3)))
  (global-set-key (kbd "<mouse-2>") #'mouse-set-point)
  (define-key evil-normal-state-map (kbd "z <return>") #'evil-scroll-line-to-top)
  (define-key evil-insert-state-map (kbd "C-n") nil) ; avoid conflict with company tooltip selection
  (define-key evil-insert-state-map (kbd "C-p") nil) ; avoid conflict with company tooltip selection
  (define-key evil-normal-state-map (kbd "C-p") nil) ; avoid conflict with counsel-find-file
  (define-key evil-normal-state-map (kbd "C-S-c") #'evil-yank)
  (define-key evil-insert-state-map (kbd "C-S-v") #'ian/paste-with-ctrl-shift-v)
  (evil-define-key 'normal prog-mode-map (kbd "gd") #'xref-find-definitions)
  (evil-define-key 'normal prog-mode-map (kbd "<f12>") #'xref-find-definitions)
  (evil-define-key 'normal prog-mode-map (kbd "gD") #'xref-find-references)
  (evil-define-key 'normal prog-mode-map (kbd "S-<f12>") #'xref-find-references)
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
  (evil-commentary-mode +1)
  (add-hook 'c-mode-hook #'(lambda () (c-toggle-comment-style -1))))

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

(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode))
  :config
  (setq diff-hl-flydiff-delay 0.05))

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
  (define-key ivy-minibuffer-map (kbd "<C-return>") #'ivy-immediate-done))

(use-package counsel
  :hook (ivy-mode . counsel-mode)
  :config
  (setq counsel-rg-base-command "rg --vimgrep %s")
  (global-set-key (kbd "s-P")           #'counsel-M-x)
  (global-set-key (kbd "C-S-p")         #'counsel-M-x)
  (global-set-key (kbd "M-x")           #'counsel-M-x)
  (global-set-key (kbd "C-x <C-right>") #'counsel-find-file) ; autohotkey fix
  (global-set-key (kbd "C-x <right>")   #'counsel-find-file) ; autohotkey fix
  (global-set-key (kbd "C-s")           #'counsel-grep-or-swiper))

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
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-<f4>") #'projectile-kill-buffers)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-S-f") #'projectile-ripgrep))

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
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          java-mode       ; eclipse-jdtls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          python-mode     ; pyright
          web-mode        ; ts-ls/HTML/CSS
          haskell-mode
          ) . lsp-deferred)
  :custom-face
  (lsp-headerline-breadcrumb-symbols-face                ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-path-face                   ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-project-prefix-face         ((t (:inherit variable-pitch))))
  (lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:inherit variable-pitch))))
  :commands lsp
  :config
  (define-key lsp-mode-map (kbd "C-c l <tab>") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l TAB") #'lsp-execute-code-action)
  (global-unset-key (kbd "<f2>"))
  (define-key lsp-mode-map (kbd "<f2>") #'lsp-rename)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))

(use-package lsp-ui
  :preface
  (defun ian/lsp-ui-doc-show ()
    "Sometimes lsp-ui-doc-show needs more than one call to display correctly."
    (interactive)
    (lsp-ui-doc-hide)
    (lsp-ui-doc-show)
    (lsp-ui-doc-show))
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :config
  (define-key lsp-ui-mode-map (kbd "C-c l s") #'ian/lsp-ui-doc-show)
  (define-key lsp-ui-mode-map (kbd "C-c l h") #'lsp-ui-doc-hide)
  (custom-set-faces '(lsp-ui-sideline-global ((t (:italic t)))))
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-diagnostics t)
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

(use-package tree-sitter
  :custom-face
  (tree-sitter-hl-face:method.call      ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.call    ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:function.builtin ((t (:inherit font-lock-function-name-face))))
  (tree-sitter-hl-face:operator         ((t (:inherit default))))
  (tree-sitter-hl-face:type.builtin     ((t (:inherit font-lock-type-face))))
  (tree-sitter-hl-face:number           ((t (:inherit highlight-numbers-number))))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

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
  (setq company-idle-delay 0.0)
  (setq company-tooltip-minimum-width 60)
  (setq company-tooltip-maximum-width 60)
  (setq company-tooltip-limit 12)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "TAB") 'company-select-next)
  (define-key company-active-map (kbd "<tab>") 'company-select-next)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode)
         (markdown-mode . flycheck-mode)
         (org-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  (setq flycheck-flake8rc "~/.config/flake8")
  (setq flycheck-checker-error-threshold 1000)
  (define-key flycheck-mode-map (kbd "<f8>") #'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "S-<f8>") #'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "C-<f8>") #'flycheck-list-errors)
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

(use-package json-mode)

(use-package vimrc-mode)

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
  (global-set-key (kbd "C-M-l") #'ian/format-code) ; inspired by IntelliJ IDEA
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter)
  (add-hook 'python-mode-hook #'(lambda ()
                                  (setq-local format-all-formatters '(("Python" yapf)))))
  (add-hook 'sql-mode-hook #'(lambda ()
                               (setq-local format-all-formatters '(("SQL" pgformatter))))))

(use-package rainbow-mode
  :config
  (bind-key* (kbd "C-c r") #'rainbow-mode))

(use-package hl-todo
  :config
  (add-to-list 'hl-todo-keyword-faces '("DOING" . "#94bff3"))
  (add-to-list 'hl-todo-keyword-faces '("WHY" . "#7cb8bb"))
  (global-hl-todo-mode +1))

;;; Dired enhancements

(use-package dired-single
  :preface
  (defun ian/dired-single-init ()
    "Keybindings setup for dired-single."
    (define-key dired-mode-map (kbd "<return>") #'dired-single-buffer)
    (define-key dired-mode-map [remap dired-mouse-find-file-other-window] #'dired-single-buffer-mouse)
    (define-key dired-mode-map [remap dired-up-directory] #'dired-single-up-directory))
  :config
  (if (boundp 'dired-mode-map)
      (ian/dired-single-init)
    (add-hook 'dired-load-hook #'ian/dired-single-init)))

;; Terminal integration

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  :preface
  (defun ian/new-vterm-instance ()
    (interactive)
    (vterm t))
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs))
  (define-key vterm-mode-map (kbd "C-l") #'(lambda ()
                                             (interactive)
                                             (vterm-clear)
                                             (vterm-clear-scrollback))))

(use-package vterm-toggle
  :after (projectile vterm evil)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (global-set-key (kbd "C-`") #'vterm-toggle)
  (add-to-list 'display-buffer-alist
               '((lambda (bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.7))))

;; Misc

(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (setq sublimity-scroll-vertical-frame-delay 0.0001)
  (setq sublimity-scroll-weight 4)
  (setq sublimity-scroll-drift-length 1)
  (sublimity-mode +1))

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
  :hook ((org-mode      . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :config
  (setq-default olivetti-body-width fill-column))

(use-package writegood-mode)

(use-package speed-type)

(use-package emojify
  :config
  (when (member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (bind-key* (kbd "C-c .") #'emojify-insert-emoji)) ; override binding in any mode

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable nil)
  (setq company-box-scrollbar nil)
  (setq company-box-frame-behavior 'point))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 0.8))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (setq all-the-icons-dired-monochrome nil))

(use-package ivy-rich
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode +1))

;; (use-package pdf-tools
;;   :mode (("\\.pdf\\'" . pdf-view-mode))
;;   :bind ((:map pdf-view-mode-map ("C--" . pdf-view-shrink))
;;          (:map pdf-view-mode-map ("C-=" . pdf-view-enlarge))
;;          (:map pdf-view-mode-map ("C-0" . pdf-view-scale-reset)))
;;   :config
;;   (pdf-loader-install))

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :init
;;   (use-package epc      :defer t)
;;   (use-package ctable   :defer t)
;;   (use-package deferred :defer t)
;;   (use-package s        :defer t)
;;   :config
;;   (setq eaf-find-file-ext-blacklist '("png" "jpg" "gif"))
;;   ;; (setq eaf-enable-debug t)
;;   (add-hook 'eaf-mode-hook 'evil-emacs-state) ; better keybinding support
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-setq eaf-pdf-dark-mode "false")
;;   (eaf-bind-key quit-window      "q"       eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key zoom_out         "C--"     eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key zoom_in          "C-="     eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key zoom_reset       "C-0"     eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_up_page   "<next>"  eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down_page "<prior>" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_up_page   "d"       eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down_page "u"       eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_to_begin  "<home>"  eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_to_end    "<end>"   eaf-pdf-viewer-keybinding))

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (minions-mode +1))

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/modified-char "*")
  (setq sml/theme 'respectful)
  (sml/setup))

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
                                 (setq-local line-spacing nil)))
  (global-set-key (kbd "C-S-e") #'ian/neotree-project-toggle)
  (define-key neotree-mode-map (kbd "<f5>") #'(lambda ()
                                                (interactive)
                                                (neotree-refresh)
                                                (message "Refreshing NeoTree...done")))
  (setq neo-theme 'icons)
  (setq neo-show-hidden-files t)
  (setq neo-window-width 30))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode +1))

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
  (setq org-latex-packages-alist '(("margin=1in" "geometry" nil)
                                   ("bitstream-charter" "mathdesign" nil)
                                   ;; ("" "tinos" nil)
                                   ("" "inconsolata" nil)))
  (setq org-latex-pdf-process
        '("/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "/usr/bin/pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))


(provide 'init)
;;; init.el ends here
