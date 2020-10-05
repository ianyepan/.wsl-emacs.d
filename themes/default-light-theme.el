;;; default-light-theme.el --- A pleasant light theme

;;; Commentary:
;;
;; This light theme is inspired by Eclipse IDE and Visual Studio
;; Code's default light themes

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-light t)

;;; Code:
(deftheme default-light)

(custom-theme-set-faces
 'default-light
 '(region                           ((t (:background "#bbddff" :extend nil))))
 '(secondary-selection              ((t (:inherit region))))
 '(fringe                           ((t (:background nil))))
 '(font-lock-keyword-face           ((t (:foreground "#7f0055" :bold t))))
 '(font-lock-builtin-face           ((t (:foreground "#7f0055"))))
 '(font-lock-type-face              ((t (:foreground "#7f0055"))))
 '(font-lock-constant-face          ((t (:foreground "blue"))))
 '(font-lock-string-face            ((t (:foreground "MediumBlue"))))
 '(font-lock-variable-name-face     ((t (:foreground "black"))))
 '(minibuffer-prompt                ((t (:inherit default))))
 '(font-lock-comment-face           ((t (:foreground "#3F7F5F"))))
 '(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

 '(hl-line                          ((t (:background "#e8f0ff"))))
 '(highlight                        ((t (:background "#e8f0ff"))))
 '(ivy-current-match                ((t (:background "#e8f0ff"))))
 '(highlight-symbol-face            ((t (:background "#ddeeff"))))
 '(show-paren-match                 ((t (:background "grey85"))))

 '(company-tooltip                  ((t (:foreground "grey20" :background "grey97"))))
 '(company-tooltip-common           ((t (:foreground "SkyBlue3" :bold t))))
 '(company-tooltip-annotation       ((t (:foreground "grey55"))))
 '(company-tooltip-selection        ((t (:background "grey90" :bold nil))))
 '(company-scrollbar-bg             ((t (:background "grey95"))))
 '(company-scrollbar-fg             ((t (:background "grey90"))))
 '(company-template-field           ((t (:inherit region))))

 '(diff-hl-insert                   ((t (:background "ForestGreen" :foreground "ForestGreen"))))
 '(diff-hl-delete                   ((t (:background "#a80000" :foreground "#a80000"))))
 '(diff-hl-change                   ((t (:background "#bbddff" :foreground "#bbddff"))))

 '(mode-line                        ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 '(mode-line-inactive               ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 '(sml/modified                     ((t (:foreground "black" :bold t))))
 '(term-color-white                 ((t (:foreground "#999999"))))

 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "#999999" :underline (:style wave :color "DarkOrange")))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
