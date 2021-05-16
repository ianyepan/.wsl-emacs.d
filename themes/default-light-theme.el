;;; default-light-theme.el --- A pleasant light theme

;;; Commentary:
;;
;; This light theme is inspired by Eclipse IDE and Visual Studio
;; Code's default Light+ theme

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-light t)

;;; Code:
(deftheme default-light)

(custom-theme-set-faces
 'default-light
 '(default                                ((t (:background "#f5f5f5"))))
 '(region                                 ((t (:background "#d7d4f0" :extend nil))))
 '(secondary-selection                    ((t (:inherit region))))
 '(fringe                                 ((t (:background nil))))
 '(font-lock-keyword-face                 ((t (:foreground "#7f0055" :bold t))))
 '(font-lock-builtin-face                 ((t (:foreground "#7f0055"))))
 '(font-lock-type-face                    ((t (:foreground "#7f0055"))))
 '(font-lock-constant-face                ((t (:foreground "blue"))))
 '(font-lock-string-face                  ((t (:foreground "MediumBlue"))))
 '(font-lock-variable-name-face           ((t (:foreground "black"))))
 '(minibuffer-prompt                      ((t (:inherit default))))
 '(font-lock-comment-face                 ((t (:foreground "#3F7F5F"))))
 '(font-lock-doc-face                     ((t (:inherit font-lock-comment-face))))
 '(error                                  ((t (:foreground "#c80000" :bold t))))
 '(warning                                ((t (:inherit error))))

 '(hl-line                                ((t (:background "#e8f0ff"))))
 '(ivy-current-match                      ((t (:background "#c8e0fe"))))
 '(highlight-symbol-face                  ((t (:background "#d5e5f5"))))
 '(show-paren-match                       ((t (:background "grey85"))))
 '(highlight                              ((t (:foreground "#0065c2" :underline t)))) ; link hover
 '(link                                   ((t (:foreground "#0065c2"))))

 '(company-tooltip                        ((t (:foreground "grey20" :background "grey94"))))
 '(company-tooltip-common                 ((t (:foreground "#0065c2" :bold t))))
 '(company-tooltip-annotation             ((t (:foreground "grey55"))))
 '(company-tooltip-selection              ((t (:background "#d8e5f5" :bold nil))))
 '(company-scrollbar-bg                   ((t (:background "grey95"))))
 '(company-scrollbar-fg                   ((t (:background "grey90"))))
 '(company-template-field                 ((t (:inherit region))))

 '(neo-dir-link-face                      ((t (:foreground "#555555"))))
 '(neo-header-face                        ((t (:foreground "#555555"))))
 '(neo-banner-face                        ((t (:foreground "#555555"))))
 '(neo-root-dir-face                      ((t (:foreground "#555555"))))
 '(neo-file-link-face                     ((t (:foreground "#555555"))))

 '(diff-hl-insert                         ((t (:background "ForestGreen" :foreground "ForestGreen"))))
 '(diff-hl-delete                         ((t (:background "#a80000" :foreground "#a80000"))))
 '(diff-hl-change                         ((t (:background "#0065c2" :foreground "#0065c2"))))

 '(mode-line                              ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 '(mode-line-inactive                     ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 '(sml/modified                           ((t (:foreground "black" :bold t))))
 '(term-color-white                       ((t (:foreground "#999999"))))

 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "#999999" :underline (:style wave :color "DarkOrange")))))

 '(markdown-header-face                   ((t (:foreground "grey25" :background "grey90" :overline t :height 1.3))))
 '(org-level-1                            ((t (:foreground "grey25" :background "grey90" :overline t :height 1.3))))
 '(org-level-2                            ((t (:foreground "grey25" :background "grey90" :overline t :height 1.3))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
