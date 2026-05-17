;;; default-light-theme.el

;;; Commentary:
;;
;; The builtin light theme, with minor tweaks

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-light t)

;;; Code:
(deftheme default-light)

(custom-theme-set-faces
 'default-light
 '(default                                ((t (:background "#f7f7f7" :foreground "#222222"))))
 '(region                                 ((t (:background "#0078d4" :foreground "#ffffff" :extend nil))))
 '(secondary-selection                    ((t (:inherit region))))
 '(fringe                                 ((t (:background nil))))
 '(font-lock-keyword-face                 ((t (:foreground "#7f0055" :bold nil))))
 '(minibuffer-prompt                      ((t (:inherit default))))
 '(font-lock-doc-face                     ((t (:inherit font-lock-comment-face))))
 '(error                                  ((t (:foreground "#c80000" :bold t))))
 '(warning                                ((t (:inherit error))))

 '(hl-line                                ((t (:background "#c8e0fe"))))
 '(ivy-current-match                      ((t (:background "#c8e0fe"))))
 '(highlight-symbol-face                  ((t (:background "#d5e5f5"))))
 '(show-paren-match                       ((t (:background "grey85"))))
 '(highlight                              ((t (:foreground "#0065c2" :underline t)))) ; link hover
 '(link                                   ((t (:foreground "#0065c2"))))
 '(mode-line                              ((t (:foreground "#010101" :background "grey75" :box (:line-width -1 :style released-button)))))
 '(mode-line-active                       ((t (:foreground "#010101" :background "grey75" :box (:line-width -1 :style released-button)))))

 '(company-tooltip                        ((t (:foreground "grey20" :background "grey94"))))
 '(company-tooltip-common                 ((t (:foreground "#0065c2" :bold nil))))
 '(company-tooltip-annotation             ((t (:foreground "grey55"))))
 '(company-tooltip-selection              ((t (:background "#c8e0fe" :bold nil))))
 '(company-scrollbar-bg                   ((t (:background "grey95"))))
 '(company-scrollbar-fg                   ((t (:background "grey90"))))
 '(company-template-field                 ((t (:inherit region))))

 '(git-gutter:added                       ((t (:foreground "#81b88b" :weight normal))))
 '(git-gutter:deleted                     ((t (:foreground "#ca4b51" :weight normal))))
 '(git-gutter:modified                    ((t (:foreground "#66afe0" :weight normal))))
 '(git-gutter-fr:added                    ((t (:foreground "#81b88b" :weight normal))))
 '(git-gutter-fr:deleted                  ((t (:foreground "#ca4b51" :weight normal))))
 '(git-gutter-fr:modified                 ((t (:foreground "#66afe0" :weight normal))))

 '(diff-hl-insert                         ((t (:background "#81b88b" :foreground "#81b88b"))))
 '(diff-hl-delete                         ((t (:background "#ca4b51" :foreground "#ca4b51"))))
 '(diff-hl-change                         ((t (:background "#66afe0" :foreground "#66afe0"))))

 '(neo-dir-link-face                      ((t (:foreground "#111111"))))
 '(neo-header-face                        ((t (:foreground "#444444"))))
 '(neo-banner-face                        ((t (:foreground "#444444"))))
 '(neo-root-dir-face                      ((t (:foreground "#444444"))))
 '(neo-file-link-face                     ((t (:foreground "#444444"))))

 '(lsp-lsp-flycheck-info-unnecessary-face ((t (:foreground "#999999" :underline (:style wave :color "DarkOrange")))))

 '(org-level-1                            ((t (:foreground "grey25" :background "grey90" :overline t :height 1.3))))
 '(org-level-2                            ((t (:foreground "grey25" :background "grey90" :overline t :height 1.3))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
