;;; default-light-theme.el --- The default light theme that came with Emacs

;;; Commentary:
;;
;; This theme strives to have as few lines as possible, only tweaking
;; the colors that hinder readability/consistency in the default light
;; theme

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-light t)

;;; Code:
(deftheme default-light)

(custom-theme-set-faces
 'default-light
 '(default                          ((t (:background "white" :foreground "grey25"))))
 '(region                           ((t (:background "#ddeeff"))))
 '(font-lock-keyword-face           ((t (:foreground "royalblue3"))))
 '(font-lock-function-name-face     ((t (:foreground "royalblue3"))))
 '(minibuffer-prompt                ((t (:foreground "royalblue3"))))
 '(font-lock-comment-face           ((t (:foreground "grey60" :italic t))))
 '(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

 '(hl-line                          ((t (:background "grey95"))))
 '(highlight                        ((t (:background "grey90"))))
 '(ivy-current-match                ((t (:background "grey90"))))
 '(highlight-symbol-face            ((t (:background "#eef8ff"))))
 '(show-paren-match                 ((t (:background "grey85"))))
 '(isearch                          ((t (:foreground "white" :background "grey35"))))
 '(lazy-highlight                   ((t (:foreground "white" :background "grey65"))))

 '(company-tooltip                  ((t (:foreground "grey35" :background "grey97"))))
 '(company-tooltip-common           ((t (:foreground "blue" :bold t))))
 '(company-tooltip-annotation       ((t (:foreground "grey55"))))
 '(company-tooltip-selection        ((t (:background "grey90" :bold nil))))
 '(company-scrollbar-bg             ((t (:background "grey95"))))
 '(company-scrollbar-fg             ((t (:background "grey90"))))

 '(diff-hl-insert                   ((t (:background "ForestGreen" :foreground "ForestGreen"))))
 '(diff-hl-delete                   ((t (:background "#980000" :foreground "#980000"))))
 '(diff-hl-change                   ((t (:background "yellow2" :foreground "yellow2"))))

 '(mode-line                        ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 '(mode-line-inactive               ((t (:foreground "grey25" :background "grey90" :box (:color "grey75" :line-width 1)))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
