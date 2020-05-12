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
 '(default                          ((t (:background "white" :foreground "#666666"))))
 '(region                           ((t (:background "#ddeeff"))))
 '(font-lock-keyword-face           ((t (:inherit default :bold t))))
 '(font-lock-type-face              ((t (:foreground "ForestGreen" :bold nil))))
 '(font-lock-variable-name-face     ((t (:inherit default))))
 '(font-lock-function-name-face     ((t (:foreground "royalblue3"))))
 '(font-lock-comment-face           ((t (:foreground "grey65"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

 '(show-paren-match                         ((t (:background "grey90" :underline t :bold t))))

 '(hl-line                          ((t (:background "grey95"))))
 '(ivy-current-match                ((t (:inherit hl-line))))

 '(company-tooltip                  ((t (:foreground "#34495e" :background "grey95"))))
 '(company-tooltip-common           ((t (:foreground "blue3" :bold nil))))
 '(company-tooltip-annotation       ((t (:italic t))))
 '(company-tooltip-selection        ((t (:background "grey90" :bold nil))))
 '(company-scrollbar-bg             ((t (:background "grey90"))))
 '(company-scrollbar-fg             ((t (:background "grey85"))))

 '(diff-hl-insert                   ((t (:background "#4a7f00" :foreground "#4a7f00"))))
 '(diff-hl-delete                   ((t (:background "#a41511":foreground "#a41511"))))
 '(diff-hl-change                   ((t (:background "#207fa1" :foreground "#207fa1"))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
