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
 '(default                          ((t (:background "white" :foreground "grey35"))))
 '(region                           ((t (:background "#ddeeff"))))
 '(font-lock-keyword-face           ((t (:foreground "blue"))))
 '(font-lock-comment-face           ((t (:foreground "grey60" :italic t))))
 '(font-lock-doc-face               ((t (:inherit font-lock-comment-face))))

 '(hl-line                          ((t (:background "grey95"))))
 '(ivy-current-match                ((t (:background "grey90"))))
 '(highlight-symbol-face            ((t (:background "#eef8ff"))))
 '(show-paren-match                 ((t (:background "grey85"))))
 '(isearch                          ((t (:foreground "white" :background "DarkCyan"))))

 '(company-tooltip                  ((t (:foreground "#34495e" :background "grey97"))))
 '(company-tooltip-common           ((t (:foreground "blue3" :bold t))))
 '(company-tooltip-annotation       ((t (:foreground "ForestGreen"))))
 '(company-tooltip-selection        ((t (:background "grey90" :bold nil))))
 '(company-scrollbar-bg             ((t (:background "grey95"))))
 '(company-scrollbar-fg             ((t (:background "grey90"))))

 '(diff-hl-insert                   ((t (:background "green3" :foreground "green3"))))
 '(diff-hl-delete                   ((t (:background "orangered" :foreground "orangered"))))
 '(diff-hl-change                   ((t (:background "royalblue" :foreground "royalblue"))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
