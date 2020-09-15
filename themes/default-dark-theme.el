;;; default-dark-theme.el --- The default dark theme that came with Emacs

;;; Commentary:
;;
;; This theme strives to have as few lines as possible, only tweaking
;; the colors that hinder readability/consistency in the default dark
;; theme

;;; Usage:
;;
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'default-dark t)

;;; Code:
(deftheme default-dark)

(custom-theme-set-faces
 'default-dark
 '(default                          ((t (:background "grey10" :foreground "grey90"))))
 '(region                           ((t (:background "#204a72" :distant-foreground "grey75" :extend nil))))
 '(font-lock-comment-face           ((t (:foreground "grey45"))))
 '(line-number                      ((t (:foreground "grey45"))))

 '(company-tooltip                  ((t (:foreground "gray90" :background "black"))))
 '(company-tooltip-common           ((t (:foreground "LightSalmon" :bold t))))
 '(company-tooltip-annotation       ((t (:foreground "gray50" :italic t))))
 '(company-tooltip-selection        ((t (:background "gray20"))))
 '(company-scrollbar-bg             ((t (:background "gray15"))))
 '(company-scrollbar-fg             ((t (:background "gray30"))))

 ;; '(hl-line                          ((t (:background "grey20"))))
 ;; '(ivy-current-match                ((t (:background "LightSteelBlue" :foreground "black"))))

 '(diff-hl-insert                   ((t (:background "green4" :foreground "green4"))))
 '(diff-hl-delete                   ((t (:background "red3" :foreground "red3"))))
 '(diff-hl-change                   ((t (:background "yellow3" :foreground "yellow3"))))

 '(highlight-symbol-face            ((t (:background "grey25"))))
 '(hl-todo                          ((t (:inverse-video t))))

 '(term-color-blue                  ((t (:foreground "LightSteelBlue"))))
 '(term-color-green                 ((t (:foreground "PaleGreen"))))
 '(term-color-yellow                ((t (:foreground "LightGoldenrod"))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
