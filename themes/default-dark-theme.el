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
 '(font-lock-comment-face           ((t (:foreground "grey55"))))
 '(line-number                      ((t (:foreground "grey45"))))

 '(company-tooltip                  ((t (:foreground "gray90" :background "black"))))
 '(company-tooltip-common           ((t (:foreground "LightSalmon" :bold t))))
 '(company-tooltip-annotation       ((t (:foreground "gray50" :italic t))))
 '(company-tooltip-selection        ((t (:background "gray20"))))
 '(company-scrollbar-bg             ((t (:background "gray15"))))
 '(company-scrollbar-fg             ((t (:background "gray30"))))

 ;; '(hl-line                          ((t (:background "grey20"))))
 ;; '(ivy-current-match                ((t (:background "LightSteelBlue" :foreground "black"))))

 '(diff-hl-insert                   ((t (:background "#36a546" :foreground "#36a546"))))
 '(diff-hl-delete                   ((t (:background "#ac0909" :foreground "#ac0909"))))
 '(diff-hl-change                   ((t (:background "#f1eb7f" :foreground "#f1eb7f"))))

 '(highlight-symbol-face            ((t (:background "#354a32"))))
 '(hl-todo                          ((t (:inverse-video t))))

 '(term-color-blue                  ((t (:foreground "LightSteelBlue"))))
 '(term-color-green                 ((t (:foreground "PaleGreen"))))
 '(term-color-yellow                ((t (:foreground "LightGoldenrod"))))

 '(mode-line                        ((t (:foreground "black" :background "grey75" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive               ((t (:foreground "black" :background "grey55" :box (:line-width -1 :style released-button)))))
 '(sml/global                       ((t (:foreground "black" :weight normal))))
 '(sml/filename                     ((t (:foreground "black" :weight bold))))
 '(sml/prefix                       ((t (:foreground "black" :weight normal))))
 '(sml/read-only                    ((t (:foreground "black" :weight normal))))
 '(sml/modes                        ((t (:foreground "black" :weight normal))))
 '(sml/modified                     ((t (:foreground "black" :weight normal))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
