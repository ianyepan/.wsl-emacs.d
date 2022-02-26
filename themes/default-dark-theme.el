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
 '(default                          ((t (:background "black" :foreground "white"))))
 '(fringe                           ((t (:inherit default))))
 '(cursor                           ((t (:background "white"))))
 '(region                           ((t (:background "#b0c4de" :foreground "black" :extend nil))))
 '(hl-line                          ((t (:background "grey25"))))
 '(line-number                      ((t (:foreground "grey45"))))

 '(company-tooltip                  ((t (:foreground "black" :background "gray80"))))
 '(company-tooltip-common           ((t (:foreground "blue" :bold t))))
 '(company-tooltip-annotation       ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-selection        ((t (:background "gray60"))))
 '(company-scrollbar-bg             ((t (:background "gray75"))))
 '(company-scrollbar-fg             ((t (:background "gray80"))))

 '(git-gutter:added                 ((t (:foreground "#4a7f00"))))
 '(git-gutter:deleted               ((t (:foreground "#a41511"))))
 '(git-gutter:modified              ((t (:foreground "#207fa1"))))
 '(git-gutter-fr:added              ((t (:foreground "#4a7f00"))))
 '(git-gutter-fr:deleted            ((t (:foreground "#a41511"))))
 '(git-gutter-fr:modified           ((t (:foreground "#207fa1"))))

 '(highlight-symbol-face            ((t (:background "#355266" :distant-foreground "white"))))

 '(term-color-blue                  ((t (:foreground "#0782f0"))))

 '(mode-line                        ((t (:foreground "black" :background "grey75" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive               ((t (:foreground "black" :background "grey55" :box (:line-width -1 :style released-button)))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
