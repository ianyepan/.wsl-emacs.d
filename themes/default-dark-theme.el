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
 '(default                          ((t (:background "#111111" :foreground "#eeeeee"))))
 '(region                           ((t (:background "#114f86"))))

 '(company-tooltip                  ((t (:foreground "black" :background "gray75"))))
 '(company-tooltip-common           ((t (:foreground "blue" :bold t))))
 '(company-tooltip-selection        ((t (:foreground "black" :background "gray60" :inverse-video nil :bold t))))
 '(company-tooltip-common-selection ((t (:foreground "blue" :background "gray60" :inverse-video nil :bold t))))
 '(company-scrollbar-bg             ((t (:background "gray75"))))
 '(company-scrollbar-fg             ((t (:background "gray60"))))

 '(diff-hl-insert                   ((t (:background "#4a7f00" :foreground "#4a7f00"))))
 '(diff-hl-delete                   ((t (:background "#a41511":foreground "#a41511"))))
 '(diff-hl-change                   ((t (:background "#207fa1" :foreground "#207fa1"))))

 '(ediff-even-diff-Ancestor         ((t (:background "#222222"))))
 '(ediff-even-diff-A                ((t (:background "#222222"))))
 '(ediff-even-diff-B                ((t (:background "#222222"))))
 '(ediff-even-diff-C                ((t (:background "#222222"))))
 '(ediff-odd-diff-Ancestor          ((t (:background "#222222"))))
 '(ediff-odd-diff-A                 ((t (:background "#222222"))))
 '(ediff-odd-diff-B                 ((t (:background "#222222"))))
 '(ediff-odd-diff-C                 ((t (:background "#222222"))))

 '(hl-todo                          ((t (:inverse-video t))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
