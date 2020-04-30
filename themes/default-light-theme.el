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
 '(region                     ((t (:background "#ddeeff"))))
 '(company-tooltip            ((t (:inherit default :background "gray90"))))
 '(company-tooltip-common     ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-selection  ((t (:inherit highlight))))
 '(company-scrollbar-bg       ((t (:background "gray90"))))
 '(company-scrollbar-fg       ((t (:background "gray50"))))
 '(ivy-current-match          ((t (:inherit highlight))))
 '(git-gutter-fr:added        ((t (:background "#00cc00" :foreground "#00cc00" :weight normal))))
 '(git-gutter-fr:deleted      ((t (:background "#cc0000" :foreground "#cc0000" :weight normal))))
 '(git-gutter-fr:modified     ((t (:background "#5588ff" :foreground "#5588ff" :weight normal))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
