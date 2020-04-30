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
 '(default                    ((t (:background "#060606" :foreground "#eeeeee"))))
 '(region                     ((t (:background "#113366"))))
 '(company-tooltip            ((t (:inherit default :background "gray20"))))
 '(company-tooltip-common     ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-annotation ((t (:inherit font-lock-builtin-face))))
 '(company-tooltip-selection  ((t (:inherit highlight))))
 '(company-scrollbar-bg       ((t (:background "gray20"))))
 '(company-scrollbar-fg       ((t (:background "gray50"))))
 '(ivy-current-match          ((t (:inherit highlight))))
 '(vterm-color-blue           ((t (:foreground "dodgerblue"))))
 '(git-gutter-fr:added        ((t (:background "green" :foreground "green" :weight normal))))
 '(git-gutter-fr:deleted      ((t (:background "red" :foreground "red" :weight normal))))
 '(git-gutter-fr:modified     ((t (:background "skyblue" :foreground "skyblue" :weight normal))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
