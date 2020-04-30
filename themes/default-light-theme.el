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
 '(region                       ((t (:background "#ddeeff"))))
 '(font-lock-keyword-face       ((t (:foreground "purple" :bold t))))
 '(font-lock-variable-name-face ((t (:foreground "black"))))
 '(line-number                  ((t (:background "grey90" :foreground "black"))))
 '(line-number-current-line     ((t (:background "grey90" :foreground "black"))))
 '(git-gutter-fr:added          ((t (:background "#00cc00" :foreground "#00cc00" :weight normal))))
 '(git-gutter-fr:deleted        ((t (:background "#cc0000" :foreground "#cc0000" :weight normal))))
 '(git-gutter-fr:modified       ((t (:background "#5588ff" :foreground "#5588ff" :weight normal))))
 )

(provide-theme 'default-light)
;;; default-light-theme.el ends here
