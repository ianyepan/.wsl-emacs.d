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
 '(region                     ((t (:background "#113386"))))

 '(company-tooltip            ((t (:foreground "black" :background "gray75"))))
 '(company-tooltip-common     ((t (:foreground "blue3" :bold t))))
 '(company-tooltip-annotation ((t (:italic t))))
 '(company-tooltip-selection  ((t (:background "gray65" :bold t))))
 '(company-scrollbar-bg       ((t (:background "gray65"))))
 '(company-scrollbar-fg       ((t (:background "gray50"))))

 '(ivy-current-match          ((t (:inherit highlight))))
 '(vterm-color-blue           ((t (:foreground "dodgerblue"))))
 '(git-gutter-fr:added        ((t (:background "green4" :foreground "green4" :weight normal))))
 '(git-gutter-fr:deleted      ((t (:background "red4" :foreground "red4" :weight normal))))
 '(git-gutter-fr:modified     ((t (:background "yellow4" :foreground "yellow4" :weight normal))))
 '(ediff-even-diff-Ancestor   ((t (:background "#222222"))))
 '(ediff-even-diff-A          ((t (:background "#222222"))))
 '(ediff-even-diff-B          ((t (:background "#222222"))))
 '(ediff-even-diff-C          ((t (:background "#222222"))))
 '(ediff-odd-diff-Ancestor    ((t (:background "#222222"))))
 '(ediff-odd-diff-A           ((t (:background "#222222"))))
 '(ediff-odd-diff-B           ((t (:background "#222222"))))
 '(ediff-odd-diff-C           ((t (:background "#222222"))))
 '(hl-todo                    ((t (:inverse-video t))))
 )

(provide-theme 'default-dark)
;;; default-dark-theme.el ends here
