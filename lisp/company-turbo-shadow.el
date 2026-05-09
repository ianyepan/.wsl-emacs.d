;;; company-turbo-shadow.el --- Borland-style drop shadow for company-mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;  Example usage -
;;    (add-to-list 'load-path "~/.emacs.d/lisp/")
;;    (require 'company-turbo-shadow)
;;    (company-turbo-shadow-mode +1)
;;; Code:

(defface company-turbo-shadow-face
  '((t :foreground "#010101" :background "#010101"))
  "Face for the Borland-style drop shadow.")

(defvar company-turbo-right-shadow--char
  (propertize "░░" 'face 'company-turbo-shadow-face)
  "Shadow glyph appended to the right of each tooltip line.")

(defvar company-turbo-shadow--bottom-overlay nil)

(defun company-turbo-shadow--patch-lines (lines)
  "Append right shadow character(s) to each line in LINES except the first two."
  (cond
   ((null lines) nil)
   ((null (cdr lines)) lines) ; Only 1 line? Return as-is
   (t
    (append
     ;; Keep the first two lines untouched
     (list (car lines) (cadr lines))
     ;; Append right shadow to the rest
     (mapcar (lambda (line) (concat line company-turbo-right-shadow--char))
             (cddr lines))))))

(defun company-turbo-shadow--remove-bottom (&rest _)
  "Clean up the bottom shadow overlay."
  (when (overlayp company-turbo-shadow--bottom-overlay)
    (delete-overlay company-turbo-shadow--bottom-overlay)
    (setq company-turbo-shadow--bottom-overlay nil)))

(defun company-turbo-shadow--line-suffix (str from-col)
  "Return the portion of STR starting at visual column FROM-COL, which includes on the right of the bottom shadow line."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (move-to-column from-col)
    (buffer-substring (point) (point-max))))

(defun company-turbo-shadow--add-bottom (&rest _)
  "Add bottom shadow to company tooltip."
  (company-turbo-shadow--remove-bottom)
  (when (and (overlayp company-pseudo-tooltip-overlay)
             (overlay-start company-pseudo-tooltip-overlay))
    (let* ((ov             company-pseudo-tooltip-overlay)
           (col            (overlay-get ov 'company-column))
           (shadow-width   (1+ (or (overlay-get ov 'company-width) 0)))
           (show-tooltip-below-p  (> (overlay-get ov 'company-height) 0))
           (ov-height      (abs (or (overlay-get ov 'company-height) 0)))
           (cand-count     (length company-candidates))
           (visible-height (min cand-count ov-height))
           (raw-disp       (overlay-get ov 'company-display))
           (internal-margin 3)
           (target-col     (1+ (max 0 (- col internal-margin))))
           (shadow-str     (propertize (make-string shadow-width ?░)
                                       'face 'company-turbo-shadow-face))
           (full-height-p  (= visible-height ov-height)))
      (when (and raw-disp show-tooltip-below-p (> visible-height 0))
        (if full-height-p
            ;; When tooltip is full height — use a separate overlay on it
            (save-excursion
              (goto-char (overlay-end ov))
              (unless (eobp)
                (let* ((line-end   (line-end-position))
                       (shadow-beg (progn (move-to-column target-col) (point)))
                       (shadow-end (min (progn (move-to-column (+ target-col shadow-width)) (point))
                                        line-end))
                       (content    (buffer-substring-no-properties shadow-beg shadow-end))
                       (actual-w   (string-width content))
                       (padded     (propertize
                                    (concat content
                                            (make-string (max 0 (- shadow-width actual-w)) ?░))
                                    'face 'company-turbo-shadow-face))
                       (bov        (if (= shadow-beg shadow-end)
                                       (make-overlay line-end line-end)
                                     (make-overlay shadow-beg shadow-end))))
                  (if (= shadow-beg shadow-end)
                      (overlay-put bov 'after-string
                                   (concat (propertize " " 'display `(space :align-to ,target-col))
                                           padded))
                    (overlay-put bov 'display padded))
                  (overlay-put bov 'priority 999)
                  (overlay-put bov 'window (selected-window))
                  (setq company-turbo-shadow--bottom-overlay bov))))
          ;; Shrunk: blank padding lines cover real buffer lines,
          ;; trade one for shadow and ovewrite parts of it
          (let* ((lines      (split-string raw-disp "\n"))
                 (before     (seq-take lines visible-height))
                 (after      (seq-drop lines visible-height))
                 (shadow-line (or (car after) ""))
                 ;; Overwrite only target-col..target-col+shadow-width in the existing line
                 (left       (truncate-string-to-width shadow-line target-col 0 ?\s))
                 (right      (company-turbo-shadow--line-suffix
                              shadow-line (+ target-col shadow-width)))
                 (spliced    (concat left shadow-str right))
                 (after-tail (if (> (length after) 1) (cdr after) '("")))
                 (new-disp   (string-join
                              (append before (list spliced) after-tail)
                              "\n")))
            (overlay-put ov 'before-string new-disp)))))))

;;;###autoload
(define-minor-mode company-turbo-shadow-mode
  "Minor mode to enable Borland-style shadows for Company."
  :global t
  (if company-turbo-shadow-mode
      (progn
        (advice-add 'company--create-lines         :filter-return #'company-turbo-shadow--patch-lines)
        (advice-add 'company-pseudo-tooltip-unhide :after         #'company-turbo-shadow--add-bottom)
        (advice-add 'company-pseudo-tooltip-hide   :before        #'company-turbo-shadow--remove-bottom))
    (advice-remove 'company--create-lines         #'company-turbo-shadow--patch-lines)
    (advice-remove 'company-pseudo-tooltip-unhide #'company-turbo-shadow--add-bottom)
    (advice-remove 'company-pseudo-tooltip-hide   #'company-turbo-shadow--remove-bottom)
    (company-turbo-shadow--remove-bottom)))

(provide 'company-turbo-shadow)

;;; company-turbo-shadow.el ends here
