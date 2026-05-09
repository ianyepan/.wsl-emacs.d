;;; company-turbo-shadow.el --- Borland-style drop shadow for company-mode  -*- lexical-binding: t; -*-
;;; Commentary:

(defface company-turbo-shadow-face
  '((t :foreground "#010101" :background "#010101"))
  "Face for the Borland-style drop shadow.")

(defvar company-turbo-right-shadow--char
  (propertize "░░" 'face 'company-turbo-shadow-face)
  "Shadow glyph appended to the right of each tooltip line.")

(defvar company-turbo-shadow--bottom-overlay nil)

(defun company-turbo-shadow--patch-lines (lines)
  "Append a shadow character to each line, skipping the first two rows."
  (cond
   ((null lines) nil)
   ((null (cdr lines)) lines) ; Only 1 line? Return as-is
   (t
    (let ((shadow (propertize company-turbo-right-shadow--char 
                              'face 'company-turbo-shadow-face)))
      (append 
       ;; Keep the first two lines untouched
       (list (car lines) (cadr lines))
       ;; Append shadow to the rest
       (mapcar (lambda (line) (concat line shadow))
               (cddr lines)))))))

(defun company-turbo-shadow--remove-bottom (&rest _)
  "Clean up the bottom shadow overlay."
  (when (overlayp company-turbo-shadow--bottom-overlay)
    (delete-overlay company-turbo-shadow--bottom-overlay)
    (setq company-turbo-shadow--bottom-overlay nil)))

(defun company-turbo-shadow--add-bottom (&rest _)
  "Add a bottom shadow that compensates for Company's internal left margin."
  (company-turbo-shadow--remove-bottom)
  (when (and (overlayp company-pseudo-tooltip-overlay)
             (overlay-start company-pseudo-tooltip-overlay))
    (let* ((ov     company-pseudo-tooltip-overlay)
           (beg    (overlay-start ov))
           (col    (overlay-get ov 'company-column))
           ;; Subtract 1 to align right
           (width  (1+ (or (overlay-get ov 'company-width) 0)))
           (height (abs (or (overlay-get ov 'company-height) 0)))
           (raw-height  (or (overlay-get ov 'company-height) 0))
           ;; 3 is the magic internal margin company offsets to align candidates
           (internal-margin 3))

      (when (and (integerp col) (integerp width) (> width 0) (> raw-height 0))
        (save-excursion
          (goto-char beg)
          (beginning-of-line)
          (let ((inhibit-field-text-motion t))
            (vertical-motion height))

          (unless (eobp)
            (let* ((line-end (line-end-position))
                   ;; COMPENSATE: Subtract the margin, but never go past the left edge (0)
                   ;; This mirrors exactly how Company 'squishes' the menu at BOL.
                   ;; +1 to match ncurses drop shadow effect.
                   (target-col (1+ (max 0 (- col internal-margin))))
                   (shadow-face 'company-turbo-shadow-face))

              (beginning-of-line)
              (let* ((shadow-beg (progn (move-to-column target-col) (point)))
                     (shadow-end (progn (move-to-column (+ target-col width)) (point)))
                     (shadow-end (min shadow-end line-end))

                     (content    (buffer-substring-no-properties shadow-beg shadow-end))
                     (actual-w   (string-width content))
                     (padded     (propertize
                                  (concat content
                                          (make-string (max 0 (- width actual-w)) (string-to-char "░")))
                                  'face shadow-face))
                     (bov (if (= shadow-beg shadow-end)
                              (make-overlay line-end line-end)
                           (make-overlay shadow-beg shadow-end))))

                ;; For short lines, we use :align-to with the window-relative target-col
                (if (= shadow-beg shadow-end)
                    (overlay-put bov 'after-string
                                 (concat (propertize "░" 'display `(space :align-to ,target-col))
                                         padded))
                  (overlay-put bov 'display padded))

                (overlay-put bov 'window (selected-window))
                (overlay-put bov 'priority 999)
                (setq company-turbo-shadow--bottom-overlay bov)))))))))

(defun company-turbo-shadow--frontend (command)
  "Frontend that keeps the bottom shadow in sync with the tooltip."
  (pcase command
    ('show         (company-turbo-shadow--add-bottom))
    ('update       (company-turbo-shadow--add-bottom))
    ('post-command (company-turbo-shadow--add-bottom))
    ('hide         (company-turbo-shadow--remove-bottom))))

;;;###autoload
(define-minor-mode company-turbo-shadow-mode
  "Minor mode to enable Borland-style shadows for Company."
  :global t
  (if company-turbo-shadow-mode
      (progn
        (advice-add 'company--create-lines  :filter-return #'company-turbo-shadow--patch-lines)
        (advice-add 'company-pseudo-tooltip-show :after  #'company-turbo-shadow--add-bottom)
        (advice-add 'company-pseudo-tooltip-hide :before #'company-turbo-shadow--remove-bottom)
        (add-to-list 'company-frontends #'company-turbo-shadow--frontend t))
    (advice-remove 'company--create-lines  #'company-turbo-shadow--patch-lines)
    (advice-remove 'company-pseudo-tooltip-show #'company-turbo-shadow--add-bottom)
    (advice-remove 'company-pseudo-tooltip-hide #'company-turbo-shadow--remove-bottom)
    (setq company-frontends (delq #'company-turbo-shadow--frontend company-frontends))
    (company-turbo-shadow--remove-bottom)))

(provide 'company-turbo-shadow)

;;; company-turbo-shadow.el ends here
