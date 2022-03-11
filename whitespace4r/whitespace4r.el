;;; whitespace4r.el --- Minor mode to show whitespace for selected region -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2022/01/12
;; Version: 0.1.10
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/whitespace4r
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor mode to show whitespace for selected region.

;; See README.md for more information.

;;; Code:

(require 'whitespace)

(declare-function evil-visual-range "evil-states")

(defvar-local whitespace4r--region-mark nil
  "Used to save the last selected region.")

(defvar-local whitespace4r-font-lock-keywords nil
  "Used to save the value ‘whitespacer-mode’ adds to ‘font-lock-keywords’.")

(defface whitespace4r-space
  '((t (:inherit whitespace-space)))
  "Face used to visualize SPACE."
  :group 'whitespace4r)

(defface whitespace4r-hspace
  '((t (:inherit whitespace-hspace)))
  "Face used to visualize HSPACE."
  :group 'whitespace4r)

(defface whitespace4r-tab
  '((t (:inherit whitespace-tab)))
  "Face used to visualize TAB."
  :group 'whitespace4r)

(defface whitespace4r-trailing
  '((t (:inherit whitespace-trailing)))
  "Face used to visualize trailing blanks."
  :group 'whitespace4r)

(defcustom whitespace4r-display-mappings
  '(
    (space-mark      . [?·])
    (hard-space-mark . [?¤])
    (tab-mark        . [?\s ?»]))
  "Specify an alist of mappings for displaying characters.

Each element has the following form:

   (KIND [CHAR1 ...])"
  :type '(repeat
          (cons :tag "Character Mapping"
                (choice :tag "Char Kind"
                        (const :tag "Tab" tab-mark)
                        (const :tag "Space" space-mark)
                        (const :tag "HardSpace" hard-space-mark))
                (repeat :inline t :tag "Vector List"
                                  (vector :tag ""
                                          (repeat :inline t
                                                  :tag "Vector Characters"
                                                  (character :tag "Char"))))))
  :group 'whitespace4r)

(defcustom whitespace4r-style '(tabs spaces hspaces trailing)
  "Specify which kind of blank is visualized.

It's a list contianing some or all of the following values:

   tabs			TABs are visualized via faces.
   spaces		SPACEs are visualized via faces.
   hspaces		HARD SPACEs are visualized via faces.
   trailing		trailing blanks are visualized via faces."
  :type 'list
  :group 'whitespace4r)

(defun whitespace4r-font-lock-keywords ()
  "Return font lock keywords."
  `(
    ,@(when (memq 'spaces whitespace4r-style)
        ;; Show SPACEs.
        `(("\\(\s\\)"
           (1 (put-text-property
               (match-beginning 1)
               (match-end 1)
               'display (propertize
                         ,(char-to-string
                           (aref (cdr (assq 'space-mark
                                            whitespace4r-display-mappings)) 0))
                         'face 'whitespace4r-space))))))
    ,@(when (memq 'hspaces whitespace4r-style)
        ;; Show HARD SPACEs.
        `(("\\(\u00A0\\)"
           (1 (put-text-property
               (match-beginning 1)
               (match-end 1)
               'display
               (propertize ,(char-to-string
                             (aref (cdr (assq 'hard-space-mark
                                              whitespace4r-display-mappings)) 0))
                           'face 'whitespace4r-hspace))))))
    ,@(when (memq 'tabs whitespace4r-style)
        ;; Show TABs.
        `(("\\(\t\\)"
           (1 (let* ((pole ,(aref (cdr (assq 'tab-mark
                                             whitespace4r-display-mappings)) 0))
                     (arrow ,(char-to-string
                              (aref (cdr (assq 'tab-mark
                                               whitespace4r-display-mappings)) 1)))
                     (s (concat (make-string
                                 (- (current-column)
                                    (save-excursion
                                      (goto-char (match-beginning 1))
                                      (current-column))
                                    1)
                                 pole)
                                arrow)))
                (put-text-property
                 (match-beginning 1)
                 (match-end 1)
                 'display (propertize s 'face 'whitespace4r-tab)))))))
    ,@(when (memq 'trailing whitespace4r-style)
        ;; Show trailing blanks.
        `(("\\([\t\s\u00A0]+\\)$"
           (1 (save-excursion
                (while (< (match-beginning 1) (point))
                  (put-text-property
                   (1- (point))
                   (point)
                   'display
                   (propertize (or (get-text-property (1- (point)) 'display)
                                   (buffer-substring (1- (point)) (point)))
                               'face 'whitespace4r-trailing))
                  (backward-char)))))))))

(defun whitespace4r-diff-regions (r1 r2)
  "Return a list of regions that contained in R1 but not R2."
  (remove nil (if (and r1 r2)
                  (list (if (> (car r2) (car r1))
                            (cons (car r1) (car r2)))
                        (if (< (cdr r2) (cdr r1))
                            (cons (cdr r2) (cdr r1))))
                (list r1))))

(defun whitespace4r--mark-region (region)
  "Mark the REGION."
  (when whitespace4r--region-mark
    (delete-overlay whitespace4r--region-mark))
  (when region
    (setq whitespace4r--region-mark
          (make-overlay (car region) (cdr region) nil nil t))))

(defun whitespace4r--marked-region ()
  "Return the marked retion."
  (when whitespace4r--region-mark
    (let ((beg (overlay-start whitespace4r--region-mark))
          (end (overlay-end whitespace4r--region-mark)))
      (when (and beg end)
        (cons beg end)))))

(defun whitespace4r--hide (regions &optional immediately-p)
  "Hide whitespace in REGIONS.

If IMMEDIATELY-P is non-nil, remove teh text properties immediately, otherwise
remove it in a timer."
  (font-lock-remove-keywords nil whitespace4r-font-lock-keywords)
  (let ((function
          (lambda (buffer beg end)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (let ((min-end (min end (point-max))))
                  (when (< beg min-end)
                    (let ((undo-list-backup buffer-undo-list)
                          (modified (buffer-modified-p)))
                      (unwind-protect
                          (remove-text-properties beg min-end '(display nil))
                        (set-buffer-modified-p modified)
                        (setq buffer-undo-list undo-list-backup))))))))))
    (dolist (region regions)
      (let ((beg (car region))
            (end (cdr region)))
        (when (< beg end)
          (font-lock-flush beg end)
          (font-lock-ensure beg end)
          (if immediately-p
              (funcall function (current-buffer) beg end)
            (run-with-timer 0 nil function (current-buffer) beg end)))))))

(defun whitespace4r--show (regions)
  "Show whitespace in REGIONS."
  (font-lock-add-keywords nil whitespace4r-font-lock-keywords t)
  (dolist (region regions)
    (when (< (car region) (cdr region))
      (font-lock-flush (car region) (cdr region))
      (font-lock-ensure (car region) (cdr region)))))

(defun whitespace4r--update ()
  "Refresh screen when selected region changed."
  (if (region-active-p)
      (let ((font-lock-extend-region-functions
             (remove 'font-lock-extend-region-wholelines
                     font-lock-extend-region-functions))
            (c-before-context-fontification-functions
             ;; FIXME: Avoid `c-font-lock-fontify-region' to expand the region
             ;; from (beg end) to (bol eol), that causes highlighting in the
             ;; wrong place.  Not sure what side effects this fix will have.
             (if (derived-mode-p 'c-mode)
                 (remove 'c-context-expand-fl-region
                         c-before-context-fontification-functions)))
            (last-region (whitespace4r--marked-region))
            (r (if (eq (bound-and-true-p evil-state) 'visual)
                   (cons (nth 0 (evil-visual-range))
                         (nth 1 (evil-visual-range)))
                 (cons (region-beginning)
                       (region-end)))))
        (whitespace4r--show (whitespace4r-diff-regions r last-region))
        (whitespace4r--hide (whitespace4r-diff-regions last-region r))
        (whitespace4r--mark-region r))
    (whitespace4r--mark-region nil)))

(defun whitespace4r--activate-mark-cb ()
  "Run after the mark becomes active."
  (setq whitespace4r-font-lock-keywords (whitespace4r-font-lock-keywords))
  (add-hook 'post-command-hook #'whitespace4r--update nil t))

(defun whitespace4r--deactivate-mark-cb ()
  "Run after the mark becomes deactive."
  (remove-hook 'post-command-hook #'whitespace4r--update t)
  (let ((last-region (whitespace4r--marked-region)))
    (when last-region
      (whitespace4r--hide (list last-region))))
  (whitespace4r--mark-region nil))

(defun whitespacer4r--advice-indent-region (beg end &optional _)
  "Advice before `indent-region' to remove text properties between BEG to END."
  (when (region-active-p)
    (whitespace4r--hide (list (cons beg end)) t)))

(defun whitespace4r--after-change-function (beg end len)
  "Remove text properties from BEG to END.

LEN is zero for insertion (including undo, paste...), non-zero for deletion.

This function must be called from `after-change-functions'."
  (when (and (not (region-active-p)) (zerop len) (< beg end))
    (whitespace4r--hide (list (cons beg end)))))

;;;###autoload
(define-minor-mode whitespace4r-mode
  "Toggle whitespace visualization for selected region (Whitespace4r mode)."
  :lighter " ws4r"
  :init-value nil
  :global     nil
  :group      'whitespace4r
  (cond
   (whitespace4r-mode
    (add-hook 'activate-mark-hook #'whitespace4r--activate-mark-cb  100 t)
    (add-hook 'deactivate-mark-hook #'whitespace4r--deactivate-mark-cb 100 t)
    (add-hook 'after-change-functions #'whitespace4r--after-change-function nil t)
    (advice-add 'indent-region :before #'whitespacer4r--advice-indent-region)
    (whitespace4r--activate-mark-cb))
   (t
    (remove-hook 'activate-mark-hook #'whitespace4r--activate-mark-cb t)
    (remove-hook 'deactivate-mark-hook #'whitespace4r--deactivate-mark-cb t)
    (remove-hook 'after-change-functions #'whitespace4r--after-change-function t)
    (advice-remove 'indent-region #'whitespacer4r--advice-indent-region)
    (whitespace4r--deactivate-mark-cb))))

(provide 'whitespace4r)

;;; whitespace4r.el ends here
