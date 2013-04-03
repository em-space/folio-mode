;;; folio-log.el --- Folio mode log maintainance

;; Copyright (C) 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Folio mode maintains a log in Org mode for notes at the convenience
;; of the transcriber and to semi-automatically file corrections that
;; are made to the original text using dedicated functions like
;; `folio-replace-word'.

;;; Code:

(require 'org)

(defun folio-log-buffer-name (parent)
  "Return the name of the Folio log buffer.
PARENT is the text buffer."
  (format "%s.org" (file-name-sans-extension
                    (buffer-file-name parent))))

(defun folio-get-log-buffer (parent)
  "Return the Folio log buffer.
PARENT is the text buffer."
  (let ((file-name (folio-log-buffer-name parent)))
    (find-file-noselect file-name 'nowarn)))

(defconst folio-log-correction-heading "Corrections to text"
  "Folio auto-log first level heading.")

(defconst folio-log-spelling-heading "Orthography & Punctuation"
  "Folio auto-log second level heading.
Entries in this sections should be notes about corrections to
spelling and punctuation.  This heading must be child to
`folio-log-correction-heading'.")

(defun folio-log-folio-tag ()
  "Set the `folio' tag for the current line."
  (org-toggle-tag (propertize
                   "folio" 'help-echo "Managed by Folio mode") 'on))

(defun folio-log-transcribers-tag ()
  "Set the `transcribers' tag for the current line."
  (org-toggle-tag (propertize
                   "transcribers" 'help-echo
                   "Subject to a transcriber's note") 'on))

;;;###autoload
(defun folio-log-format-spelling-correction (item-data)
  ;; XXX TODO add back-reference/jump
  (let* ((page (elt item-data 0))
         (bad (elt item-data 1))
         (good (elt item-data 2))
         (context (when (and (> (length item-data) 3)
                             (> (length (elt item-data 3)) 1))
                    (elt item-data 3)))
         (item (format "Page %d, \"%s\" corrected to \"%s"
                       page bad good)))
    (if context
        (progn
          (setq item (concat item
                             (format "\" (\"%s\")" context)))
          (unless (eq (aref context (1- (length context)))
                      ?.)
            (setq item (concat item "."))))
      (unless (eq (aref good (1- (length good)))
                  ?.)
        (setq item (concat item "."))))
    item))

;;;###autoload
(defun folio-log-spelling-correction (entry)
  "Log an entry for a correction in spelling or punctuation."
  (interactive "sEntry: ")
  (with-current-buffer (folio-get-log-buffer (current-buffer))
    (goto-char (point-min))
    (let ((pos (condition-case nil
                   (org-find-olp `(,folio-log-correction-heading
                                   ,folio-log-spelling-heading)
                                 (current-buffer))
                 (error nil))))
      (atomic-change-group
        (if pos
            (goto-char pos)
          (setq pos (goto-char (point-max)))
          (org-insert-heading 'force)
          (insert folio-log-correction-heading)
          (folio-log-folio-tag)
          (folio-log-transcribers-tag)
          (org-insert-subheading 'force)
          (insert folio-log-spelling-heading)
          (beginning-of-line)
          (when (looking-back "\n\n")
            (forward-char -1)
            (delete-char 1))
          (goto-char pos)
          (while (> (or (org-current-level) 1) 1)
            (org-promote-subtree))
          (forward-line 1)
          (org-end-of-subtree nil))
        (end-of-line)
        (when (= (forward-line 1) 1)
          (insert "\n"))
        (let ((struct (org-list-struct)))
          (if struct
              (progn
                (org-end-of-item-list)
                (when (org-at-heading-p)
                  (forward-char -1))
                (org-insert-heading)
                (end-of-line))
            (insert-char ?\ (1+ (org-current-level)))
            (insert (org-list-bullet-string "-"))))
        (insert (if (listp entry)
                    (folio-log-format-spelling-correction entry)
                  entry)))
      (org-end-of-item-list)
      (forward-char -1))))


(provide 'folio-log)

;;; folio-log.el ends here
