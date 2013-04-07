;;; folio-dialog.el --- Folio mode project buffer

;; Copyright (C) 2012, 2013  Christoph W. Kluge

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

;; Maintain a project buffer in the form of individual chapters or
;; pages for the representation of project state and user interaction
;; within a workflow.  The pages are setup logically to guide through
;; a workflow without forcing the user into a strict order.
;;
;; This package combines all pages and binds them into what is called
;; the Folio mode project buffer.

;;; Code:

(require 'folio-base)
(require 'folio-dialog-forms)
(require 'folio-dialog-setup)
(require 'folio-dialog-pages)
(require 'folio-dialog-spellcheck)

(defun folio-project-buffer-setup (name parent)
  "Setup the Folio mode project buffer.
NAME is the project buffer object or name, PARENT the parent
buffer, normally the project text buffer."
  (let ((child (get-buffer name)))
    (with-current-buffer child
      (setq folio-dialog-form-page-list
            '(("Contents" folio-dialog-form-contents
               :header "Contents")
              ("Project Setup" folio-dialog-setup-page
               :header "Project Setup")
              ("Text Structure & Pagination" folio-dialog-pages-page
               :header "Text Structure & Pagination")
              ("Spellcheck" folio-dialog-spellcheck-page
               :header "Spellcheck")))
      (folio-dialog-form-mode)
      (overwrite-mode)
      (set (make-local-variable 'widget-documentation-face) 'default)
      (folio-set-parent-buffer parent))))

;;;###autoload
(defun folio-project-buffer-name (parent)
  "Return the name of the Folio mode project buffer.
PARENT is the text buffer."
  (format "*Project: %s*" (buffer-name parent)))

;;;###autoload
(defun folio-show-project-buffer (&optional parent)
  "Display the Folio mode project buffer."
  (interactive)
  (let* ((parent (get-buffer (or parent (current-buffer))))
         (mode (with-current-buffer parent major-mode)))
    (cond
     ((folio-mode-p mode)
      (let* ((buffer-name (folio-project-buffer-name parent))
             (buffer (get-buffer buffer-name)))
        (unless (eq buffer (switch-to-buffer-other-window
                            (or buffer buffer-name)))
          ;; Project buffer was newly created.
          (folio-project-buffer-setup buffer-name parent))))
     ((eq mode 'folio-dialog-form-mode)
      (switch-to-buffer-other-window parent))
     (t
      (error "Unrecognized project")))
    (folio-dialog-form-refresh)))


(provide 'folio-dialog)

;;; folio-dialog.el ends here
