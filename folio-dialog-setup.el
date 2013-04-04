;;; folio-dialog-setup.el --- Folio project setup page

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

;; Define the project buffer page for setting up a Folio project.

;;; Code:

(require 'wid-edit)

(require 'folio-base)
(require 'folio-dublin-core)
(require 'folio-faces)
(require 'folio-image)

(defun folio-dialog-setup-page ()
  "Create the dialog page for setting up a Folio project."
  (widget-insert "\n\n")
  (widget-create 'const
                 :value ""
                 :format "%h"
                 :create (lambda (widget)
                           (let ((widget-documentation-face 'default))
                             (widget-default-create widget)))
                 :doc "Setup project credentials and directories.\n\
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.")

  (widget-insert "\n\n\n")
  (folio-form-indent 7)
  (widget-create 'string
                 :tag "Project ID"
                 :format "%t: %v"
                 :size 16
                 :value-face 'folio-widget-field
                 :value (or (folio-with-parent-buffer
                              (or folio-project-id
                                  (folio-assign-project-id))) "")
                 :help-echo "The PGDP Project Identifer.\nThis \
normally is of the form `projectID<hexadecimal number>'."
                 :notify (lambda (widget &rest ignore)
                           (folio-with-parent-buffer
                             (setq folio-project-id
                                   (widget-value widget)))))

  ;; XXX default input method for this project? `toggle-input-method'
  (widget-insert "\n\n")
  (folio-form-indent 12)
  (widget-create 'string
                 :tag "Title"
                 :format "%t: %v"
                 :size 20
                 :value-face 'folio-widget-field
                 :value (or (folio-with-parent-buffer
                              (or folio-text-title
                                  (setq folio-text-title
                                        (folio-pgdp-dc
                                         'title 'pretty))))
                            "")
                 :help-echo "The book's title."
                 :notify (lambda (widget &rest ignore)
                           (folio-with-parent-buffer
                             (setq folio-text-title
                                   (widget-value widget)))))

  (widget-insert "\n\n")
  (folio-form-indent 11)
  (widget-create 'string
                 :tag "Author"
                 :format "%t: %v"
                 :size 16
                 :value-face 'folio-widget-field
                 :value (or (folio-with-parent-buffer
                              (or folio-text-author
                                  (setq folio-text-author
                                        (folio-pgdp-dc
                                         'creator 'pretty)))) "")
                 :help-echo "The book's author."
                 :notify (lambda (widget &rest ignore)
                           (folio-with-parent-buffer
                             (setq folio-text-author
                                   (widget-value widget)))))
  (widget-insert "\n\n\n")
  (folio-form-indent 1)
  (widget-create 'string
                 :tag "Directory for page scans"
                 :format "%t: %v"
                 :size 14
                 :value-face 'folio-widget-field
                 :value folio-page-scan-directory-default
                 :notify (lambda (widget &rest ignore)
                           (let ((dir (widget-value widget)))
                             (folio-with-parent-buffer
                               (make-directory dir default-directory)
                               (setq folio-page-scan-directory dir)))))
  (widget-insert "\n\n")
  (widget-create 'string
                 :tag "Directory for book images"
                 :format "%t: %v"
                 :size 14
                 :value-face 'folio-widget-field
                 :value folio-image-directory-default
                 :notify (lambda (widget &rest ignore)
                           (let ((dir (widget-value widget)))
                             (folio-with-parent-buffer
                               (make-directory dir default-directory)
                               (setq folio-image-directory dir))))))


(provide 'folio-dialog-setup)

;;; folio-dialog-setup.el ends here
