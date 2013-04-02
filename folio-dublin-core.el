;;; folio-dublin-core.el --- DC document processing for Folio mode

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

;; This package provides defuns for processing Dublin Core documents.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'url)
(require 'url-http)
(require 'xml)

(require 'folio-base)

(defvar folio-pgdp-dc nil
  "PGDP Dublin Core document for the current project.
The document is in the form of a parse-tree as returned by
`xml-parse-file'.")
(make-variable-buffer-local 'folio-pgdp-dc)

;; From the current PGDP DC unfortunately the elements `description',
;; `date', `format', `language', and `source' often are unused or
;; contain non-sense, like a LC number in `source' that is anything
;; but a Library of Congress number.

(defun folio-retrieve-pgdp-dc (project-id &optional timeout)
  "Retrieve the DC document for the PROJECT-ID.
The XML file is fetched from a pgdp.net server using HTTP.
`folio-pgdp-dc' should be used to extract individual elements
from this document."
  ;; XXX TODO This should be using a proper hook for notifying errors
  ;; and informing about progress.
  (let ((url (concat folio-pgdp-projects-url project-id "/dc.xml"))
        (buffer (current-buffer)))
    (url-retrieve url (lambda (status buffer)
                        (unless status
                          (let* ((response-buffer (current-buffer))
                                 (pos (url-http-symbol-value-in-buffer
                                       'url-http-end-of-headers
                                       response-buffer))
                                 (dc (xml-parse-region
                                      pos (point-max))))
                            (with-current-buffer buffer
                              (setq folio-pgdp-dc dc))
                            (kill-buffer response-buffer))))
                  (list buffer))))

(defun folio-retrieve-synchronously-pgdp-dc (project-id
                                             &optional timeout)
  "A synchronous version of `folio-retrieve-pgdp-dc'.
Compared to the asynchronous `url-retrieve' with Emacs 24.1.50
the alternative `url-retrieve-synchronously' takes abnormally
long time to finish for reasons yet unknown."
  (let* ((url (concat folio-pgdp-projects-url
                      project-id "/dc.xml"))
         (buffer (url-retrieve-synchronously url))
         (pos (url-http-symbol-value-in-buffer
               'url-http-end-of-headers buffer))
         dc)
    (when (and buffer pos)
      (with-current-buffer buffer
        (setq dc (xml-parse-region pos (point-max)))
        (kill-buffer (current-buffer))))
    dc))

(defun folio-pgdp-dc (element &optional pretty)
  "Return the XML element value for the node with name ELEMENT.
ELEMENT must be a symbol like 'title or 'author.  If PRETTY is
non-nil leading and trailing whitespace and comments in brackets
or curly braces are removed.  Otherwise the element value is
returned as is.  The encoding should be UTF-8."
  (let* ((dc (progn
               (unless folio-pgdp-dc
                 (condition-case nil
                     (folio-retrieve-pgdp-dc folio-project-id)
                   (error nil)))
               folio-pgdp-dc))
         (dc-elt (and dc (caddr (car (xml-get-children
                                      (car-safe dc) element))))))
    (if (and dc-elt pretty)
        (with-temp-buffer
          (insert dc-elt)
          (goto-char (point-min))
          (save-match-data
            (let ((kill-list "^\s+\\|\\[[^]]+\\]\\|{[^}]+}"))
              (while (re-search-forward kill-list nil t)
                (replace-match "" nil t))))
          (delete-trailing-whitespace)
          (buffer-string))
      dc-elt)))


(provide 'folio-dublin-core)

;;; folio-dublin-core.el ends here
