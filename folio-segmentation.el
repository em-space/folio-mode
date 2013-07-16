;;; folio-segmentation.el --- Text segmentation for Folio mode

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

;; This package provides functionality for the identification of a
;; text's structure and macro typographic elements such as pages,
;; chapters, sections, illustrations, side-notes, footnotes,
;; block-quotes (wrapable), or no-wrap paragraphs including poetry.

;;; Code:

(require 'folio-core)

; XXX:TODO query custom value, hook into before-save

(defun folio-scan-page-separators (&optional buffer)
  "Scan page separators from text file.
BUFFER is the buffer or buffer name to scan.  If omitted this
defaults to the current buffer.

Once page information is available the physical page number is shown in
the header line, references to page scan images become available, and
the proofer names for the current page can be queried."
  (interactive)
  (let ((buffer (get-buffer (or buffer (current-buffer))))
        (page 0)
        page-markers
        page-scans
        proofer-from-page)

    ;; XXX todo/cleanup
    (setq proofer-from-page (make-hash-table :test 'string=))

    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))

          ;; should the format of the page separator change the code
          ;; most like has to be updated too; the regex therefore
          ;; neither is defined globally nor subject to customizing
          (let ((separator-pp (concat
                               "-----File: \\(\\(?:[pi]_\\)?[0-9]+[^-]+\\)-+"
                               "\\\\\\([^\\]+\\)"
                               "\\\\\\([^\\]+\\)"
                               "\\\\\\([^\\]+\\)"
                               "\\\\\\([^\\]+\\)"
                               "\\\\\\([^\\]+\\)\\\\-*$")))
            (while (not (bobp))
              (when (looking-at separator-pp)
                (setq page (+ 1 page))
                (setq page-markers (cons (point) page-markers))
                (setq page-scans (cons (match-string-no-properties 1 nil)
                                       page-scans)))
              (forward-line -1)))))
      ;; XXX

      ;; update the various buffer local tables

      ;; folio-proofer-from-page proofer-from-page
      (folio-restore-page-markers page-markers)
      (folio-restore-page-scans page-scans))))


(provide 'folio-segmentation)

;;; folio-segmentation.el ends here
