;;; folio-word-break.el --- Word Boundary Detection for Folio mode

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

;; Support for word boundary detection for but not limited to pattern
;; matching, text segmentation, and line breaking (wrapping)
;; algorithms.

;;; Code:

(eval-and-compile
  (defvar folio-word-break-table nil
    "Character table associating code points with word break properties."))

(eval-and-compile
  (defun folio-word-break-parse-table-data (table)
    "Parse the Unicode `WordBreakProperty.txt' file in the format as
specified by http://www.unicode.org/reports/tr44/.
Store the result in the global variable
`folio-word-break-table'."
    (let ((property
           "^\\([0-9A-F]+\\)\\(?:\\.\\.\\([0-9A-F]+\\)\\)?\
\\s-+;\\s-+\\(\\w+\\)\\s-+#"))
      (goto-char (point-min))
      (while (re-search-forward property nil t)
        (let ((chars (if (match-string 2)
                         (cons (string-to-number (match-string 1) 16)
                               (string-to-number (match-string 2) 16))
                       (string-to-number (match-string 1) 16)))
              (value (intern (match-string 3))))
          (set-char-table-range table chars value)))))

  (defun folio-word-break-load-table (&optional file)
    (let ((table (make-char-table 'word-break-table)))
      (with-temp-buffer
        (insert-file-contents-literally
         (or file (file-truename "data/aux-word-break-property.txt")))
        (folio-word-break-parse-table-data table))
      table)))

(setq folio-word-break-table
      (eval-when-compile (folio-word-break-load-table)))

(defsubst folio-word-break-property (c)
  "Return the word-break property of the character C.
The return value is a symbol from the set \(CR LF Newline Extend
Format Katakana ALetter MidLetter MidNum MidNumLet Numeric
ExtendNumLet)."
  (aref folio-word-break-table c))

(defsubst folio-mid-numeric-p (c)
  "Return non-nil if the character C has one of the properties
MidNum or MidNumLet."
  (memq (folio-word-break-property c) '(MidNum MidNumLet)))

(defun folio-numeric-sequence-p (str)
  "Return non-nil if STR matches a numeric sequence."
  (when (and (eq (folio-word-break-property
                  (aref str 0)) 'Numeric)
             (eq (folio-word-break-property
                  (aref str (1- (length str)))) 'Numeric))
    (let ((i 1)
          (j (1- (length str)))
          (match t))
      (while (and match (< i j))
        (setq match (memq (folio-word-break-property (aref str i))
                          '(Numeric MidNum MidNumLet))
              i (1+ i)))
      match)))


(provide 'folio-word-break)

;;; folio-word-break.el ends here
