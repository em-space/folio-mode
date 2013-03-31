;;; folio-roman.el --- Conversion of Roman numerals for Folio mode

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

;; Convert Roman numerals to decimal numbers and vice versa.
;; `folio-arabic-to-roman' and `folio-roman-to-arabic' currently do
;; not support the archaic writing system for large numbers (4000 and
;; above) that used horizontal bars above a numeral to indicate
;; multiplication: _M = 1000,000.  The largest number that can be
;; written in Roman numerals is 3,999,999.  The Roman numeral system
;; did not include zero and Romans had no concept of it in their
;; arithmetic.

;; To add some more lore, in the Middle Ages, Latin writers used
;; additional vertical lines on either side of the numeral to denote
;; one hundred times the number.  So an M with a horizontal bar above
;; and two vertical lines on either side was 1,000 x 1,000 x 100 = one
;; hundred million.  Alternatively "parentheses" were used, C and its
;; mirror or upside down C, and the letter I to denote multiplication
;; by 1000: (I) (resembling M) is 1,000, and (X) 10,000.  In medieval
;; texts and some early printed books, the numerals are written in
;; lower case letters and u was frequently substituted for v.  In the
;; final position of the numeral, j could be used instead of i.  So 18
;; could be written xuiij rather than XVIII.  These substitutions are
;; particularly found in italic fonts.

;;; Code:

(require 'cl) ;; for coerce

(defconst folio-arabic-numeral "Arabic"
  "Symbol definition for uses of arabic numerals.")

(defconst folio-roman-numeral "Roman"
  "Symbol definition for uses of roman numerals.")

(defconst folio-roman-arabic-numerals-alist
  '((1000 . (?M))
    (900 . (?C ?M))
    (500 . (?D))
    (400 . (?C ?D))
    (100 . (?C))
    (90 . (?X ?C))
    (50 . (?L))
    (40 . (?X ?L))
    (10 . (?X))
    (9 . (?I ?X))
    (5 . (?V))
    (4 . (?I ?V))
    (1 . (?I)))
  "Alist serving as a lookup-table for the conversion to roman numerals.
The ordering is descending and must not be changed.  See also
`folio-arabic-to-roman'.")

(defconst folio-arabic-roman-numerals-alist
  '((?M . 1000)
    (?D . 500)
    (?C . 100)
    (?L . 50)
    (?X . 10)
    (?V . 5)
    (?U . 5)
    (?I . 1)
    (?J . 1))
  "Alist mapping roman numerals to arabic numbers.")

(defun folio-arabic-to-roman (number &optional archaic)
  "Convert the decimal NUMBER to roman numerals.
The parameter ARCHAIC is meant for the old Roman system using
horizontal lines above a numeral to denote multiplication.  It
currently is ignored.  The result is a string and always in upper
case; see `downcase'."
  (when (zerop number)
    (signal 'arith-error '(number)))
  (let ((rest number)
        (result '()))
    (while (> rest 0)
      (mapc (lambda (x)
              (while (>= rest (car x))
                (setq result (append result (cdr x)))
                (setq rest (- rest (car x)))))
            folio-roman-arabic-numerals-alist))
    (coerce result 'string)))

(defun folio-roman-to-arabic-recur (rest last sum)
  "Recursive helper for `folio-roman-to-arabic'."
  (if (consp rest)
      (let ((current (car rest))
            (rest (cdr rest)))
        ;; Recursively apply the summation by substracting the current
        ;; value if the last number was less than the current value or
        ;; otherwise add its value to the sum.  The subtractive
        ;; numeral to the left must be I, X, or C.  This is using a
        ;; none-strict approach, though.
        (folio-roman-to-arabic-recur
         rest current (funcall (if (< current last) #'- #'+)
                               sum current))) sum))

(defun folio-roman-to-arabic (numeral &optional archaic)
  "Convert the roman numeral NUMERAL to a decimal integer.
The parameter ARCHAIC is meant for the old Roman system using
horizontal lines above a numeral to denote multiplication.  It
currently is ignored.  Return the value of NUMERAL as a decimal
integer."
  (folio-roman-to-arabic-recur
      ;; Coerce the string of roman numerals NUMERAL into a list of
      ;; characters and recursively sum up their decimal values.
      (mapcar (lambda (x)
                (or (cdr (assoc x folio-arabic-roman-numerals-alist))
                    (signal 'arith-error '(numeral))))
              (nreverse (append (upcase numeral) nil)))
      0 0))


(provide 'folio-roman)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-roman.el ends here
