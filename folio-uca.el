;;; folio-uca.el --- Unicode Collation Algorithm for Folio mode

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

;;

;;; Code:

(defvar folio-uca-table (make-char-table 'uca-table)
  "DUCET table associating code points to collation vectors.
As of Unicode 6.2.0 this table maintains 24405 single-character
entries, 723 two-character contractions and 4 tree-character
contractions.")

(defun folio-uca-make-table-value (levels)
  "Return a byte vector of collation element weights.
The first level DUCET weight is stored in the first 16 bit of the
vector, followed by 16 bit for the level two weight, followed by
8 bit for the level three weight.

The DUCET fourth level is not stored as it is computable: In most
cases the fourth level is equal to the code point itself; the
fourth level can be used for a deterministic comparison of
Unicode strings for the purpose of deterministic sort keys to
little effect to the practical application of the algorithm (see
UTS #10, Appendix A.)"
  (let ((weights (make-vector
                  (* (length levels) (+ 2 2 1)) 0))
        (index -1))
    (mapc (lambda (x)
            ;; 16 bit level 1
            (aset weights (setq index (1+ index))
                  (lsh (elt x 0) -8))
            (aset weights (setq index (1+ index))
                  (logand (elt x 0) #x00ff))
            ;; 16 bit level 2
            (aset weights (setq index (1+ index))
                  (lsh (elt x 1) -8))
            (aset weights
                  (setq index (1+ index))
                  (logand (elt x 1) #x00ff))
            ;; 8 bit level 3
            (aset weights
                  (setq index (1+ index))
                  (logand (elt x 2) #x00ff))) levels)
    weights))

(defun folio-uca-parse-levels ()
  "Parse a sequence of collation element weights at point.
Return a list of weights for all levels but excluding the
parameter marking the element variable.  Point ought to be
positioned at the opening bracket.  The call to this defun must
be repeated for entries having multiple character such as
contractions."
  (let (levels)
    (while (looking-at "\\[[.*]\\([0-9a-f.]+\\)\\]")
      (let* ((pos (match-end 1))
             (match (nbutlast (split-string
                               (match-string 1) "[.]" t) 1))
             (level (mapcar (lambda (x)
                              (string-to-number x 16))
                            match)))
        (push level levels)
        (goto-char (1+ pos))))
    (nreverse levels)))

(defun folio-uca-table-put-internal (table char-list
                                           collation-elements)
  "Recursive helper function for `folio-uca-table-put'.
This function should not be called directly."
  (let* ((char (pop char-list))
         (node (aref table char)))
    (if (null char-list)
        (setq node (cons (folio-uca-make-table-value
                          collation-elements) (cdr node)))
      (setq node (cons (car node)
                       (or (cdr node)
                           (make-char-table 'uca-aux-table)))))
    (aset table char node)
    (when char-list
      (folio-uca-table-put-internal
       (cdr (aref table char)) char-list collation-elements))))

(defun folio-uca-table-put (char-list collation-elements)
  "Store an entry in the DUCET table.
CHAR-LIST and COLLATION-ELEMENTS list the character or character
contractions and their weights as parsed from the `allkeys' DUCET
file."
  (folio-uca-table-put-internal
   folio-uca-table char-list collation-elements))

(defun folio-uca-parse-table ()
  "Parse the DUCET `allkeys' file in the format as specified by
sec. 3.6.1 of the UCA.  Store the result in the global variable
`folio-uca-table'.  For querying the table the defun
`folio-uca-find-prefix' should be used, see which."
  (let ((char-list-regexp
         "^\\([0-9a-f]+\\(?:\s+[0-9a-f]+\\)*\\)\s+;\s+"))
    (goto-char (point-min))
    (while (re-search-forward char-list-regexp nil t)
      (let ((chars (mapcar (lambda (x)
                             (string-to-number x 16))
                           (split-string
                            (match-string 1) "[ ]" t)))
            collation-elements)
      (while (progn
               (push (folio-uca-parse-levels)
                     collation-elements)
               (not (null (car collation-elements)))))
      (setq collation-elements
            (car (nreverse (cdr collation-elements))))
      (folio-uca-table-put chars collation-elements)))))

;; XXX eval-when-compile parse/dump table
;; XXX eval-after-load load table

(defun folio-uca-find-prefix (prefix)
  "Return the DUCET collation elements for the string prefix PREFIX.
PREFIX is a list of character codes.  Return value a cons of a
vector of collation elements and a list of remainder characters
from PREFIX with the longest matching prefix contraction removed.
If there is no match return nil."
  (let ((remainder prefix)
        (node (aref folio-uca-table (car prefix))))
    (while (when (and node (progn
                             (pop remainder)
                             (cdr node)) remainder)
             (let ((child (aref (cdr node) (car remainder))))
               (when child
                 (setq node child)))))
    (when node
      (cons (car node) remainder))))


(provide 'folio-uca)

;;; folio-uca.el ends here
