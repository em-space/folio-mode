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


(defun folio-uca-make-table-value (levels)
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
  (let (collation-levels)
    (while (looking-at "\\[[.*]\\([0-9a-f.]+\\)\\]")
      (let* ((pos (match-end 1))
             (weights (nbutlast (split-string
                                 (match-string 1) "[.]" t) 1))
             (levels (mapcar (lambda (x)
                               (string-to-number x 16))
                             weights)))
        (push levels collation-levels)
        (goto-char (1+ pos))))
    (nreverse collation-levels)))

(defvar folio-uca-table (make-char-table 'uca-table)
  "")

(defun folio-uca-load-table ()
  (goto-char (point-min))
  (while (re-search-forward
          "^\\([0-9a-f]+\\)\s+;\s+" nil t)
    (let ((char (string-to-number (match-string 1) 16))
          collation-elements)
      (while (progn
               (push (folio-uca-parse-levels)
                     collation-elements)
               (not (null (car collation-elements)))))
      (aset folio-uca-table
            char (folio-uca-make-table-value
                  (car (nreverse (cdr collation-elements))))))))


(provide 'folio-uca)

;;; folio-uca.el ends here
