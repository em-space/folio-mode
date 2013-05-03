;;; folio-levenshtein.el --- Levenshtein algorithm for Folio mode

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

;; Compute the Levenshtein edit distance between two strings.  This
;; version is pretty much the standard algorithm extended to allow
;; specifying an upper limit edit distance for minimizing search
;; times.  The implementation is space efficient O(2m) because only
;; two columns of the usual matrix O(n m) for storing the prefix
;; character weights of deletion, insertion, and substitution is
;; maintained.

;;; Code:

(require 'cl)

(defun folio-levenshtein-distance (str1 str2 &optional max-distance)
  "Calculate the Levenshtein distance between two strings.
Return the editing distance between strings STR1 and STR2 as an
integer.  Case is ignored if the variable `case-fold-search' is
non-nil in the current buffer (see which).  If the optional
parameter MAX-DISTANCE is non-nil, the calculation is aborted
prematurely as soon as the minimum Levenshtein distance between
prefixes of the strings exceeds the maximum allowed distance.
The return value in this case is nil."
  (let ((n (length str1))
        (m (length str2)))
    (cond
     ((= 0 n) ;; trivial cases
      m)
     ((= 0 m)
      n)
     ((and max-distance (> (abs (- n m)) max-distance))
      nil)
     ((> n m) ;; str1 should be the shorter string
      (folio-levenshtein-distance str2 str1 max-distance))
     (t
      ;; only two columns are maintained, the current one that is
      ;; being built and the previous one
      (let ((col (make-vector (1+ m) 0))
            (prev-col (make-vector (1+ m) 0))
            break)
        (dotimes (i (1+ m))
          (setf (aref prev-col i) i))
        (let ((i 0))
          (while (< i n)
            (let ((prefix-distance m))
              (setf (aref col 0) (1+ i))
              (dotimes (j m)
                (setf (aref col (1+ j))
                      (min (1+ (aref col j))
                           (1+ (aref prev-col (1+ j)))
                           (+ (aref prev-col j)
                              (if (char-equal (elt str1 i)
                                              (elt str2 j)) 0 1))))
                (setq prefix-distance (min prefix-distance
                                           (aref col (1+ j)))))
              (if (and max-distance
                       (> i prefix-distance)
                       (> prefix-distance max-distance))
                    (setq i n break t)
                ;; Swap columns.
                (cl-rotatef col prev-col)
                (setq i (1+ i))))))
        (if (or (null max-distance)
                (and (null break)
                     (<= (aref prev-col m) max-distance)))
            (aref prev-col m)))))))

;; (eq (folio-levenshtein-distance
;;      "Marry Poppins"
;;      "Supercalifrajilisticexpialidotious") 29)

;; (folio-levenshtein-distance
;;  "Supercalifragilisticexpialidocious"
;;  "Supercalifrajilisticexpialidotious" 2)

;; (folio-levenshtein-distance
;;  "Supercalifrajilisticexpialidotious"
;;  "Supercalifragilisticexpialidocious" 2)



(provide 'folio-levenshtein)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-levenshtein.el ends here
