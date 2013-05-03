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
        (setq i 0)
        (while (< i n)
          (setq prefix-distance m)
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
            (setq i (1+ i))))
        (if (or (null max-distance)
                (and (null break)
                     (<= (aref prev-col m) max-distance)))
            (aref prev-col m)))))))

(defun folio-jaro-winkler-distance (str1 str2 &optional min-score)
  "XXX"
  (let ((score-threshold 0.7)
        (score-chars 3)
        (n (length str1))
        (m (length str2))
        (common 0)
        (transposed 0)
        (score 0.0)
        i j k range match1 match2)
    (cond ;; trivial cases
     ((or (= n 0) (= m 0))
      (when (= n m)
        (setq score 1.0)))
     ((> n m)
      (setq score (folio-jaro-winkler-distance
                   str2 str1 min-score)))
     (t
      ;; m is larger than or equal to n
      (setq match1 (make-vector n nil)
            match2 (make-vector m nil)
            range (max 0 (1- (/ m 2)))
            i 0)
      (while (< i n)
        (setq j (max 0 (- i range))
              k (min (+ i range 1) m))
        (while (< j k)
          (if (or (aref match2 j)
                  (not (char-equal (aref str1 i)
                                   (aref str2 j))))
              (setq j (1+ j))
            (aset match1 i t)
            (aset match2 j t)
            (setq common (1+ common)
                  j k)))
        (setq i (1+ i)))
      (unless (= common 0)
        (setq i 0 j 0 transposed 0)
        (while (< i n)
          (when (aref match1 i)
            (while (and (< j m)
                        (null (aref match2 j)))
              (setq j (1+ j)))
            (unless (char-equal (aref str1 i)
                                (aref str2 j))
              (setq transposed (1+ transposed)))
            (setq j (1+ j)))
          (setq i (1+ i)))
        ;; Jaro score
        (setq score (/ (+ (/ (float common) n)
                           (/ (float common) m)
                           (/ (- common (/ transposed 2.0))
                              (float common)))
                       3.0))
        ;; Winkler prefix boost
        (when (> score score-threshold)
          (setq j 0 k (min score-chars n))
          (while (and (< j k)
                      (char-equal (aref str1 j) (aref str2 j)))
            (setq j (1+ j)))
          (when (/= j 0)
            (setq score (+ score (* j 0.1 (- 1.0 score)))))))))
    (if (and min-score (< score min-score))
        nil
      score)))

(defun folio-ngram-distance (str1 str2 &optional min-score)
  "XXX"
  (let ((n (length str1))
        (m (length str2))
        (cost 0)
        (distance 0.0)
        i j tj0 tj1 sa0 sa1 sa p d)
    (cond
     ((or (= n 0) (= m 0))
      (setq distance 1.0))
     ((> m n) ;; str1 should be the longer string
      (setq distance (folio-ngram-distance
                      str2 str1 min-score)))
     ((< n 2)
      (dotimes (i n)
        (when (char-equal (aref str1 i) (aref str2 i))
          (setq cost (1+ cost))))
      (setq distance (/ (float cost) m)))
     (t
      ;; one char prefix padding for bi-grams
      (setq sa (concat (string #x0000) str1)
            p (make-vector (1+ n) 0.0)
            d (make-vector (1+ n) 0.0)
            i 1 j 1)
      (dotimes (i (1+ n))
        (aset p i i))
      (while (<= j m)
        ;; extract t_j n-gram
        (if (< j 2)
            (setq tj0 (aref str2 0)
                  tj1 (aref str2 1)
                  i 1)
          (setq tj0 (aref str2 (- j 2))
                tj1 (aref str2 (- j 1))
                i 1))
        (aset d 0 (float j))
        (setq i 1)
        (while (<= i n)
          ;; compare sa to t_j
          (setq sa0 (aref sa (1- i))
                sa1 (aref sa i)
                cost 0)
          (unless (= sa0 #x0000) ;; discount prefix
            (when (not (char-equal sa0 tj0))
              (setq cost (1+ cost)))
            (when (not (char-equal sa1 tj1))
              (setq cost (1+ cost)))
            (setq cost (/ (float cost) 2)))
          (aset d i (min (min (1+ (aref d (1- i)))
                              (1+ (aref p i)))
                         (+ (aref p (1- i)) cost)))
          (setq i (1+ i)))
        (cl-rotatef p d)
        (setq j (1+ j)))
      (setq distance (- 1.0 (/ (aref p n) (float n))))))
    (if (and min-score (< distance min-score))
        nil
      distance)))

(when nil
(let ((iii 0))
  (dotimes (iii 10000)

    ;; folio-ngram-distance        50000 33.919688999   0.0006783937
    ;; folio-levenshtein-distance  50000  7.3527599999  0.0001470551
    ;; folio-jaro-winkler-distance 50000  1.8399310000  3.679...e-05

    ;; (eq (folio-levenshtein-distance
    ;;  "Marry Poppins"
    ;;     "Supercalifrajilisticexpialidotious") 29)

    (folio-levenshtein-distance
     "Supercalifrajilisticexpialidotious"
     "Supercalifragilisticexpialidocious")
    (folio-jaro-winkler-distance
     "Supercalifrajilisticexpialidotious"
     "Supercalifragilisticexpialidocious")
    (folio-ngram-distance
     "Supercalifrajilisticexpialidotious"
     "Supercalifragilisticexpialidocious")

    (folio-levenshtein-distance "zimmermann" "cannon")
    (folio-jaro-winkler-distance "zimmermann" "cannon")
    (folio-ngram-distance "zimmermann" "cannon")

    (folio-levenshtein-distance "martha" "marhta")
    (folio-jaro-winkler-distance "martha" "marhta")
    (folio-ngram-distance "martha" "marhta")

    (folio-levenshtein-distance "aye" "cannon")
    (folio-jaro-winkler-distance "aye" "cannon")
    (folio-ngram-distance "aye" "cannon"))))


(provide 'folio-levenshtein)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-levenshtein.el ends here
