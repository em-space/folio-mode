;;; folio-hash.el --- Folio mode minimal perfect hashing

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

;; This package provides a hash table implementation based on the
;; construction of a minimal perfect hash function.  The algorithm for
;; creating the MPHF is of the classic mapping, ordering, searching
;; kind (MOS).  Although the searching step currently just is simple
;; trial-and-error, the MPHF is found in linear time.  ---XXX provide
;; teh numbers

;; Other algorithms are based on bipartite or acyclic graphs.  The
;; latter are necessary for finding order preserving minimal perfect
;; hash functions but also are interesting as a Folio dictionary
;; already is an acyclic graph.

;; Further reading:
;;   [1] Havas and Majewski, Optimal algorithms for minimal perfect
;;         hashing.
;;   [2] Belazzougui, Botelho, and Dietzfelbinger, Hash, displace, and
;;         compress.

;;; Code:

;; Layout of a hash table vector:
;;   [MPHF VALUES]
;;     where MPHF is an N-vector maintaining the minimal perfect hash
;;     function, and VALUES an N-vector maintaining the values
;;     associated with externally stored keys.

(defun folio-prime-p (n)
  "Return t if it is a prime number, or nil otherwise.
Run-time complexity is O\(sqrt\(N))."
  (interactive)
  (if (integerp n)
      (cond ((< n 2) nil)
            ((= n 2) t)          ;; the only even prime
            ((= 0 (% n 2)) nil)  ;; there are no other even primes
            (t (catch 'break
                 ;; using the most basic method: trial division
                 (let ((bound (floor (sqrt n)))
                       (div 2))
                   (while (<= div bound)
                     (if (= 0 (% n div))
                         (throw 'break nil))
                     (setq div (1+ div)))
                   t))))
    nil))

(defun folio-next-prime (n)
  "Return the next prime number after N, or nil otherwise.
Run-time complexity is O\(sqrt\(N))."
  (if (integerp n)
      (cond ((> n 1)
             ;; start from next odd number
             (let* ((next (if (= 0 (% n 2))
                              (1+ n)
                            (+ n 2))))
               (while (not (folio-prime-p next))
                 (setq next (+ next 2)))
               next))
            (t 2))
    nil))

(defun folio-hash-fnv (str &optional base)
  "Return a 24-bit hash of the string STR.
The hash algorithm is 32-bit FNV-1a xor-folded down to 24 bit."
  ;; http://isthe.com/chongo/tech/comp/fnv/
  ;; http://tools.ietf.org/html/draft-eastlake-fnv-03
  (let ((hash (or base #x811c9dc5)))
    (mapc (lambda (x)
            (setq hash (* #x01000193 (logxor hash x)))) str)
    (logxor (lsh hash -24) (logand hash #xffffff))))

(defun folio-make-mphf-hash-table (keys)
  "Given a set of a priori known KEYS return a perfect hash table.
The hash table is final with respect to its keys, i.e. none can
be added or removed.  The table is built by computing a minimal
perfect hash function for KEYS in O\(N\) space and time.  Note
that the hash table does not store the keys themselves which are
expected in external storage such as a DAFSA, but only associated
values.  See also `folio-mphf-gethash', `folio-mphf-puthash', and
`folio-make-mafsa'."
  (let* ((size (length keys))
         (buckets (make-vector size nil))
         (D (make-vector size nil))
         (T (make-bool-vector size nil))
         (G (lambda (x)
              (% (folio-hash-fnv x) size)))
         (H (lambda (bucket seed)
              (let ((i 0)
                    slot slots)
                (while (< i (length (cdr bucket)))
                  (setq slot (% (folio-hash-fnv
                                 (elt (cdr bucket) i) seed) size))
                  (if (or (aref T slot) (memq slot slots))
                      (setq i 0
                            slots nil
                            seed (1+ seed))
                    (setq slots (cons slot slots)
                          i (1+ i))))
                (mapc (lambda (x)
                        (aset T x t)) slots))
              seed)))
    ;; Mapping phase--Place all keys into buckets.
    (let (g)
      (mapc (lambda (x)
              (setq g (funcall G x))
              (setf (aref buckets g)
                    (cons g (cons x (cdr (aref buckets g))))))
            keys))
    ;; Ordering phase--Sort buckets by number of entries descending.
    (setq buckets (sort (append buckets nil)
                        (lambda (x y)
                          (< (length y) (length x)))))
    ;; Searching phase--Keys in each bucket are mapped separately to
    ;; the value vector T by processing buckets in order and seeking a
    ;; secondary hash function for each bucket that places entries
    ;; into distinct yet unoccupied slots of the value vector T.
    ;; Store this hash function in the intermediate distributor table
    ;; D at the position determined in the mapping phase.  Once there
    ;; are only buckets with only one entry remaining, store the address
    ;; into the hash table T directly in the form (1- (- index)).  For
    ;; simplicity the search trial-and-error.
    (let ((mphf (make-vector 2 nil))
          (seed 1)
          free-slots
          bucket-size)
      (catch 'break
        (mapc (lambda (x)
                (setq bucket-size (length (cdr x)))
                (cond ((zerop bucket-size)
                       (throw 'break t))
                      ((= bucket-size 1)
                       (unless free-slots
                         (dotimes (i size)
                           (unless (aref T i)
                             (push i free-slots))))
                       (aset D (car x) (1- (- (car free-slots))))
                       (setq free-slots (cdr free-slots)))
                      (t
                       (setq seed (funcall H x seed))
                       (aset D (car x) seed))))
              buckets))
      ;; The distributor D, effectively now storing the MPHF in at
      ;; most N machine words frequently is a sparse vector.  It is
      ;; stored uncompressed, though.
      (aset mphf 0 D)
      (aset mphf 1 (make-vector size nil))
      mphf)))

(defun folio-mphf-hash-table-p (object)
  "Return t if OBJECT is a perfect hash table."
  (and (vectorp object)
       (= (length object) 2)
       (vectorp (aref object 0))
       (vectorp (aref object 1))))

(defmacro folio-mphf-gethash (table key)
  "Lookup KEY in TABLE and return its associated value.
TABLE is a hash table as returned by `folio-make-mphf-hash-table'.
Note that a MPHF hash table has no concept for unknown keys: KEY
must be a member of the set of keys used when creating the table
or bogus data might get returned.  Time complexity of this
function is O\(1)."
  (let ((seed (make-symbol "seed")))
    `(let ((,seed (aref (aref ,table 0)
                        (% (folio-hash-fnv ,key)
                           (length (aref ,table 0))))))
       (if (< ,seed 0) ;; pseudo-hash from fast-branch
            (aref (aref ,table 1) (1- (- ,seed)))
         (aref (aref ,table 1) (% (folio-hash-fnv ,key ,seed)
                                 (length (aref ,table 1))))))))

(defsubst folio-mphf-puthash (key value table)
"Associate KEY with VALUE in hash table TABLE.
KEY must be a member of the set of keys used when creating the
table.  Also note that KEY must be maintained by storage external
to the TABLE, see `folio-make-mphf-hash-table'.  Time complexity
of this function is O\(1).  Return VALUE."
  (setf (folio-mphf-gethash table key) value))

(provide 'folio-hash)

;;; folio-hash.el ends here
