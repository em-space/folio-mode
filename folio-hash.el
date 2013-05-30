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

;;; Code:

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

(defun folio-fnv (str &optional base)
  "Return a 24-bit hash of the string STR.
The hash algorithm is 32-bit FNV-1a xor-folded down to 24 bit."
  ;; http://isthe.com/chongo/tech/comp/fnv/
  ;; http://tools.ietf.org/html/draft-eastlake-fnv-03
  (let ((hash (or base #x811c9dc5)))
    (mapc (lambda (x)
            (setq hash (* #x01000193 (logxor hash x)))) str)
    (logxor (lsh hash -24) (logand hash #xffffff))))


(provide 'folio-hash)

;;; folio-hash.el ends here
