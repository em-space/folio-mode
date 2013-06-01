;;; folio-dictionary.el --- Folio mode dictionaries

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

(require 'folio-automata)

;; XXX (require 'folio-hash)

(defun folio-make-dictionary (entries &optional predicate extra-slots)
  (let ((dict (make-vector 3 nil))
        (fsa (folio-make-mafsa))
        (pred (symbol-function (or predicate #'string<)))
        keys)
    (if (consp (car entries))
        (setq keys (mapcar (lambda (x)
                             (car x))
                           (sort entries
                                 (lambda (x y)
                                   (funcall pred
                                            (cdr x) (cdr y))))))
      (setq keys (sort entries pred)))
    (mapc (lambda (x)
            (folio-mafsa-insert-word fsa x)) keys)
    (folio-mafsa-finalize fsa)
    (aset dict 0 fsa)
    ;; Index 1 is reserved for dictionary values.
    (unless (zerop (or extra-slots 0))
      (aset dict 2 (make-vector extra-slots nil)))
    dict))

(defun folio-lookup-dictionary (word dict &optional max-distance)
  "Lookup WORD in the dictionary DICT."
  (if (zerop (or max-distance 0))
      (folio-mafsa-string-accept-p (aref dict 0) word)
    (let ((levenshtein-dfa
           (folio-nfa-to-dfa (folio-make-levenshtein-nfa
                              word max-distance))))
      (folio-intersect-mafsa (aref dict 0) levenshtein-dfa))))


(provide 'folio-dictionary)

;;; folio-dictionary.el ends here
