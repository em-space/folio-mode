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

;; This package provides dictionaries, static but compact fast random
;; access data structures maintaining a fixed lexicographic sort
;; order.  Dictionaries use a minimal acyclic finite state automaton
;; for storing words, and a minimal perfect hash function to map the
;; word of an entry to its value, if any.  Dictionaries support fast
;; exact and fuzzy queries using Levenshtein edit distances.

;;; Code:

(require 'folio-automata)

;; XXX (require 'folio-hash)

(defun folio-make-dictionary (entries &optional predicate extra-slots)
Dictionaries are compact fast random access data structures
maintaining a fixed lexicographic sort order.  They are meant for
basically static word corpora where inserts and deletes happen
rarely as a dictionary has to be recreated to accommodate a
modification.  A dictionary can be queried for exact matches or,
given a maximal edit distance, also for word similarities.

A dictionary entry is a word and, optionally, an associated value.
ENTRIES can be a hash table, an alist or a plain word list.  In
the latter case no value is assumed at construction time, for a
hash table the key's value, and for an alist the cdr of a list
member is stored.

LESSP is a binary function that is called for any two elements of
ENTRIES.  If ENTRIES is a hash table, the element is the cons of
key and value much like for an alist.  The default for LESSP is
`string-lessp'.  If ENTRIES is known to be correctly sorted
already, LESSP should be set to `identity'.

KEYWORDS are additional keyword arguments.  If the keyword
:no-values is non-nil no storage is reserved for word-specific
data; none then can be added later.  Additionally, a dictionary
can have extra slots to hold additional data not associated with
particular entries.  The :extra-slots keyword specifies the
number of slots to allocate.

The primary query function for is `folio-lookup-dictionary';
general dictionary traversal is provided by
`folio-map-dictionary', and slots are accessible using
`folio-dictionary-extra-slot' and
`folio-dictionary-set-extra-slot', respectively, see which."
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
  "Lookup WORD in the dictionary DICT.

KEYWORDS are additional keyword arguments.

The search is exact unless the keyword :max-distance is set to a
non-zero value in which case a Levenshtein similarity search is
performed considering insertion, deletion, and substitution of
letters.

If the query for WORD was successful return non-nil, or nil else.
For a successful search, the return value is an alist of key-value
pairs unless the keyword :no-values is set to non-nil in which case
the return value is a plain list of words."

(defun folio-map-dictionary (function dict)
  "Apply FUNCTION to each entry in the dictionary DICT."
  (folio-map-mafsa function (aref dict 0)))

(defun folio-dictionary-extra-slot (dict n)
  "Return the contents of extra slot N of DICT.

The number of extra slots in a dictionary is fixed at
construction time."
  (aref (aref dict 2) n))

(defun folio-dictionary-set-extra-slot (dict n value)
  "Store VALUE in extra slot N of DICT.

The number of extra slots in a dictionary is fixed at
construction time."
  (aset (aref dict 2) n value))

(provide 'folio-dictionary)

;;; folio-dictionary.el ends here
