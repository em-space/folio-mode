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

(eval-when-compile (require 'cl))

(require 'folio-automata)
(require 'folio-hash)

(defun folio-make-dictionary (entries &rest keywords)
  "Return a newly created dictionary with entries ENTRIES.

Dictionaries are compact fast random access data structures
maintaining a fixed lexicographic sort order.  They are meant for
basically static word corpora where inserts and deletes happen
rarely as a dictionary is immutable with respect to its keys;
accommodating additional entries or removing obsolete entries
requires recreating the dictionary.  A dictionary can be queried
for exact matches or, given a maximal edit distance, also for
word similarities.

A dictionary entry is a word and, optionally, an associated
value.  ENTRIES can be an alist or a plain word list.  In the
latter case no value is assumed at construction time.  The
relative order of members of ENTRIES is retained in a dictionary.

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
`folio-dictionary-set-extra-slot', respectively, see which.  The
dictionary's alphabet can be queried from
`folio-dictionary-alphabet'."
  (let ((dict (make-vector 4 nil))
        (extra-keywords nil)
        (extra-slots nil)
        (no-values nil)
        (folio-mafsa-maintain-alphabet t)
        keyword structured keys fsa mphf)

    ;; Destructure keywords.
    (while (keywordp (setq keyword (car keywords)))
      (setq keywords (cdr keywords))
      (pcase keyword
        (`:extra-slots (setq extra-slots (pop keywords)))
        (`:no-values (setq no-values (pop keywords)))
        (_ (push keyword extra-keywords)
           (push (pop keywords) extra-keywords))))
    (when extra-keywords
      (error "Unrecognized keywords `%s'"
             (nreverse extra-keywords)))

    ;; Prepare dictionary data.
    (let ((i 0))
      (cond ((and (listp entries)
                (consp (car entries))
                (stringp (caar entries)))
           (mapc (lambda (x)
                   (setq structured (cons (cons i x) structured)
                         keys (cons (car x) keys)
                         i (1+ i))) entries))
          ((and (listp entries)
                (stringp (car entries)))
           (mapc (lambda (x)
                   (setq structured (cons (cons i x) structured)
                         i (1+ i))) entries))
          (t
           (error "Invalid dictionary data"))))

    ;; Sort data by sorting the keys.  The internal sort order must be
    ;; binary not lexicographic for the graph not to break apart at
    ;; the last common prefix!  The lexicographic sort order is the
    ;; outer sort order; it is maintained by assigning every final
    ;; state marking the end of a word with the word position in the
    ;; original sequence.
    (setq structured
          (sort structured (lambda (x y)
                             (string-lessp (cadr x) (cadr y)))))
    ;; Build data structures.
    (setq fsa (folio-make-mafsa)
          mphf (unless no-values
                 (folio-make-mphf-hash-table keys)))
    (lexical-let ((fsa fsa)
                  (mphf mphf))
      (if no-values
          (mapc (lambda (x)
                  (folio-mafsa-insert-word fsa (cadr x) (car x)))
                structured)
        (mapc (lambda (x)
                (folio-mafsa-insert-word fsa (cadr x) (car x))
                (folio-mphf-puthash (cadr x) (cddr x) mphf))
              structured)))
    (folio-mafsa-finalize fsa)
    (aset dict 0 fsa)
    (aset dict 1 mphf)
    (unless (zerop (or extra-slots 0))
      (aset dict 2 (make-vector extra-slots nil)))
    (aset dict 3 (length keys))
    dict))

(defun folio-dictionary-p (object)
  "Return non-nil if OBJECT is a dictionary."
  (and (vectorp object)
       (= (length object) 4)
       (folio-mafsa-p (aref object 0))
       (folio-mphf-hash-table-p (aref object 1))
       (integerp (aref object 3))))

(defun folio-dictionary-count (dict)
  "Return the number of entries in DICT."
  (aref dict 3))

(defun folio-dictionary-alphabet (dict &optional as-alist)
  "Return the alphabet of the dictionary DICT.

The return value is a char-table mapping letter to absolute
occurrence count.  If AS-ALIST is non-nil return an alist
instead, in no particular order."
  (let ((alphabet (folio-mafsa-alphabet (aref dict 0))))
    (if as-alist
        (let ((len (length alphabet))
              from to alist)
          (catch 'break
            (map-char-table
             #'(lambda (k v)
                 (if (consp k)
                     (progn
                       (setq from (car k) to (cdr k))
                       (if (and (= to len) (zerop v)) ;; at end
                           (throw 'break t)
                         (unless (zerop v)
                           (while (< from (1+ to))
                             (setq alist (cons (cons from v)
                                               alist)
                                   from (1+ from))))))
                   (unless (zerop v)
                     (setq alist (cons (cons k v) alist)))))
             alphabet))
          alist)
      alphabet)))

(defun folio-lookup-dictionary (word dict &rest keywords)
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
  (let (keyword extra-keywords max-distance no-values table keys
                values)

    ;; Destructure keywords.
    (while (keywordp (setq keyword (car keywords)))
      (setq keywords (cdr keywords))
      (pcase keyword
        (`:max-distance (setq max-distance (pop keywords)))
        (`:no-values (setq no-values (pop keywords)))
        (_ (push keyword extra-keywords)
           (push (pop keywords) extra-keywords))))
    (when extra-keywords
      (error "Unrecognized keywords `%s'"
             (nreverse extra-keywords)))

    ;; Retrieve keys.
    (if (zerop (or max-distance 0))
        (when (folio-mafsa-string-accept-p (aref dict 0) word)
          (setq keys (list word)))
      (let ((levenshtein-dfa
             (folio-nfa-to-dfa (folio-make-levenshtein-nfa
                                word max-distance))))
        (setq keys (folio-intersect-mafsa
                    (aref dict 0) levenshtein-dfa))))

    ;; Collect associated values.
    (if no-values
        keys
      (setq table (aref dict 1))
      (mapc (lambda (x)
              (push (cons x (folio-mphf-gethash
                             table x)) values)) keys)
      values)))

(defmacro folio-get-dictionary-entry (word dict)
  "Return the dictionary entry for WORD.

WORD must have been added to the dictionary DICT previously or
bogus data might get returned.  Different from
`folio-lookup-dictionary' the return value is a generalized
variable that can be modified using `setf', `aset', etc."
  `(folio-mphf-gethash (aref ,dict 1) ,word))

(defun folio-map-dictionary (function dict &rest keywords)
  "Apply FUNCTION to each entry in the dictionary DICT.

KEYWORDS are additional keyword arguments.

FUNCTION should be a binary function accepting key and value
arguments unless the keyword :no-values is set to non-nil in
which case it only is called with one argument for the key."
  (let (keyword extra-keywords no-values values)

    ;; Destructure keywords.
    (while (keywordp (setq keyword (car keywords)))
      (setq keywords (cdr keywords))
      (pcase keyword
        (`:no-values (setq no-values (pop keywords)))
        (_ (push keyword extra-keywords)
           (push (pop keywords) extra-keywords))))
    (when extra-keywords
      (error "Unrecognized keywords `%s'"
             (nreverse extra-keywords)))

    ;; Map over FSA and fetch associated values.
    (if (unless no-values
          (setq values (aref dict 1)))
        (lexical-let ((function function)
                      (values values))
          (folio-map-mafsa
           (lambda (x)
             (funcall function x (folio-mphf-gethash
                                  values x))) (aref dict 0)))
      (folio-map-mafsa function (aref dict 0)))))

(defun folio-dictionary-extra-slot (dict n)
  "Return the contents of extra slot N of DICT.

The number of extra slots in a dictionary is fixed at
construction time."
  (aref (aref dict 2) n))

(defun folio-dictionary-set-extra-slot (dict n value)
  "Store VALUE in extra slot N of DICT.

The number of extra slots in a dictionary is fixed at
construction time.  Return VALUE."
  (aset (aref dict 2) n value))


(provide 'folio-dictionary)

;;; folio-dictionary.el ends here
