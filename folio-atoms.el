;;; folio-atoms.el --- Folio mode basic building blocks

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

;; This package defines basic building blocks not related to any
;; particular functionality.  Functions include operations on lists
;; and strings, buffer motion, and other auxiliary utilities.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'folio-compat)

;; Randomize the seed in the random number generator.
(random t)

;;;; lists

;;;###autoload
(defsubst folio-filter-list (list pred)
  "Filter the elements of LIST according to the predicate PRED."
  (delq nil (mapcar (lambda (x)
                      (and (funcall pred x) x)) list)))

;;;###autoload
(defun folio-shuffle-list (list)
  "Randomly permute the elements of LIST.
All permutations are equally likely."
  ;; Adapted from 'shuffle-vector' in cookie1.el
  (let ((i 0)
        j
        temp
        (len (length list)))
    (while (< i len)
      (setq j (+ i (random (- len i))))
      (setq temp (nth i list))
      (setf (nth i list) (nth j list))
      (setf (nth j list) temp)
      (setq i (1+ i))))
  list)

;;;###autoload
(defun folio-sublist (list start &optional end)
  "Return a sublist of LIST.
Index START counts from 0, END is exclusive.  If END is omitted,
it defaults to the length of LIST.  Otherwise END should be
greater than or equal to START.

This function can operate destructively; write
`(setq l (folio-sublist from))' to be sure of correctly changing the
value of a list `l'."
    (when (> start 0)
      (setq list (nthcdr start list)))
    (when (and end (>= end start))
      (setcdr (nthcdr (- end start 1) list) nil))
    list)

;;;###autoload
(defmacro folio-assoc-pop (key alist)
  "Pop element with key KEY from ALIST.

Return the first element of ALIST whose car is `equal' to KEY, and
delete it from the list.  For a non-existant KEY nil is returned."
  `(let ((result (assoc ,key ,alist)))
     (setq ,alist (delete result ,alist))
     result))

;;;###autoload
(defmacro folio-assq-pop (key alist)
  "Pop element with key KEY from ALIST.

Return the first element of ALIST whose car is `eq' to KEY, and
delete it from the list.  For a non-existant KEY nil is returned."
  `(let ((result (assq ,key ,alist)))
     (setq ,alist (assq-delete-all ,key ,alist))
     result))

;;;###autoload
(defun folio-truncate-list (list n)
  "Truncate LIST to at most N elements destructively.
The argument is modified directly; the return value is undefined."
  (let ((here (nthcdr (1- n) list)))
    (when (consp here)
      (setcdr here nil))))

;;;###autoload
(defun folio-flatten (arg)
  "Recursively flatten ARG.
Return a list with the atoms in ARG at any level.  ARG need not
be a list, but also can be a cons cell with non-nil cdr, or a
primitive type, both then also coerced into a flat list."
  (cond
   ((null arg)
    nil)
   ((listp arg)
    (append (folio-flatten (car arg)) (folio-flatten (cdr arg))))
   (t (list arg))))


(defun folio-hash-md5 (buffer)
  "Return a MD5 message digest of the buffer contents of BUFFER."
  (if (fboundp 'secure-hash) ;; Emacs 24.
      (secure-hash 'md5 (current-buffer))
    (md5 (current-buffer))))


;;;###autoload
(defun folio-map-range (ra1 ra2 rb1 rb2 f)
  "Linearly map the number F from one into another range.
The interval [RA1, RA2] is the source range, [RB1, RB2] the
target range.  RA1, RA2, RB1, RB2 and F should be floating point
numbers."
  (+ (float rb1) (* (- (float f) ra1)
                    (/ (- rb2 (float rb1)) (- ra2 (float ra1))))))


;;;; cyclic state variables.

;;; XXX unused
(defun folio-cycle-state-scope (state item pred)
  (let ((last-scope (get state 'folio-cycle-scope))
        scope)
    (when (functionp pred)
      (setq scope (funcall pred item last-scope)))
    (put state 'folio-cycle-scope scope)
    scope))

;;;###autoload
(defun folio-cycle-state (state &optional item &rest keywords)
  "Advance the cyclic state variable STATE by one.

STATE should be the symbol of a non-empty sequence.  If item is
non-nil assume its position in STATE for the current state.
KEYWORDS are additional keyword parameters.  Return a cons of the
state index positions of the old and the new state.
\nKeywords supported:  :test :scope
\n(fn STATE [ITEM [KEYWORD VALUE]...])"
  (let ((num-states (length (symbol-value state)))
        current next test scope)
    (when keywords
      (while (keywordp (car keywords))
        (case (pop keywords)
          (:test (setq test (list :test (pop keywords))))
          (:scope (setq scope (pop keywords)))
          (t (error "Unknown keyword")))))
    (setq current (or (when item
                        (apply 'cl-position
                               item (symbol-value state) test))
                      (get state 'folio-cycle-state)
                      0))
    (setq next (% (+ current 1) num-states))
    (put state 'folio-cycle-state next)
    (cons current next)))


;;;; regions

(defun folio-join-regions (a b)
  "Join regions A and B.
Either region may be nil, and regions may intersect or be
adjoined; if separated the new region will include the gap.
Return the beginning and end positions of the new combined
region."
  (let ((a-beg (car a))
        (a-end (cdr a))
        (b-beg (car b))
        (b-end (cdr b)))
    (cond
     ((and a b)
      (if (>= (1+ a-end) b-beg)
          (list (cons a-beg
                      (if (> b-end a-end) b-end a-end)))
        (list a b)))
     ((not a) (list b))
     (t (append (list a) b)))))


;;;; text properties

;;;###autoload
(defun folio-propertize-region (beg end props
                                    &optional restrict fontify)
  "Mark a region by adding text properties.

BEG and END are buffer positions.  PROPS is plist with text
properties to add.  If RESTRICT is non-nil only propertize
matching stretches of text within the given region; RESTRICT
should be a regexp.  Search case-sensitivity is determined by the
value of the variable `case-fold-search', which see.  If FONTIFY
is non-nil additionally fontify the tagged text."
  (with-silent-modifications
    (if restrict
        (progn
          (goto-char beg)
          (while (re-search-forward restrict end t)
            (add-text-properties
             (match-beginning 0) (match-end 0) props)))
      (add-text-properties beg end props))
    (when fontify
      (font-lock-fontify-region beg end))))

(defun folio-unpropertize-region (beg end props
                                      &optional fontify)
  "Remove the text properties PROPS from the region BEG, END.

PROPS and FONTIFY have the same meaning as for
`folio-propertize-region', which see."
  (with-silent-modifications
    (remove-list-of-text-properties beg end props)
    (when fontify
      (font-lock-fontify-region beg end))))

(defun folio-next-property-bound (prop &optional pos)
  "Search for the next change to property PROP.
If POS is non-nil use that as a start position instead of the
current point.  Return a cons of change position and property
value."
  (or pos (setq pos (point)))
  (let ((change (next-single-char-property-change
                 pos prop)))
    (when change
      (when (cdr (setq change
                       (cons change (get-text-property
                                     change prop))))
        change))))

(defun folio-previous-property-bound (prop &optional pos)
  "Search for the previous change to property PROP.
If POS is non-nil use that as a start position instead of the
current point.  Return a cons of change position and property
value."
  (or pos (setq pos (point)))
  (let ((change (previous-single-char-property-change
                 pos prop)))
    (when change
      (when (cdr (setq change
                       (cons change (get-text-property
                                     change prop))))
        change))))

(defun folio-property-bounds (prop &optional pos which)
  "Return point positions of a region with constant property PROP.
If within a stretch of text with constant PROP return start and
end positions of that region or nil.

If the optional second argument POS is non-nil use that for
searching the boundaries instead.

If the optional third argument WHICH is the symbol 'begin or 'end
return either boundary."
  (let ((pos (or pos (point)))
        (limit (point-max))
        beg end)
    (when (get-text-property pos prop)
      (cond
       ((or (null which) (eq which 'any))
        (setq end (next-single-char-property-change
                   pos prop nil limit)
              beg (previous-single-char-property-change
                   end prop))
        (unless (and (= beg 1)
                     (= end limit))
          (cons beg (1- end))))
       ((eq which 'begin)
        (setq beg (previous-single-char-property-change
                   (if (= pos limit) pos (1+ pos)) prop))
        (unless (= beg 1)
          beg))
       ((eq which 'end)
        (setq end (next-single-char-property-change
                   pos prop nil limit))
        (when end
          (1- end)))
       (t
        (signal 'wrong-type-argument (cons which (type-of which)))
        nil)))))

;;;; motion


;;;###autoload
(defun folio-current-line ()
  "Return current line number of point.
For absolute line numbers, narrowing must not be in effect."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

;;;###autoload
(defsubst folio-goto-line (line)
  "Goto line LINE moving point.
Return the number of lines left to move."
  (goto-char (point-min))
  (forward-line (1- line)))

;;;###autoload
(defsubst folio-beginning-of-word-p ()
  "Move to the beginning of the word at point.
Return non-nil if point has moved.  See also
`folio-within-word-p'."
  (and (not (eq (char-syntax (or (char-before) ?\u0000)) ?w))
       (eq (char-syntax (or (char-after) ?\u0000)) ?w)
       t))

;;;###autoload
(defsubst folio-end-of-word-p ()
  "Return non-nil if point is at the end of a word.
See also `folio-within-word-p'."
  (and (eq (char-syntax (or (char-before) ?\u0000)) ?w)
       (not (eq (char-syntax (or (char-after) ?\u0000)) ?w))
       t))

;;;###autoload
(defsubst folio-within-word-p ()
  "Return non-nil if point is within a word.
A word is defined as any stretch of characters from the syntax
class of word constituents.  By the definition of this function
the return value is nil if point is before the first character of
a word but non-nil if it is at the last character."
  (eq (char-syntax (or (char-after) ?\u0000)) ?w))

;;;###autoload
(defun folio-beginning-of-word ()
  "Move to the beginning of the word at or before point.
Return non-nil if point has moved.  See also
`folio-within-word-p' and `folio-end-of-word-p'."
  (if (and (not (folio-beginning-of-word-p))
           (or (folio-within-word-p)
               (folio-end-of-word-p)))
      (and (forward-word -1) t)
    nil))

;;;###autoload
(defun folio-end-of-word ()
  "Move to the end of the word at point.
Return non-nil if point has moved.  See also
`folio-beginning-of-word'."
  (if (or (folio-within-word-p)
          (folio-beginning-of-word-p))
      (and (forward-word 1) t)
    nil))


;;;; search algorithms

;;; XXX TODO change those to accept &rest args, and pop keywords
;;; XXX TODO rework those into iterative versions
;;;###autoload
(defun* folio-upper-bound (value ordered-seq &key
                                 (value-extract #'identity))
  (folio-binary-search value ordered-seq
                       :value-extract value-extract
                       :upper-test #'<))

;;;###autoload
(defun* folio-lower-bound (value ordered-seq &key
                                 (value-extract #'identity))
  (folio-binary-search value ordered-seq
   :value-extract value-extract :upper-test #'<= :lower-test #'<))

;;;###autoload
(defun* folio-binary-search (value ordered-seq &key
                                   (value-extract #'identity)
                                   (lower-test #'<=)
                                   (upper-test #'<=))
  "Perform a binary search in a sequence."
  (folio-binary-search-internal value ordered-seq
                                0 (1- (length ordered-seq))
                                value-extract
                                lower-test upper-test))

;;;###autoload
(defun folio-binary-search-internal (value ordered-seq start end
                                           value-extract
                                           lower-test upper-test)
  (cond
   ((= (- end start) 1)
    (if (and (funcall lower-test
                      (funcall value-extract
                               (elt ordered-seq start)) value)
             (funcall upper-test value
                      (funcall value-extract
                               (elt ordered-seq end))))
        start)) ; return nil if termination condition fails
   (t
    (let ((middle (truncate (+ start end) 2)))
      (if (funcall upper-test value
                   (funcall value-extract
                            (elt ordered-seq middle)))
          (folio-binary-search-internal value ordered-seq start
           middle value-extract lower-test upper-test)
        (folio-binary-search-internal value ordered-seq middle end
         value-extract lower-test upper-test))))))


;;;; string helper

;;;###autoload
(defsubst folio-string-prefix-p (prefix s &optional ignore-case)
  "Return non-nil if PREFIX is a prefix of S.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.  Text properties are ignored.  See
also `folio-string-suffix-p'."
  (eq t (compare-strings prefix nil nil
                         s 0 (length prefix) ignore-case)))

;;;###autoload
(defun folio-string-suffix-p (suffix s &optional ignore-case)
  "Return non-nil if SUFFIX is a suffix of S.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences.  Text properties are ignored.  See
also `folio-string-prefix-p'."
  (let ((s-len (length s))
        (suffix-len (length suffix)))
    (when (>= s-len suffix-len)
      (eq t (compare-strings s (- s-len suffix-len) nil
                             suffix 0 suffix-len ignore-case)))))

(defun folio-cprintcharfun (c)
  "Print character function for `folio-cprinc', see which.

C should be an 8-bit character.  Return its string
representation."
  (cond
   ((>= c 127)
    (format "\\%03o" c))
   ((eq c ?\\)
    "\\")
   ((eq c ?\")
    "\\\"")
   ((eq c ?\')
    "\\'")
   ((>= c 32)
    (format "%c" c))
   ((eq c ?\a)
    "\\a")
   ((eq c ?\b)
    "\\b")
   ((eq c ?\f)
    "\\f")
   ((eq c ?\n)
    "\\n")
   ((eq c ?\r)
    "\\r")
   ((eq c ?\t)
    "\\t")
   ((eq c ?\v)
    "\\v")
   (t
    (format "\\%03o" c))))

(defun folio-cprinc (object)
  "Print a string representation of OBJECT.
This is similar to `princ' only that for a string valued OBJECT
non-printable characters are represented quoted, and 8bit
characters by their octal codes.  The function is most useful for
debugging."
  (if (and object (stringp object))
      (mapconcat #'folio-cprintcharfun (string-to-list object) "")
    (princ object)))

;;;###autoload
(defun folio-chomp (str)
  "Chomp leading and tailing whitespace from STR.
Match data is modified."
  ;; Stolen from EmacsWiki.
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'" str)
    (setq str (replace-match "" t t str)))
  str)

;;;###autoload
(defsubst folio-case-fold-string-equal (str1 str2)
  "Compare the contents of two strings STR1 and STR2 ignoring case.
Convert to multibyte if needed.  Return t if strings are equal,
or nil otherwise."
  (eq t (compare-strings str1 nil nil str2 nil nil 'ignore-case)))

;;;###autoload
(defalias 'folio-case-fold-string= 'folio-case-fold-string-equal
  "Alternative function name for `folio-case-fold-string-equal'.")

;;;###autoload
(defsubst folio-case-fold-string-hash (str)
  "Return a hash code for case folded string STR."
  (sxhash (upcase str)))

;;;###autoload
(defun folio-hash-table-to-alist (table)
  "Map the hash table TABLE into an alist."
  (let (alist)
    (maphash (lambda (k v)
               (setq alist
                     (cons (cons k v) alist))) table)
    alist))

;;;###autoload
(define-hash-table-test 'folio-case-fold-hash-table-test
  'folio-case-fold-string-equal 'folio-case-fold-string-hash)

;;;###autoload
(defsubst folio-lower-case-char-at-p (&optional pos)
  "Return t if the character after POS is in lowercase.
If POS is nil check the character at point instead."
  (let ((c (char-after (or pos (point)))))
    (eq c (char-table-range (current-case-table) c))))

;;;###autoload
(defsubst folio-upper-case-char-at-p (&optional pos)
  "Return t if the character after POS is in uppercase.
If POS is nil check the character at point instead."
  (not (folio-lower-case-char-at-p (or pos (point)))))

(defmacro folio-replace-regexp-in-string (str &rest clauses)
  "Replace all matches according to CLAUSES in string STR.

Each clause is of the form \(REGEXP REPLACEMENT):

   \(folio-replace-regexp-in-string s
      \(\"ß\" \"ss\")
      \(\"æ\" \"ae\")).

Clauses are applied in order with side-effects, i.e. the
replacements of a clause might get modified by a later clause.

Case in replacements is preserved if the variable `case-replace'
is non-nil.

Return a new string containing the replacements."
  (declare (indent 1))
  (let ((repl (make-symbol "repl")))
    `(let ((,repl ,str))
       ,@(let (forms)
           (mapc (lambda (x)
                   (push `(setq ,repl
                                (replace-regexp-in-string
                                 ,(car x) ,(cadr x) ,repl
                                 (not case-replace)))
                         forms))
                 (nreverse clauses))
           forms))))

(defun folio-lrpad-string (str width)
  "Return the string STR left and right padded to the width WIDTH
using spaces."
  (let* ((len (length str))
         (lpad (+ len (/ (- width len) 2))))
    (format (format "%%-%ds" width)
            (format (format "%%%ds" lpad) str))))


;;;; windows

;;;###autoload
(defun folio-copy-other-window (beg end)
  "Copy region text or word to buffer in other window.
Text properties are not retained."
  (interactive "r")
  (let ((s (if (use-region-p)
               (buffer-substring-no-properties beg end)
             (current-word nil 'really-word)))) ;; not symbol
    (if s
        (progn
          ;; XXX TODO not just other, but also dedicated, supported by
          ;; a history variable
          (other-window 1)
          (insert s))
      (folio-user-error "No word nearby"))))


(defun folio-regexp-valid-p (regexp)
  "Return non-nil if the regexp REGEXP syntactically is valid."
  (and (stringp regexp) (condition-case nil
                            (prog1 t
                              (string-match regexp ""))
                          (error nil))))


;;;; pilot-machine interface

;;;###autoload
(defmacro folio-with-muted-message (&rest body)
  "Redefine `message' to be silent.
Eval BODY forms sequentially and return value of last one.  Upon
return restore the normal behaviour of `message'."
  (declare (indent 1))
  `(let ((save-message (symbol-function 'message)))
     (fset 'message (lambda (format-string &rest args) nil))
     (unwind-protect
         (progn ,@body)
       (fset 'message save-message))))


;;;; maintaining undo

(defmacro folio-with-disabled-undo (&rest body)
  "Disable undo recording in current buffer; execute BODY.
BODY is executed just like `progn'.

Eval BODY forms sequentially and return value of last one.  Upon
return restore the normal undo recording and history."
  (declare (indent 0) (debug t))
  `(let ((save-undo-list (symbol-value 'buffer-undo-list)))
     (set 'buffer-undo-list t)
     (unwind-protect
         (progn ,@body)
       (set 'buffer-undo-list save-undo-list))))


;;;; external processes

(defsubst folio-process-running-p (process)
  "Return t if PROCESS is running."
  (eq 'run (process-status process)))


(provide 'folio-atoms)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-atoms.el ends here
