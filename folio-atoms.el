;;; folio-atoms.el --- Folio mode basic building blocks

;; Copyright (C) 2012, 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines basic building blocks not related to any
;; particular functionality.  Functions include operations on lists
;; and strings, buffer motion, and other auxiliary utilities.

;;; Code:

(eval-when-compile
  (require 'cl))

;; Randomize the seed in the random number generator.
(random t)

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
  "From ALIST pop element with key KEY.
Return the first element of ALIST whose car is `equal' to KEY, and
delete it from the list.  For a non-existant KEY nil is returned."
  `(let ((result (assoc ,key ,alist)))
     (setq ,alist (delete result ,alist))
     result))

;;;###autoload
(defmacro folio-assq-pop (key alist)
  "From ALIST pop element with key KEY.
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

;;;###autoload
(defun folio-map-range (ra1 ra2 rb1 rb2 f)
  "Linearly map the number F from one into another range.
The interval [RA1, RA2] is the source range, [RB1, RB2] the
target range.  RA1, RA2, RB1, RB2 and F should be floating point
numbers."
  (+ (float rb1) (* (- (float f) ra1)
                    (/ (- rb2 (float rb1)) (- ra2 (float ra1))))))

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
non-nil assume its position in STATE for the current state.  KEYS
are additional keyword parameters.  Return a cons of the state
index positions of the old and the new state.
\nKeywords supported:  :test :scope
\n(fn STATE [ITEM [KEYWORD VALUE]...])"
  (let ((num-states (length (symbol-value state)))
        current next test scope)
    (when keywords
      (while (keywordp (car keywords))
        (pcase (pop keywords)
          (`:test (setq test (list :test (pop keywords))))
          (`:scope (setq scope (pop keywords)))
          (_ (error "unknown keyword")))))
    (setq current (or (when item
                        (apply 'cl-position
                               item (symbol-value state) test))
                      (get state 'folio-cycle-state)
                      0))
    (setq next (% (+ current 1) num-states))
    (put state 'folio-cycle-state next)
    (cons current next)))


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

;;; XXX TODO change those to accept &rest args, and pop keywords
;;;###autoload
(defun* folio-upper-bound (value ordered-seq &key
                                 (value-extract #'identity))
  (folio-binary-search value ordered-seq
                       :value-extract value-extract
                       :upper-test '<))

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
(defun folio-binary-search-internal
  (value ordered-seq start end
         value-extract lower-test upper-test)
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

;;;###autoload
(defsubst folio-starts-with-p (s beginning)
  "Return t if string S starts with BEGINNING.
S or BEGINNING also can be symbols in which case their print
names are compared.  Text properties are ignored.  See also
`folio-ends-with-p'."
  (cond ((>= (length s) (length beginning))
         (string-equal (substring s 0 (length beginning)) beginning))
        (t nil)))

;;;###autoload
(defsubst folio-ends-with-p (s ending)
  "Return t if string S ends with ENDING.
S or ENDING also can be symbols in which case their print names
are compared.  Text properties are ignored.  See also
`folio-starts-with-p'"
  (string-equal (substring s (- (length ending))) ending))

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
(define-hash-table-test 'folio-case-fold-hash-table-test
  'folio-case-fold-string-equal 'folio-case-fold-string-hash)

;;;###autoload

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
      (user-error "No word nearby"))))


;;;###autoload
(defmacro folio-with-muted-message (&rest body)
  "Redefine `message' to be silent.
Eval BODY forms sequentially and return value of last one.  Upon
return restore the normal behaviour of `message'."
  `(let ((save-message (symbol-function 'message)))
     (fset 'message (lambda (format-string &rest args) nil))
     (unwind-protect
         (progn ,@body)
       (fset 'message save-message))))

(defsubst folio-process-running-p (process)
  "Return t if PROCESS is running."
  (eq 'run (process-status process)))

(provide 'folio-atoms)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-atoms.el ends here
