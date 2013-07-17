;;; folio-things.el --- `thing-at-point' things for Folio mode

;; Copyright (C) 2012, 2013  Christoph W. Kluge

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

(eval-when-compile (require 'cl))

(require 'thingatpt)

(require 'folio-atoms)

(defconst folio-footnote-signs '(?* ?† ?‡ ?§ ?‖ ?¶)
  "Typographical devices for footnotes.
In traditional order of application these are the asterisk,
U+002A, the dagger or obelisk, U+2020, the double dagger or
diesis, U+2021, the section sign, U+00A7, the double vertical
line or bar, U+2016, the pilcrow, paragraph sign, or alinea,
U+00B6.")

(defvar folio-auto-thing-function nil
  "Mode-specific function to return a `thing-at-point', or nil if
there is none.  The return value must be a symbol \(not a
region).")
(make-local-variable 'folio-auto-thing-function)

(defvar folio-auto-style-function nil
  "Mode-specific function to return a style for transforming a
`thing-at-point', or nil if there is none.  The return value must
be a symbol \(not a region).")
(make-local-variable 'folio-auto-style-function)

(defconst folio-dash '("-" "‐" "--" "–" "----" "—")
  "*List of dash-like glyphs.
This variable lists dash-like glyphs in particular hyphen-minus,
minus, en-dash, em-dash glyphs and their equivalents.")

(put 'folio-dash 'desc '("HYPHEN-MINUS" "HYPHEN U+2010" "EN DASH \
equivalent" "EN DASH U+2013" "EM DASH equivalent" "EM DASH \
U+2014"))

(defconst folio-ellipsis '("." "..." "…" "⋯" "....")
  "*List of ellipsis-like glyphs.
This variable lists ellipsis-like glyphs, in particular the
period itself, the canonical-ellipsis glyphs HORIZONTAL ELLIPSIS
#x2026, the MIDLINE HORIZONTAL ELLIPSIS #x22ef, and their
equivalents using un-spaced periods.  The FOUR DOT ELLIPSIS is a
contraction of the horizontal ellipsis and a sentence ending
period; it is not defined by Unicode.")

(put 'folio-ellipsis 'desc '("FULL STOP" "HORIZONTAL ELLIPSIS \
equivalent" "HORIZONTAL ELLIPSIS U+2026" "MIDLINE HORIZONTAL \
ELLIPSIS U+22EF" "FOUR DOT ELLIPSIS"))

;;;###autoload
(defun folio-cycle-dash ()
  "Replace a dash-like glyph at point with an alternative.
Alternatives include canonical dash glyphs as well as
equivalents made up from HYPHEN-MINUS.  Repeated
execution of this command at point switches to the next
alternative.  If called from Lisp return non-nil if a
replacement has happened."
  (interactive)
  (save-excursion
    (save-match-data
      ;; The character set here and below should be computed from the
      ;; length-1 strings in `folio-dash'.
      (while (and (not (bobp))
                  (member (char-before) '(?- #x2010 #x2013 #x2014)))
        (backward-char))
      (when (looking-at "[-‐–—]+"))
      (let ((state (folio-cycle-state
                    'folio-dash (match-string 0) :test #'string=)))
        (replace-match (elt folio-dash (cdr state)))
        (if (called-interactively-p 'any)
            (message (elt (get 'folio-dash 'desc) (cdr state)))
          t)))))

;;;###autoload
(defun folio-cycle-ellipsis ()
  "Replace an ellipsis-like glyph at point with an alternative.
Alternatives include canonical ellipsis glyphs as well as
equivalents made up from full stop characters.  Repeated
execution of this command at point switches to the next
alternative.  Retain leading and trailing whitespace but remove
inter-dot space.  If called from Lisp return non-nil if a
replacement has happened."
  (interactive)
  (save-excursion
    (save-match-data
      ;; The character set here and below should be computed from the
      ;; length-1 strings in `folio-ellipsis'.
      (while (and (not (bobp))
                  (member (char-before) '(?\s ?\. #x2026 #x22ef)))
        (backward-char))
      ;; Exclude leading space from replacement...
      (while (eq (char-after) ?\s)
        (forward-char))
      (when (looking-at "\\([.…⋯]+\\(?: ?[.…⋯]\\)*\\)")
        (let ((state (folio-cycle-state
                      'folio-ellipsis (match-string 1) :test #'string=)))
          (replace-match
           (elt folio-ellipsis (cdr state)) nil 'literal nil 1)
          (if (called-interactively-p 'any)
            (message (elt (get 'folio-ellipsis 'desc) (cdr state)))
            t))))))

;;;###autoload
(defun folio-thing-at-point-p (thing)
  "Predicate returning t if THING is defined as a `thing-at-point'
type, or nil otherwise."
  (and (symbolp thing)
       (or (functionp (or (get thing 'forward-op)
                          (intern-soft (format "forward-%s" thing))))
           (and (functionp (get thing 'beginning-op))
                (functionp (get thing 'end-op)))
           (functionp (get thing 'bounds-of-thing-at-point)))))

;;;###autoload
(defun folio-looking-at-thing-p (thing &optional arg)
  "Return THING if text after point matches a `thing-at-point' of
type THING.  Otherwise return nil.  THING normally is a symbol
defining a suitable forward movement function.  It also can be a
list of THING symbols in which case tests are performed in order.
Point must be positioned at the beginning of THING for the test
to succeed.  With non-nil prefix argument ARG look backward
instead."
  (let ((things (cond
                 ((listp thing)
                  thing)
                 ((symbolp thing)
                  (list thing))
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing))))))
        pred)
    (save-match-data
      (while (and (not pred) things)
        (let ((bounds (bounds-of-thing-at-point (car things))))
          (if (and bounds (eq (if arg
                                  (cdr bounds)
                                (car bounds)) (point)))
              (setq pred (car things))
            (pop things)))))
    pred))

;;;###autoload
(defsubst folio-looking-back-at-thing-p (thing &optional arg)
  "Return THING if text before point matches a `thing-at-point'
of type THING, or nil otherwise.  THING normally is a symbol
defining a suitable forward movement function.  It also can be a
list of THING symbols in which case tests are performed in order.
Point must be positioned at the end of THING for the test to
succeed.  With non-nil prefix argument ARG look forward instead."
  (folio-looking-at-thing-p thing (not arg)))

;;;###autoload
(defun folio-check-parens (&optional arg)
  (interactive "P")
  (let ((pos (if arg (1+ (point)) (point-min)))
        (parse-sexp-ignore-comments t))
    (condition-case data
        ;; Buffer can't have more than (buffer-size) sexps.
        (scan-sexps pos (buffer-size))
      (scan-error (goto-char (nth 2 data))
                  (when (called-interactively-p 'any)
                    (error "Unmatched parentheses bracket"))))))

;;;###autoload
(defun folio-point-inside-pairs-p (&optional ppss)
  "Return t if point is inside any pairs of the parenthesis syntax
class, or nil otherwise.  This function does not consider unbalanced
parentheses."
  (setq ppss (or ppss (syntax-ppss)))
  (let ((depth (car ppss)))
    (cond
     ((> depth 0)
      t)
     ((< depth 0)
      (save-excursion
        (not (eq (car (syntax-ppss (nth 1 ppss))) depth))))
     (t nil))))

;;;###autoload
(defun folio-region-or-thing (thing)
  "Return THING or region at point.

The result is a consp of the form (TEXT (START . END)) containing
the region and its bounds if there is a region active and it's
appropriate to act on it, or THING in the same form.  TEXT is
returned with properties."
  (if (region-active-p)
      (cons (buffer-substring (region-beginning) (region-end))
            (cons (region-beginning) (region-end)))
    (let ((bounds (bounds-of-thing-at-point thing)))
      (and bounds
           (cons (buffer-substring (car bounds) (cdr bounds)) bounds)))))

;;;###autoload
(defun folio-mark-thing-at-point (thing &optional arg allow-extend)
  "Set mark ARG things away from point.

The place mark goes is the same place `forward-thing' would move
to with the same argument.  THING is a symbol identifying a
`thing-at-point' including 'paragraph, 'line, '`folio-footnote',
'`folio-sidenote', etc.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it extends the selection to include
the next ARG THINGs."

;  (interactive "i\nP\np")

  (when (and arg (zerop arg))
    (error "Cannot mark zero things"))

  (let ((regionp mark-active))
    (cond ((and allow-extend
                (or (and (eq last-command this-command) (mark t))
                    (and transient-mark-mode regionp)))
           ;; Extend existing selection.
           (setq arg (if arg
                         (prefix-numeric-value arg)
                       (if (< (mark) (point)) -1 1)))
           (set-mark (save-excursion (goto-char (mark))
                                     (forward-thing thing arg)
                                     (point))))
          (t
           ;; Set mark to the end (if ARG > 0) or beginning (if ARG <
           ;; 0) of THING at occurrence ARG.
           (push-mark (save-excursion
                        (forward-thing thing (prefix-numeric-value arg))
                        (point))
                      nil t)))
    (unless regionp
      (let ((bounds (bounds-of-thing-at-point thing)))
      ;; Unless extending an existing region and if within THING,
      ;; extend the region backward (or forward if ARG < 0) up to its
      ;; beginning (or end if ARG < 0), to mark THING in its
      ;; boundaries.
      (unless (or (not bounds) (= (point) (car bounds)))
        (forward-thing thing (if (< (mark) (point)) 1 -1))))))
  ;; retain mark upon return
  (setq deactivate-mark nil))

;;;###autoload
(defun folio-unfold-thing-at-point (thing)
  "Unfold a thing by deleting new-line characters.
THING either is a symbol of a `thing-at-point' or a cons of the
left and right boundaries of THING.  The text stretch of THING is
assumed to be normalized to contain LF characters only (not CRs).
The replacement character is SP.  Successive occurences of LF
characters will be replaced by single SP character.

The return value is a cons describing the boundaries of THING."

  (interactive) ; XXX ARGS
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "No recognized thing at point"))
    (save-excursion
      (goto-char (car thing))
      (while (re-search-forward "\n+" (cdr thing) t)
        (setcdr thing (- (cdr thing)
                         (- (match-end 0) (match-beginning 0) 1)))
        (replace-match " " nil t)))
    thing))

(defun folio-strip-thing-at-point (thing prefix &optional postfix)
  "Strip text from either end of a thing.
THING either is a symbol of a `thing-at-point' or a cons of the
left and right boundaries of THING.  Both PREFIX and the optional
argument POSTFIX should be a string or a regexp."
  (interactive) ; XXX ARGS
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "No recognized thing at point"))
    (save-excursion
      (when prefix
        (goto-char (car thing))
        (when (and (re-search-forward prefix (cdr thing) t)
                   (= (match-beginning 0) (car thing)))
          (setcdr thing (- (cdr thing)
                           (- (match-end 0)
                              (match-beginning 0))))
          (delete-region (match-beginning 0) (match-end 0))))
      (when postfix
        (goto-char (cdr thing))
        (when (and (re-search-backward postfix (car thing) t)
                   (= (match-end 0) (cdr thing)))
          (setcdr thing (point))
          (delete-region (match-beginning 0) (match-end 0)))))
    thing))

;;;###autoload
(defun folio-pad-thing-at-point (thing lpad &optional rpad)
  "Apply left or right padding to THING.
The first argument THING either is a symbol of a `thing-at-point'
or a cons cell describing the boundaries of THING.  For left
padding the second argument LPAD must be a string or a character;
correspondingly the optional third argument RPAD is used for
right padding.  Multiple occurrences of the first character of
LPAD before THING automatically are collapsed into a single
instance of this character; with right padding the last character
of RPAD is collapsed.

Note that the paddings do not inherit any properties from THING,
i.e. padding text is not considered part of the thing.

The return value is a cons of start and end positions of THING
\(excluding any padding)."

  (interactive) ; XXX args
  (let ((thing (cond
                ((folio-thing-at-point-p thing)
                 (bounds-of-thing-at-point thing))
                ((consp thing)
                 thing)
                (t
                 (signal 'wrong-type-argument
                         (cons thing (type-of thing)))))))
    (unless thing
      (error "No recognized thing at point"))

    (save-excursion
      (when lpad
        (let* ((collapse (if (characterp lpad) lpad (aref lpad 0)))
               (case-fold-search nil) ; for char-equal
               (end (cdr thing))
               (end-marker (progn
                             (goto-char end)
                             (copy-marker (point-marker)))))
          (goto-char (car thing))
          (while (char-equal (char-before (point)) collapse)
            (delete-char -1))
          (insert lpad)
          (setq thing (cons (+ (car thing)
                               (- end-marker end))
                            (marker-position end-marker)))
          (set-marker end-marker nil)))
      (when rpad
        (let ((collapse (if (characterp rpad)
                            rpad
                          (aref rpad (1- (length rpad)))))
              (case-fold-search nil)) ; for char-equal
          (goto-char (cdr thing))
          (insert rpad)
          (while (char-equal (char-after) collapse)
            (delete-char 1)))))
    thing))

;;;###autoload
(defun folio-indent-thing-at-point (thing indent &optional hang)
  "XXX"
  (interactive) ; XXX args
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                ((consp thing)
                 thing)
                (t
                 (signal 'wrong-type-argument
                         (cons thing (type-of thing))))))
         (hang (cond
                ((null hang)
                 hang)
                ((integerp hang)
                 (list "^." hang 1))
                ((and (stringp hang) (> (length hang) 0))
                 (list (if (eql (aref hang 0) ?^)
                           hang
                         (concat "^" hang)) nil 1))
                ((consp hang)
                 (cond
                  ((and (integerp (car hang))
                        (integerp (cdr hang)))
                   (list "^." (car hang) (cdr hang)))
                  ((and (and (stringp (car hang))
                             (> (length (car hang)) 0))
                        (integerp (cdr hang)))
                   (list (if (eql (aref (car hang) 0) ?^)
                             (car hang)
                           (concat "^" (car hang))) nil (cdr hang)))
                  (t
                   (signal 'wrong-type-argument
                           (cons hang (type-of hang))))))
                (t
                 (signal 'wrong-type-argument
                         (cons hang (type-of hang)))))))
    (unless thing
      (error "No recognized thing at point"))

    (when hang
      (let* ((bounds (folio-indent-thing-at-point thing nil))
             (end (save-excursion
                    (goto-char (cdr bounds))
                    (folio-current-line)))
             (start (save-excursion
                      (goto-char (car bounds))
                      (beginning-of-line)
                      (let (candidate)
                        (while (and (null candidate)
                                    (<= (folio-current-line) end))
                          (when (looking-at (car hang))
                            (unless (integerp (cadr hang))
                              (setcar (cdr hang) (- (- (match-end 0)
                                                       (match-beginning 0)))))
                            (setq candidate (folio-current-line)))
                          (forward-line))
                        candidate))))
        ;; (message "XXX hang %s start %s end %s indent %s count %s" hang start end (cadr hang) (cl-caddr hang))
        (unless start
          (error "Line initiating hanging indent not found: spec %s" hang))

        (save-excursion
          (goto-char (car bounds))
          (beginning-of-line)
          (setcar bounds (point))
          (while (< (folio-current-line) start)
            (forward-line))
          ;; (message "XXX 1 %s count %s" (folio-current-line) (cl-caddr hang))
          (let ((indent (cadr hang))
                (count (max (caddr hang) 0)))
            (if (> indent 0)
                (progn
                  (while (and (> count 0) (<= (folio-current-line) end))
                    ;; (message "XXX indent -> %s" indent)
                    (indent-to indent)
                    (forward-line)
                    (setq count (1- count)))
                  (while (< (folio-current-line) end)
                    (forward-line)))
              (while (and (> count 0) (<= (folio-current-line) end))
                (forward-line)
                (setq count (1- count)))
              (while (<= (folio-current-line) end)
                (indent-to (- indent))
                (forward-line))))
          (end-of-line)
          (setcdr bounds (point)))
        ;; (message "XXX goo %s %s" bounds end)
        (setq thing bounds)))

    (cond
     ((or (null indent) (integerp indent))
      (save-excursion
        (goto-char (car thing))
        (beginning-of-line)
        (setcar thing (point))
        (let ((start (car thing)))
          (goto-char (cdr thing))
          (unless hang
            (indent-rigidly start (point) most-negative-fixnum))
          (when indent
            (indent-rigidly start (point) indent))
          (end-of-line)
          (setcdr thing (point)))))
      ((stringp indent)
       (when (string-match-p ".*[\n\r]" indent) ; XXX move out & up
         (error "Indent string must not contain newline characters --%s--" indent))
       ;; (message "XXX %s" thing)
       (save-excursion
         (goto-char (car thing))
         (beginning-of-line)
         (setcar thing (point))
         (let ((start (point))
               (delta 0))
           (goto-char (cdr thing))
           ;; (message "XXX folio-indent uuu %d %d" (point) start)
           (while (>= (point) start)
             (beginning-of-line)
             ;; (message "XXX folio-indent p %d" (point))
             ;; Unless at first or last line of thing and looking at
             ;; newline
             (unless (and (eql (char-after) ?\n)
                          (or (= (point) start)
                              (zerop delta)))
               (insert indent)
               ;; (message "XXX folio-indent III %s" indent)
               (setq delta (1+ delta)))
             (forward-line -1))
           (setcdr thing (+ (cdr thing)
                            (* delta (length indent)))))))
      (t
       (signal 'wrong-type-argument
               (cons indent (type-of indent)))))
    thing))

;(defun canonically-space-region (beg end)
; XXX apply style: let fill-column
; XXX kinsoku processing--use with the dreaded ", I\n"?
; XXX drop the hang, use fill-individual-varying-indent
; XXX justify with `default-justification'/`current-justification'
;;;###autoload
(defun folio-fill-thing-at-point (thing &optional justify hang)

  (interactive) ; XXX args
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "Unrecognized thing at point"))

    ;; if hang -1 compute number of lines in thing
    (save-excursion
      (let ((end (progn
                   (goto-char (cdr thing))
                   (copy-marker (point-marker))))
            (beg (car thing)))
           (fill-region beg end justify)
           (setq thing (cons beg (marker-position end)))
           (set-marker end nil)))
    thing))

;; collapse "[^\\.] \\( +\\)"

;;;###autoload
(defun folio-unfill-thing-at-point (thing)

  (interactive) ; XXX args
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "Unrecognized thing at point"))

    (save-excursion
      (let ((end (progn
                   (goto-char (cdr thing))
                   (copy-marker (point-marker))))
            (beg (car thing)))
        (goto-char (car thing))
        ;; retain newlines from paragraph separators
        (while (re-search-forward ".\\(\n\\)\\([^\n]\\)"
                                  (marker-position end) t)
          (if (eq (char-after (match-beginning 2)) ?\ )
              (replace-match "" nil nil nil 1)
            (replace-match " " nil nil nil 1)))
         (setq thing (cons beg (marker-position end)))
         (set-marker end nil)))
    thing))

;;;###autoload
(defun folio-canonically-space-thing-at-point (thing)
  "Remove extra spaces between words.
In the region occupied by THING leave one space between words,
two at end of sentences or after colons \(depending on values of
`sentence-end-double-space', `colon-double-space', and
`sentence-end-without-period').  Remove indentation from each
line."
  (interactive) ; XXX args
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "Unrecognized thing at point"))

    (save-excursion
      (let ((end (progn
                   (goto-char (cdr thing))
                   (copy-marker (point-marker))))
            (beg (car thing)))
           (canonically-space-region beg end)
           (setq thing (cons beg (marker-position end)))
           (set-marker end nil)))
    thing))

;;;###autoload
(defun folio-transform-thing-at-point (thing regexp to-string
                                             &optional repeat fixed-case literal)
  "Transform THING using REGEXP search and replace with TO-STRING.
THING either is a symbol identifying a `thing-at-point' or a cons
cell with the buffer start and end positions (inclusive) of
THING.  If the optional third argument REPEAT is non-nil search
and replace all occurences within the boundaries of THING, or
only the first occurence.  Search case-sensitivity is determined
by the value of the variable `case-fold-search', which see.  If
the fourth optional argument FIXED-CASE is non-nil, retain case
of replacement text.  Otherwise maybe capitalize the whole text,
or maybe just word initials, based on the replaced text.  If the
fifth optional argument is non-nil TO-STRING is inserted
literally.  Otherwise treat `\' as special:
  `\&' in NEWTEXT means substitute original matched text.
  `\N' means substitute what matched the Nth `\(...\)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `\'.
Case conversion does not apply to these substitutions."
  (interactive) ; XXX args
  (let* ((thing (cond
                 ((folio-thing-at-point-p thing)
                  (bounds-of-thing-at-point thing))
                 ((consp thing)
                  thing)
                 (t
                  (signal 'wrong-type-argument
                          (cons thing (type-of thing)))))))
    (unless thing
      (error "Unrecognized thing at point"))

    (save-excursion
      (let ((end (progn
                   (goto-char (cdr thing))
                   (copy-marker (point-marker))))
            (beg (car thing))
            (count (or (and repeat most-positive-fixnum) 1)))
        (goto-char beg)
        (while (and (> count 0) (re-search-forward regexp end t))
          (replace-match to-string fixed-case literal))
        (setq thing (cons beg (marker-position end)))
        (set-marker end nil)))
    thing))

;;;###autoload
(defun folio-transpose-thing-at-point (thing other
                                             &optional nth precede)
  "Transpose two things.

With the `thing-at-point' THING and OTHER move THING to follow
OTHER and return the boundaries of THING in the new location on
success, or nil if OTHER is not found.  OTHER can also be a
marker or a consp defining a buffer region, or a buffer position
such as `mark'.  By repeatedly calling this function with the
return value of a previous call, a sequence of THINGs can easily
be reordered or moved to a new page location further up or down.

Except for point, marker or consp types of OTHER, the optional
third argument NTH can be used to select the nth occurence of
OTHER instead of the next following point.  For NTH > 0 the
search for OTHER is forward, or backward (NTH < 0) otherwise.

If the optional fourth argument PRECEDE is non-nil THING is moved
to directly precede (the NTH occurence of) OTHER.  This is a
legal operation even if THING already precedes OTHER (only
further up), i.e. effectively a double transposition.

THING may be completely contained by OTHER, any other
intersection of the two things including a shared boundary will
be rejected by raising an error, though.

If THING is moved its text properties are preserved.

Note that THING if moved will always directly precede or follow
OTHER; if any kind of padding is required to separate the two
things, `folio-pad-thing-at-point' can be used, or a simple
`insert' (see which) at either of the new boundaries."
  (setq nth (or nth 1))
  (when (zerop nth)
    (error "Argument must be less than or greater than zero"))

  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point thing))
           (bounds-other (cond
                          ((folio-thing-at-point-p other)
                           (when bounds
                             (save-excursion
                               (goto-char (if (< nth 0)
                                              (car bounds)
                                            (cdr bounds)))
                               (forward-thing other nth)
                               (bounds-of-thing-at-point other))))
                          ((markerp other)
                           (let ((pos (marker-position other)))
                             (cons pos pos)))
                          ((consp other)
                           other)
                          ((integerp other)
                           (cons other other))
                          (t
                           (signal 'wrong-type-argument
                                   (cons other (type-of other))))))
          (beg (car bounds))
          (end (cdr bounds))
          (beg-other (car bounds-other))
          (end-other (cdr bounds-other)))
      (if (and bounds bounds-other)
          (let ((thing (buffer-substring beg end))
                (zwidth-other-p (= beg-other end-other)))
            (cond
             ((or (<= end-other beg)
                  (and zwidth-other-p (= beg end-other)))
              ;; thing follows other
              (delete-region beg end)
              (if precede
                  (progn
                    (goto-char beg-other)
                    (insert thing)
                    (setq bounds (cons beg-other (point))))
                (goto-char end-other)
                (unless zwidth-other-p
                  (forward-char))
                (setq bounds (cons (point)
                                   (+ end-other (- end beg))))
                (insert thing)))
            ((or (<= end beg-other)
                 (and zwidth-other-p (= end beg-other)))
             ;; thing precedes other
             (if precede
                 (progn
                   (goto-char beg-other)
                   (insert thing)
                   (setq bounds (cons beg-other (point))))
               (goto-char end-other)
               (unless zwidth-other-p
                 (forward-char))
               (setq bounds (cons (point) (- end beg)))
               (insert thing))
             (delete-region beg end))
            ((or (and (>= beg beg-other) (< end end-other))
                 (and (> beg beg-other) (<= end end-other)))
             ;; thing contained by other
             (if precede
                 (progn
                   (delete-region beg end)
                   (goto-char beg-other)
                   (insert thing)
                   (setq bounds (cons beg-other (point))))
               (goto-char end-other)
               (unless zwidth-other-p
                 (forward-char))
               (setq bounds (cons (point)
                                  (+ (point) (- end beg))))
               (insert thing)
               (delete-region beg end)))
            ((equal bounds bounds-other)
              ;; complete overlap; probably a logic error elsewhere
              (error "Things occupying the same position: %d, %d"
                     beg end))
            (t
             ;; partial intersection
             (error "Things in partial intersection: %d %d" beg end))))
        (if (null bounds)
            (message "No %s at point" (downcase (symbol-name thing)))
          (when (null bounds-other)
            (message "Other thing not found")))
        (setq bounds nil))
      bounds)))

;;;###autoload
(defun folio-kill-thing-at-point (thing &optional verb)
  "Kill THING at point adding it to the kill-ring.
THING must be a symbol defining a `thing-at-point' such as
`folio-proofer-note'."
  (unless (symbolp thing)
    (error "Argument must be a symbol"))

  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (when verb
        (message "No %s at point to kill" (downcase (symbol-name thing)))))))

;;;###autoload
(defun folio-style-thing-at-point (&optional thing style)
  "Apply transformations to a `thing-at-point' as defined by a
given style."
  (interactive "SThing: \nSStyle: ")
  (let* ((thing (or thing
                    (and (functionp folio-auto-thing-function)
                         (funcall folio-auto-thing-function))))
         (style (or style
                    (and (functionp folio-auto-style-function)
                         (funcall folio-auto-style-function thing))))
         (debug-on-error t))

    (unless (and thing style)
      (error "Unrecognized thing at point or unknown style"))

    (mapc (lambda (x)
            (let* ((x (if (listp x) x (list x)));; (x (folio-flatten x))
                   (args (cdr x))
                   (func (car x)))
           ;;   (message "XXX %S style func %s (%s)" x func args)
              (cond
               ((eq func 'unfill)
                (setq thing (folio-unfill-thing-at-point thing)))
               ((eq func 'canonify-spaces)
                (let ((sentence-end-double-space
                       (and (memq 'sentence-end-double-space args) t))
                      (colon-double-space
                       (and (memq 'colon-double-space args) t))
                      (sentence-end-without-period
                       (and (memq 'sentence-end-without-period args) t)))
                  (setq thing
                        (folio-canonically-space-thing-at-point thing))))
               ((eq func 'fill)
                (message "XXX fill %s %s" (car args) (cadr args))
                (let ((fill-column (car args))
                      (fill-individual-varying-indent (> (cadr args) 0)))
                  (setq thing (folio-fill-thing-at-point thing))))
               ((eq func 'strip)
                (setq thing
                      (folio-strip-thing-at-point
                       thing (car args) (cadr args))))
               ((eq func 'pad)
                (setq thing (folio-pad-thing-at-point
                             thing (car args) (cadr args))))
               ((eq func 'indent)
                (message "XXX indent %s %s" (car args) (cadr (folio-flatten args)))
                (setq thing (folio-indent-thing-at-point
                             thing (car args) (cadr (folio-flatten args)))))
               ((eq func 'transpose)
                nil)
               ((eq func 'transform)
                (message "XXX args %s car %s cdr %s" args (car args) (cdr args))
                (let ((regexp (caar args))
                      (to-string (cl-caadr args))
                      (repeat (and (memq 'repeat (cl-cadar args)) t))
                      (case-fold-search
                       (and (memq 'case-fold-search (cl-cadar args)) t))
                      (fixed-case
                       (and (memq 'fixed-case (cl-cadadr args)) t))
                      (literal (and (memq 'literal (cl-cadadr args)) t)))
                  (message "XXX regex %s to-string %s repeat %s case-fold %s fixed-case %s literal %s"
                           regexp to-string repeat case-fold-search fixed-case literal)
                  (setq thing
                        (folio-transform-thing-at-point
                         thing regexp to-string repeat fixed-case literal))))
               (t
                (error "Unrecognized style function: %s" func)))))
          (cl-caddr style))
    thing))

;;;###autoload
(defun folio-find-things (thing) ;; XXX bounds, transform-callback
  "Return a list of `thing-at-point' THINGs.
The symbol THING identifies the type of things to search.  Search
starts at point and always is forward.

The result is a list of conses of the buffer start and end
positions of a THING sorted ascending."
  (save-excursion
    (forward-thing thing -1)
    (let ((bounds (bounds-of-thing-at-point thing)))
      (while (car bounds)
        (forward-thing thing -1)
        (push (bounds-of-thing-at-point thing) bounds)
        (when (equal (car bounds) (cadr bounds))
          (setcar bounds nil))
      (cdr bounds)))))


(provide 'folio-things)

;;; folio-things.el ends here
