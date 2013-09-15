;;; folio-segmentation.el --- Text segmentation for Folio mode

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

;; This package provides functionality for the identification of a
;; text's structure and macro typographic elements such as pages,
;; chapters, sections, illustrations, side-notes, footnotes,
;; block-quotes (wrapable), or no-wrap paragraphs including poetry.

;;; Code:

(require 'folio-atoms)
(require 'folio-babel)
(require 'folio-core)
(require 'folio-things)

; XXX:TODO query custom value, hook into before-save

(defun folio-scan-page-separators (&optional buffer)
  "Scan page separators from text file.
BUFFER is the buffer or buffer name to scan.  If omitted this
defaults to the current buffer.

Once page information is available the physical page number is shown in
the header line, references to page scan images become available, and
the proofer names for the current page can be queried."
  (interactive)
  (let ((buffer (get-buffer (or buffer (current-buffer))))
        (page 0)
        (proofer #x20)
        page-markers page-scans proofer-names page-proofers)
    (setq proofer-names (make-hash-table :test #'equal))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-max))
          ;; should the format of the page separator change the code
          ;; most like has to be updated too; the regex therefore
          ;; neither is defined globally nor subject to customizing
          (let ((separator-pp (concat
                               "^-----File: "
                               "\\(\\(?:[pi]_\\)?[0-9]+[^-]+\\)-+"
                               "\\(\\(?:\\\\[^\\\n]+\\)+\\)\\\\-+$"))
                proofers)
            (while (not (bobp))
              (when (looking-at separator-pp)
                (setq page (+ 1 page)
                      page-markers (cons (point) page-markers)
                      page-scans (cons (match-string-no-properties 1)
                                       page-scans)
                      proofers (or (split-string
                                    (match-string-no-properties 2)
                                    "\\\\"
                                    'omit-nulls) "<unknown>"))
                ;; bit-compress proofer names by perfect hashing a
                ;; name into a character code; the round number is
                ;; implicitly encoded and only non-strict since it is
                ;; assumed that either all rounds of a page have
                ;; names assigned or none
                (let (pos names)
                  (mapc (lambda (x)
                          (setq pos (gethash x proofer-names))
                          (unless pos
                            (puthash x proofer proofer-names)
                            (setq pos proofer
                                  proofer (1+ proofer)))
                          (setq names (cons pos names))) proofers)
                  (setq page-proofers
                        (cons (concat (nreverse names))
                              page-proofers))))
              (forward-line -1)))))
      ;; update the various buffer local tables
      (folio-restore-proofers
       (cons (folio-hash-table-to-alist proofer-names)
             (nreverse page-proofers)))
      (folio-restore-page-markers page-markers)
      (folio-restore-page-scans page-scans))))


;;;; semantic elements, definition and movement

(defconst folio-word "Word"
  "Semantic element of a word.")

(defun folio-forward-word (&optional arg)
  "Move point forward ARG words or backward if ARG is negative.

This function is like the builtin `forward-word' except for
hyphenated words where the second half of the word is located on
the next or previous page.  I.e. different from `forward-word'
movement considers the hyphen and optionally the asterisk for a
hyphenated word at the end of a line or the leading asterisk for
a hyphenated word at the beginning of a line part of the word."
  (interactive "^p")
  (when (forward-word arg)
    (cond
     ((> (or arg 1) 0)
      (when (and (eq (char-after (point)) ?-)
                 (memq (char-after (1+ (point))) '(?* ?\n)))
        (forward-char 2)))
     (t
      (when (and (eq (char-before (point)) ?*)
                 (= (point) (1+ (line-beginning-position))))
        (backward-char 1))))
    t))

(defun folio-backward-word (&optional arg)
  "Move point backward ARG words or forward if ARG is negative.

See also `folio-forward-word'."
  (interactive "^p")
  (folio-forward-word (- (or arg 1))))

(put 'folio-word 'forward-op 'folio-forward-word)


;;;; Things

(defvar folio-thing-at-point-syntax-table
  (let ((table (make-syntax-table nil)))

    ;; nowrap
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)

    ;; nowrap front matter
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?F "w 23b" table)

    ;; nowrap poetry
    (modify-syntax-entry ?/ ". 14c" table)
    (modify-syntax-entry ?P "w 23c" table)

    ;; blockquote, may be nested
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?# ". 23n" table)

    ;; note-like thing: proofer's, footnote, side note, but also
    ;; illustration, blank page, etc.
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    ;; ignore parentheses and braces
    (modify-syntax-entry ?\( "w" table)
    (modify-syntax-entry ?\) "w" table)

    (modify-syntax-entry ?{ "w" table)
    (modify-syntax-entry ?} "w" table)

    ;; optional expression prefix
    ;; (modify-syntax-entry ?* "'" table)

    table)

"Syntax table for thing-at-point things in `folio-mode'.")

(defun folio-forward-note-thing (thing arg &optional verbose)
  "Move point to the next position that is the end of a syntactically
note-like thing.

THING is any of `folio-footnote', `folio-sidenote',
`folio-proofer-note', `folio-illustration', `folio-greek',
`folio-hebrew' \(which see), or more generally a symbol
describing a typographic element in square brackets syntax,
optionally prefix- or postfix-flagged with '*'.

Different from other `thing-at-point' move functions, point is
not changed if no THING to move over is found.

With XXX prefix argument ARG, move ARG times forward if positive, or
move backwards ARG times if negative.

The return value is t if THING is found, or nil otherwise."
  (let* ((bound (when (markerp arg) arg))
         (arg-+ve (if bound
                      (> arg (point))
                    (> arg 0)))
         (count (if bound
                    (buffer-size)
                  (if arg-+ve arg (- arg))))
         (form (get thing 'form))
         (pp (car form))
         (flagged (cl-caddr form))
         (flagged-break (eq flagged 'break)) ;; XXX footnote-break
         (continued nil) ;; XXX
         (case-fold-search t)  ;; for rogue forms
         pos)
    (with-syntax-table folio-thing-at-point-syntax-table
      (save-excursion
        (let ((beg (point))
              end)
          (beginning-of-line)
          (if arg-+ve
              ;; Search forward.
              (when (or (looking-at-p pp)
                        (re-search-backward pp nil t))
                (setq end (ignore-errors
                            (scan-sexps (point) 1)))
                (when (and end
                           (> end beg)
                           (if bound (>= end bound) t))
                  (when (and flagged
                             (eq (char-before beg) ?*))
                    (setq beg (1- beg)))
                  (goto-char end)
                  (when (and continued (eq (char-after) ?*))
                    (forward-char))
                  (setq pos (cons (cons beg (point)) pos)
                        count (1- count))))
            ;; backward
            (when (or (and (looking-at-p pp)
                           (< (point) beg)
                           (if bound
                               (>= (point) bound)
                             t))
                      (and (re-search-backward pp bound t)
                           (> (or (ignore-errors
                                    (scan-sexps (point) 1)) 0)
                              beg)))
                  (setq end (scan-sexps (point) 1))
                  (when (and continued (eq (char-after end) ?*))
                    (setq end (1+ end)))
                  (when (and flagged (eq (char-before) ?*))
                    (backward-char))
                  (setq pos (cons (cons (point) end) pos)
                        count (1- count))))
          (unless pos
            (goto-char beg)))
        (while (> count 0)
          (if arg-+ve
              ;; forward
              (if (re-search-forward pp bound t)
                  (let (beg end)
                    (goto-char (match-beginning 0))
                    (when (and flagged (eq (char-after) ?*))
                      (forward-char))
                    (setq beg (point)
                          end (ignore-errors
                                (scan-sexps (point) 1)))
                    (if end
                        (progn
                          (goto-char end)
                          (when (and continued (eq (char-after) ?*))
                            (forward-char))
                          (setq pos (cons (cons beg (point)) pos)
                                count (1- count)))
                      (setq count 0)))
                (setq count 0))
            ;; backward
            (if (re-search-backward pp bound t)
                (let ((end (ignore-errors
                             (scan-sexps (point) 1))))
                  (when (and continued (eq (char-after end) ?*))
                    (setq end (1+ end)))
                  (when (and flagged (eq (char-before) ?*))
                    (backward-char))
                  (setq pos (cons (cons (point) end) pos)
                        count (1- count)))
              (setq count 0))))))
    (if pos
        (goto-char (if arg-+ve (cdar pos) (caar pos)))
      (when verbose
        (message "No %s found" (downcase (symbol-value thing)))))
    (if bound (if arg-+ve (nreverse pos) pos) count)))

(defconst folio-illustration "Illustration"
  "An illustration, optionally with caption.")

(put 'folio-illustration 'form '("\\[Illustration:?" "\\]"))
(put 'folio-illustration 'forward-op 'folio-forward-illustration)

(defun folio-forward-illustration (&optional arg)
  "Move forward to the end of an illustration.

With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N illustration.  When moving backward
point is at the beginning of an illustration.  Return the number
of pending moves.  Point only is moved if an illustration
actually was found."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive)))
    (folio-forward-note-thing 'folio-illustration arg interactively)
    (when interactively
      (recenter))))

(defun folio-backward-illustration (&optional arg)
  "Move backward to the beginning of an illustration.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N illustrations.  For additional information
see the related command `folio-forward-illustration'."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive)))
    (folio-forward-note-thing
     'folio-illustration (- (or arg 1)) interactively)
    (when interactively
      (recenter))))

(defconst folio-footnote "Footnote"
  "Semantic element of a footnote.

`folio-footnote' can be operated on by using `thing-at-point' and
related functions, and additionally `folio-mark-thing-at-point',
`folio-kill-thing-at-point', and `folio-move-thing-at-point' (which
see).")

(put 'folio-footnote 'form '("\\*?\\[Footnote\\s-*" "\\]*?" 'break))
(put 'folio-footnote 'forward-op 'folio-forward-footnote)

(defun folio-forward-footnote (&optional arg)
  "Move forward to the end of a footnote.

With argument ARG, move ARG times; a negative argument ARG = -N
means move backward N footnotes.  When moving backward point is
at the beginning of a footnote.  Return the number of pending
moves.  Point only is moved if a footnote actually was found."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive)))
    (folio-forward-note-thing 'folio-footnote arg interactively)
    (when interactively
      (recenter))))

(defun folio-backward-footnote (&optional arg)
  "Move backward to the beginning of a footnote.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N blockquotes.  For additional information see
the related command `folio-forward-footnote'."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive)))
    (folio-forward-note-thing
     'folio-footnote (- (or arg 1)) interactively)
    (when interactively
      (recenter))))

(defconst folio-sidenote "Side-note"
  "Typographic element of a side-note.

`folio-sidenote' can be operated on by using `thing-at-point' and
related functions, and additionally `folio-mark-thing-at-point',
`folio-kill-thing-at-point', and `folio-move-thing-at-point' (see
which.)")

(put 'folio-sidenote 'form '("\\*?\\[Sidenote:?" "\\]" t))
(put 'folio-sidenote 'forward-op 'folio-forward-sidenote)

(defun folio-forward-sidenote (&optional arg)
  "Move forward to the end of a side-note.

With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N sidenotes.  When moving backward point
is at the beginning of a sidenote.  Return the number of pending
moves.  Point only is moved if a sidenote actually was found."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive))
        count)
    (setq count (folio-forward-note-thing
                 'folio-sidenote arg interactively))
    (when interactively
      (recenter))
    count))

(defun folio-backward-sidenote (&optional arg)
  "Move backward to the beginning of a side-note.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N sidenote.  For additional information see
the related command `folio-forward-sidenote'."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive))
        count)
    (setq count (folio-forward-note-thing
                 'folio-sidenote (if (markerp arg)
                                     arg
                                   (- (or arg 1)))
                 interactively))
    (when interactively
      (recenter))
    count))

(defconst folio-blank-page "Blank Page"
  "Semantic element of a blank page.")

(put 'folio-blank-page 'form '("\\[Blank\s+Page" "\\]"))
(put 'folio-blank-page 'forward-op 'folio-forward-blank-page)

(defun folio-forward-blank-page (&optional arg)
  "Move forward to the end of a blank page.
With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N blank pages.  When moving backward
point is at the beginning of a blank page.  Return the number of
pending moves.  Point only is moved if a blank page actually was
found."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive))
        count)
    (setq count (folio-forward-note-thing
                 'folio-blank-page arg (not interactively)))
    (when interactively
      (recenter))
    count))

(defun folio-backward-blank-page (&optional arg)
  "Move backward to the beginning of a blank page.
With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N blank pages.  For additional information see
the related command `folio-forward-blank-page'."
  (interactive "^p")
  (let ((interactively (called-interactively-p 'interactive))
        count)
    (setq count (folio-forward-note-thing
                 'folio-blank-page (- (or arg 1)) interactively))
    (when interactively
      (recenter))
    count))

(defvar folio-wrap-thing-syntax-table
  (let ((table (make-syntax-table)))

    ;; no-wrap
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)

    ;; no-wrap front matter
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?F "w 23b" table)

    ;; no-wrap poetry
    (modify-syntax-entry ?/ ". 14c" table)
    (modify-syntax-entry ?P "w 23c" table)

    ;; blockquote, may be nested
    (modify-syntax-entry ?/ ". 14n" table)
    (modify-syntax-entry ?# ". 23n" table)

    table)
  "Syntax table for `folio-forward-wrap-thing'.")

(defun folio-forward-wrap-thing (thing arg &optional verb)
  "Move forward to a section with re-wrap constraints.

THING is any of `folio-blockquote', `folio-nowrap',
`folio-frontmatter', `folio-poetry' \(which see), or more
generally a symbol describing a typographic element in
slash-symbol syntax.

Different from other `thing-at-point' move functions, point is
not changed if no THING to move over is found."
  (let* ((arg-+ve (> arg 0))
         (count (if arg-+ve arg (- arg)))
         (pp (car (get thing 'form)))
         pos)
    (with-syntax-table folio-thing-at-point-syntax-table
      (save-excursion
        ;; if within syntactic comment move to its beginning or to its
        ;; end if direction is forward
        (let ((state (syntax-ppss)))
          (when (nth 4 state)
            (goto-char (nth 8 state))
            (unless arg-+ve
              (when (looking-at-p pp)
                (setq pos (point) count (1- count))))))
        ;; the parse predicate describes the syntax of the opening; it
        ;; is a regexp in the car of the form attribute of THING
        (when (> count 0)
          (let ((at-end (if arg-+ve
                            (function eobp)
                          (function bobp))))
            (while (and (> count 0) (not (funcall at-end)))
              (if (if arg-+ve
                      (if (looking-at-p pp)
                          (forward-comment 1)
                        (when (re-search-forward pp nil t)
                          (beginning-of-line)
                          (forward-comment 1)))
                    (re-search-backward pp nil t))
                  (setq pos (point) count (1- count))
                (setq count 0)
                (when (and (null pos) verb)
                  (message "No %s found"
                           (downcase
                            (symbol-value thing)))))))))
      (when pos
        (goto-char pos)
        (if arg-+ve
            (end-of-line)
          (beginning-of-line))))
    count))

(defconst folio-blockquote "Block Quotation"
  "Semantic element of a block quotation or extract.

`folio-blockquote' can be operated on by using `thing-at-point'
and related functions, and additionally
`folio-mark-thing-at-point', `folio-kill-thing-at-point', and
`folio-move-thing-at-point' (see which.)")

(put 'folio-blockquote 'form '("^/#" "^#/"))
(put 'folio-blockquote 'forward-op 'folio-forward-blockquote)

(defun folio-forward-blockquote (&optional arg)
  "Move forward to the end of a block quotation or extract.

With argument ARG, move ARG times; a negative argument ARG = -N
means move backward N block-quotes.  When moving backward point
is at the beginning of a block-quote.  Return the number of
pending moves.  Point only is moved if a block-quote actually was
found.  Parsing is non-strict, i.e. for a successful move the
quote's end marker need not even exist, or, with nested
block-quotes the outer quote's end-marker effectively may close
an inner quote missing it's end-marker.  In order to descend into
nested block-quotes both forward and backward moves must be
used."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-blockquote arg called-interactively)
    (when called-interactively
      (recenter))))

(defun folio-backward-blockquote (&optional arg)
  "Move backward to the beginning of a block quotation or extract.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N block-quotes.  For additional information
see the related command `folio-forward-blockquote'."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-blockquote (- (or arg 1)) called-interactively)
    (when called-interactively
      (recenter))))

(defconst folio-nowrap "No-Wrap"
  "Structural element of a \"no-wrap\" text section.

XXX requires special treatment with respect to the preservation
of line breaks, indentation, and spacing.  `folio-sidenote' can
be operated on by using `thing-at-point' and related functions,
and additionally `folio-mark-thing-at-point',
`folio-kill-thing-at-point', and `folio-move-thing-at-point' (see
which.)")

(put 'folio-nowrap 'form '("^/\\*" "^\\*/"))
(put 'folio-nowrap 'forward-op 'folio-forward-nowrap)

(defun folio-forward-nowrap (&optional arg)
  "Move forward to the end of a \"no-wrap\" section.

With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N times.  When moving backward point is
at the beginning of a \"no-wrap\" section.  Return the number of
pending moves.  Point only is moved if a \"no-wrap\" actually was
found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-nowrap arg called-interactively)
    (when called-interactively
      (recenter))))

(defun folio-backward-nowrap (&optional arg)
  "Move backward to the start of a \"no-wrap\" section.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N times.  When moving forward point is at the
end of a \"no-wrap\" section.  Return the number of pending
moves.  Point only is moved if a \"no-wrap\" section actually was
found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-nowrap (- (or arg 1)) called-interactively)
    (when called-interactively
      (recenter))))

(defconst folio-poetry "Poetry"
  "Semantic element of a \"poetry\" text section.

XXX requires special treatment with respect to the preservation
of line breaks, indentation, and spacing.  `folio-sidenote' can
be operated on by using `thing-at-point' and related functions,
and additionally `folio-mark-thing-at-point',
`folio-kill-thing-at-point', and `folio-move-thing-at-point' (see
which.)")

(put 'folio-poetry 'form '("^/P" "^P/"))
(put 'folio-poetry 'forward-op 'folio-forward-poetry)

(defun folio-forward-poetry (&optional arg)
  "Move forward to the end of a \"poetry\" section.

With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N times.  When moving backward point is
at the beginning of a \"poetry\" section.  Return the number of
pending moves.  Point only is moved if a \"poetry\" section
actually was found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-poetry arg called-interactively)
    (when called-interactively
      (recenter))))

(defun folio-backward-poetry (&optional arg)
  "Move backward to the start of a \"poetry\" section.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N times.  When moving forward point is at the
end of a \"poetry\" section.  Return the number of pending moves.
Point only is moved if a \"poetry\" section actually was found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-poetry (- (or arg 1)) called-interactively)
    (when called-interactively
      (recenter))))

(defconst folio-frontmatter "Front-matter"
  "Structural element of a \"front-matter\" text section.
XXX requires special treatment with respect to the preservation
of line breaks, indentation, and spacing.  `folio-sidenote' can
be operated on by using `thing-at-point' and related functions,
and additionally `folio-mark-thing-at-point',
`folio-kill-thing-at-point', and `folio-move-thing-at-point' (see
which.)")

(put 'folio-frontmatter 'form '("^/F" "^F/"))
(put 'folio-frontmatter
     'forward-op 'folio-forward-frontmatter)

(defun folio-forward-frontmatter (&optional arg)
  "Move forward to the end of a \"front-matter\" section.

With prefix argument ARG, move ARG times; a negative argument ARG
= -N means move backward N times.  When moving backward point is
at the beginning of a \"front-matter\" section.  Return the
number of pending moves.  Point only is moved if a
\"front-matter\" section actually was found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-frontmatter arg called-interactively)
    (when called-interactively
      (recenter))))

(defun folio-backward-frontmatter (&optional arg)
  "Move backward to the start of a \"front-matter\" section.

With argument ARG, move ARG times; a negative argument ARG = -N
means move forward N times.  When moving forward point is at the
end of a \"front-matter\" section.  Return the number of pending moves.
Point only is moved if a \"front-matter\" section actually was found."
  (interactive "^p")
  (let ((called-interactively
         (called-interactively-p 'interactive)))
    (folio-forward-wrap-thing
     'folio-frontmatter (- (or arg 1)) called-interactively)
    (when called-interactively
      (recenter))))

(defun folio-forward-section-thing (thing &optional arg verb)
  "Move forward by section of type THING.

THING is the symbol of a structural element like `folio-section',
`folio-chapter'.  With argument ARG, move ARG times; a negative
argument ARG = -N means move backward N times.  If VERB is
non-nil print a message if THING is not found."
  (let* ((arg (or arg 1))
         (arg-+ve (> arg 0))
         (count (if arg-+ve arg (- arg)))
         pos)
    (save-excursion
      (when (> count 0)
        (let ((at-end (if arg-+ve (function eobp) (function bobp)))
              (pp (car (get thing 'form))))
          (save-match-data
            (while (and (> count 0) (not (funcall at-end)))
              (if (if arg-+ve
                      (progn
                        (when (and arg-+ve (looking-at pp))
                          (goto-char (match-end 0)))
                        (re-search-forward pp nil t))
                    (re-search-backward pp nil t))
                  (progn
                    (goto-char (+ (match-beginning 0) 2))
                    (setq pos (point) count (1- count)))
                (if arg-+ve
                    (setq pos (point-max))
                  (setq pos (point-min)))
                (setq count 0)))))))
    (if (or (null pos) (eq pos (point)))
        (when verb
          (message "No %s found" (downcase (symbol-value thing))))
      (goto-char pos))
    count))

(defun folio-join-words-help-form ()
  "Return the help form for `folio-join-words'."
  (concat "You have typed "
          (single-key-description help-char)
          ", the help character.

\(Type `q' to exit the Help command.)

j         Join the two halfs of the word.
k or -    Keep the hyphen.
t or *    Tag the hypen with an asterisk for a later check.
C-g       Abort the command."))

(defun folio-join-words-prompt (lhs rhs count diff)
  "Return the prompt for `folio-join-words'.

LHS and RHS are either side of a hyphenated word.  COUNT is the
occurrence count of the hyphenated spelling, DIFF is the
occurrence count of an un-hyphenated variant."
  (concat "(\"" lhs rhs "\": "
          (number-to-string count) " "
          (folio-pluralize "occurrence" count)
          ",  \"" lhs "-" rhs "\": "
          (number-to-string diff) " "
          (folio-pluralize "occurrence" diff)
          ") Join words? ("
          (single-key-description help-char)
          " for help) "))

;; XXX TODO integrate 'folio-stemmer.el'
;; XXX make dictionary prefix search available
;; XXX use the current vocabulary folio-dictionary for assistance
(defun folio-join-words (&optional mode)
  "Join the two hyphenated words at point.

The MODE argument is a symbol controlling how this function
operates.  Meaningful values are:

join         unconditionally join words
keep         unconditionally keep a hyphen
tag          keep the hyphen but tag with asterisk for
               later inspection
frequency    join if this is the only occurrence
ask          like frequency but query the user if ambiguous

When called interactively MODE is ask or join if called with
prefix argument; when called from Lisp, MODE defaults to ask."
  (interactive "P")
  (if (called-interactively-p 'any)
      (if mode
          (setq mode 'join)
        (setq mode 'ask))
    (setq mode (or mode 'ask)))
  (save-match-data
    ;; A hyphenated word split accross lines shall always be tagged
    ;; with a trailing asterisk; the asterisk might be removed
    ;; later, depending on the value of MODE.
    (end-of-line)
    (when (eq (char-before (point)) ?-)
      (insert ?*))
    ;; The hyphen is not at end of line if rewrapping already has been
    ;; applied.
    (let* ((pos (line-beginning-position))
           (hyphen-break (save-excursion
                           (while (and (> (point) pos)
                                       (not (eq (char-before (point)) ?*)))
                             (backward-char 1))
                           (point)))) ;; also beginning of rhs
      (if (and (eq (char-before hyphen-break) ?*)
               (eq (char-before (1- hyphen-break)) ?-))
          ;; else nothing to join
          (atomic-change-group
            (when (eq (char-after hyphen-break) ?\n)
              (delete-char 1)
              (when (eq (char-after (point)) ?*)
                (delete-char 1))
              (skip-syntax-forward "^ ")
              ;; restore line break
              (when (eq (char-after (point)) ?\ )
                (delete-char 1)
                (insert ?\n)))
            (goto-char hyphen-break)
            (cond
             ((eq mode 'tag)
              t)
             ((eq mode 'keep)
              (assert (eq (char-before (point)) ?*))
              (delete-char -1))
             ((eq mode 'join)
              (assert (eq (char-before (point)) ?*))
              (delete-char -2))
             ((or (eq mode 'frequency) (eq mode 'ask))
              (folio-backward-word)
              (unless (looking-at "\\(\\w+\\)-\\*\\(\\w+\\)")
                (error "Failure parsing hyphenated word"))
              (folio-forward-word)
              (let ((lhs (match-string-no-properties 1))
                    (rhs (match-string-no-properties 2))
                    (lhs-beg (match-beginning 1))
                    (rhs-beg (match-beginning 2))
                    (case-fold-search t)
                    (count 0)
                    (diff 0))
                (save-excursion
                  (goto-char (point-min))
                  (let ((word (concat lhs rhs)))
                    (when (search-forward word nil t)
                      (setq count (1+ count))
                      (while (search-forward word nil t)
                        (setq count (1+ count))))
                    (when (or (> count 0) (eq mode 'ask))
                      (goto-char (point-max))
                      (setq word (concat lhs "-" rhs))
                      (while (search-backward word nil t)
                        (setq diff (1+ diff))))))
                (if (eq mode 'frequency)
                    ;; the threshold possibly should be customizable
                    (when (> count diff)
                      (assert (eq (char-before (point)) ?*))
                      (delete-char -2)) ;; else keep tag
                  (if (= diff 0)
                      ;; non-hyphenated (non-tagged) version if either
                      ;; half starts upper case; keep the hyphen
                      ;; otherwise join
                      (if (or (folio-upper-case-char-at-p lhs-beg)
                              (folio-upper-case-char-at-p rhs-beg))
                          (delete-char -1)
                        (delete-char -2))
                    (let* ((prompt (folio-join-words-prompt
                                    lhs rhs count diff))
                           (choices '(?j ?k ?- ?t))
                           (help-form `(folio-join-words-help-form))
                           (choice (read-char-choice prompt choices)))
                      (cond
                       ((eq choice ?j)
                        (assert (eq (char-before (point)) ?*))
                        (delete-char -2))
                       ((or (eq choice ?k) (eq choice ?-))
                        (assert (eq (char-before (point)) ?*))
                        (delete-char -1))
                       ((or (eq choice ?t) (eq choice ?*))
                        t)
                       (t
                        (error "Invalid choice")))
                      (message ""))))))
             (t
              (error "Unrecognized argument"))))
        t)
      nil)))

(defun folio-insert-section-break (section)
  "At current line insert the section break SECTION.

SECTION normally is a string consisting of one, two or four
new-line characters.

Before actually inserting SECTION remove any blank lines before
and after point."
  (while (and (not (bobp))
              (eq (char-after (line-beginning-position -2)) ?\n))
    (forward-line -1))
  (while (eq (char-after (point)) ?\n)
    (delete-char 1))
  (insert section))

(defun folio-join-pages-mark (&optional clear)
  "Create a marker for a page boundary at the current line.

The current line should be non-empty, i.e. contain more than a
new-line character.  If called with a non-nil argument CLEAR only
remove any existing marker.  Return the marker in the form of an
evaporating overlay."
  (remove-overlays (point-min) (point-max) 'folio-page-join t)
  (unless clear
    (let* ((beg (line-beginning-position))
           (end (line-end-position 1))
           (width (- end beg))
           (len (max (- (min fill-column
                             (window-body-width))
                        width
                        6)
                     0))
           (after-string (propertize (make-string len ?\ )
                                     'face 'folio-highlight-page-join))
           (overlay (make-overlay beg end nil 'font-advance)))
      (dolist (prop `((face . folio-highlight-page-join)
                      (after-string . ,after-string)
                      (evaporate . t)
                      (folio-page-join . t)
                      (help-echo "Page boundary")))
        (overlay-put overlay (car prop) (cdr prop)))
      overlay)))

(defun folio-join-pages-help-form ()
  "Return the help form for `folio-join-pages'."
  (concat "You have typed "
          (single-key-description help-char)
          ", the help character.

\(Type `q' to exit the Help command.)

j      Join pages without section break.
p      Separate pages by paragraph break.
s      Separate pages by section break.
c      Separate pages by chapter break.
C-g    Abort the command."))

(defun folio-join-pages-prompt ()
  "Prompt and execute a page join."
  ;; XXX support recursive edit
  (recenter)
  (let ((mark (folio-join-pages-mark))
        resume)
    (unwind-protect
        (progn
          (end-of-line 2)
          (let* ((choices '(?j ?p ?s ?c ?r))
                 (prompt (concat "Join pages? ("
                                 (single-key-description help-char)
                                 " for help) "))
                 (help-form `(folio-join-pages-help-form))
                 (choice (read-char-choice prompt choices)))
            (setq resume (cond
                          ((eq choice ?j)
                           t)
                          ((eq choice ?p)
                           (folio-insert-section-break "\n")
                           (message "Paragraph"))
                          ((eq choice ?s)
                           (folio-insert-section-break "\n\n")
                           (message "Section"))
                          ((eq choice ?c)
                           (folio-insert-section-break "\n\n\n\n")
                           (message "Chapter"))
                          ((eq choice ?r) ;; XXX sync page scan
                           t)
                          (t
                           (error "Invalid choice"))))))
      (delete-overlay mark))
    resume))

(defun folio-join-pages (&optional start end)
  "Remove page separators in buffer or region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-match-data
    (let ((end-marker (copy-marker (or end (point-max))))
          (start (or start (point)))
          (separator-pp (concat
                         "^-----File: "
                         "\\(?:[pi]_\\)?[0-9]+[^-]+-+"
                         "\\(?:\\\\[^\\\n]+\\)+\\\\-+$")))
      (goto-char start)
      (delete-trailing-whitespace start end-marker)
      (while (re-search-forward separator-pp end-marker t)
        (message "Joining page %d" (folio-page-at-point))
        (let ((change (prepare-change-group)))
          (unwind-protect
              (progn
                (activate-change-group change)
                (delete-region (match-beginning 0) (match-end 0))
                ;; newlines preceeding the separator safely can be
                ;; removed; this simplifies joining
                (while (eq (char-before (point)) ?\n)
                  (delete-char -1))
                (let* ((pos (point))
                       (post (prog2
                                 (forward-char 1)
                                 (folio-looking-at-thing-p
                                  '(folio-word
                                    folio-nowrap
                                    folio-sidenote
                                    paragraph
                                    folio-blockquote
                                    folio-section
                                    folio-chapter
                                    folio-footnote))
                               (forward-char -1)))
                       post-skip)
                  (when (eq post 'paragraph)
                    (forward-line 2)
                    (cond
                     ((folio-looking-at-thing-p 'folio-sidenote)
                      (setq post 'folio-sidenote
                            post-skip (point)))
                     ((folio-looking-at-thing-p 'folio-illustration)
                      (setq post 'folio-illustration
                            post-skip (point))))
                    (goto-char pos))
                  (when (cond
                         ((eq post 'folio-word)
                          ;; unless joining of two word halfs was
                          ;; successful, or the post word starts lower
                          ;; case, query: there may be punctuation
                          ;; missing at end of line or page, or a
                          ;; paragraph break
                          (or (eq (char-before pos) ?,)
                              (folio-join-words 'ask)
                              (folio-lower-case-char-at-p (1+ pos))
                              (folio-join-pages-prompt)))
                         ((or (eq post 'folio-nowrap)
                              (eq post 'folio-blockquote))
                          ;; two no-wraps or block-quotes directly
                          ;; either side of the page boundary safely
                          ;; can be merged by deleting their marker
                          ;; end and begin lines; otherwise prompt for
                          ;; a missing paragraph break or some such
                          (if (folio-looking-back-at-thing-p post)
                              (progn
                                (delete-region (line-beginning-position)
                                               (line-beginning-position 3))
                                (forward-char -1)
                                (folio-join-words 'ask)
                                t)
                            (folio-join-pages-prompt)))
                         ((eq post 'paragraph)
                          ;; a paragraph break followed by a double
                          ;; quotation mark although not necessarily
                          ;; indicating direct speech is assumed to be
                          ;; reasonably safe for auto-joining: query
                          ;; for anything else
                          (or (looking-at-p "\n\n\"")
                              (folio-join-pages-prompt)))
                         ((or (eq post 'folio-sidenote)
                              (eq post 'folio-illustration))
                          ;; attempt joining words split across pages
                          ;; and separated by one or more side-notes,
                          ;; illustrations, or blank lines only; a
                          ;; split word in this case however is
                          ;; retained tagged; any missing paragraph
                          ;; break however is considered an issue of
                          ;; illustration or sidenote fixup
                          (when (and (eq (char-before pos) ?*)
                                     (folio-looking-back-at-thing-p
                                      'folio-word))
                            (let (word)
                              (while (or (eq (char-after (point)) ?\n)
                                         (folio-looking-at-thing-p
                                          'folio-sidenote)
                                         (folio-looking-at-thing-p
                                          'folio-illustration))
                                (forward-line 1))
                              (when (folio-looking-at-thing-p
                                     'folio-word)
                                (when (eq (char-after (point)) ?*)
                                  (delete-char 1))
                                (skip-syntax-forward "^ ")
                                ;; may be a single word, or two or
                                ;; more words joined by hyphens or
                                ;; dashes, with or without trailing
                                ;; punctuation actually
                                (setq word (buffer-substring
                                            (line-beginning-position)
                                            (point)))
                                (delete-region (line-beginning-position)
                                               (point))
                                (while (eq (char-after (point)) ?\ )
                                  (delete-char 1)))
                              (goto-char pos)
                              (when word
                                (insert word)
                                (goto-char pos)
                                (folio-join-words 'frequency))))
                          t)
                         (t
                          ;; XXX continued footnote in pre
                          (folio-join-pages-prompt)))
                    (accept-change-group change)
                    (setq change nil))))
            (when change
              (cancel-change-group change))))))))



(provide 'folio-segmentation)

;;; folio-segmentation.el ends here
