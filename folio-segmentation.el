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
                        (cons (concat (nreverse names)) page-proofers))))
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

(defun folio-join-words-help-form ()
  "Return the help form for `folio-join-words'."
  (concat "You have typed "
          (single-key-description help-char)
          ", the help character.

\(Type `q' to exit the Help command.)

j           Join the two halfs of the word.
k or -      Keep the hyphen.
t or *      Tag the hypen with an asterisk for a later check.
C-g         Abort the command."))

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



(provide 'folio-segmentation)

;;; folio-segmentation.el ends here
