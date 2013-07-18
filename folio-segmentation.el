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
(provide 'folio-segmentation)

;;; folio-segmentation.el ends here
