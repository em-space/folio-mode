;;; folio-outline.el --- Outlines for Folio mode

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

(require 'folio-core)
(require 'folio-segmentation)
(require 'folio-time)

(defvar folio-outline-level 1
  "The current outline level.

Chapters commonly are associated with level 1, sections with
level 2.  This variable can be let-bound in the indexing
process.")

(defconst folio-outline-level-alist
  '((1 . folio-chapter)
    (2 . folio-section))
  "Alist mapping outline level to section symbol.")

(defvar folio-outline-sequence 0
  "Running sequence number for sections when indexing.")

(defconst folio-outline-skip-regexp
  (concat "^\\(/[*#FPL]\\)\\|^\\([*#FPL]/\\)\\|"
          "^\\(-----File: \\(?:[pi]_\\)?[0-9]+[^-]+\\)")
  "Regexp for lines to skip when assembling outline context.")

(defconst folio-outline-kill-regexp
  (concat "</?\\(i\\|b\\|g\\|f\\|u\\|sc\\|tb\\)\\(\\)>\\|"
          "\\[\\*\\*[^]]+\\]")
  "Regexp for tokens to kill when assembling outline context.")

(defvar folio-outline-marker nil
  "Progress marker for outline indexing.")
(make-variable-buffer-local 'folio-outline-marker)

(defcustom folio-outline-delay 0.3
  "Time in seconds to wait before resuming an outline scan."
  :group 'folio-outline
  :tag "Outline Scan Delay"
  :type 'number)

(defcustom folio-outline-pause 0.1
  "Time in seconds to pause an outline scan."
  :group 'folio-outline
  :tag "Outline Scan Pause"
  :type 'number)

(folio-define-timer 'outline
  "Idle timer driving the outline scanner."
  :function 'folio-outline-process-buffer
  :repeat 'repeat
  :secs (lambda () folio-outline-delay)
  :pause (lambda () folio-outline-pause))

(defvar folio-outline-props
  '(folio-outline folio-chapter folio-section folio-target)
  "*List of text properties used for outlines.")

(defun folio-outline-propertize (beg end props &optional undo)
  "Mark a region interesting by adding outline properties.

BEG and END are buffer positions.  PROPS should be a plist with
text properties from `folio-outline-props', like 'folio-chapter
or 'folio-section.  If UNDO is non-nil remove any outline
properties instead, ignoring PROPS.  If adding font-lock face
properties `font-lock-fontify-region' or
`font-lock-fontify-buffer' should be called at an appropriate
time.  This is omitted here for performance reasons."
  (with-silent-modifications
    (if undo
        (progn ;; XXX TODO selective w.r.t. section prop
          (remove-list-of-text-properties
           beg end folio-outline-props)
          (font-lock-fontify-region beg end))
      (add-text-properties
       beg end `(rear-sticky ,folio-outline-props
                 folio-outline t ,@props)))))

(defsubst folio-outline-unpropertize (&optional props beg end)
  "Remove any text properties of the outline scanner.

BEG and END restrict the operation to a region.  If omitted the
respective buffer beginning or end position is used."
  (folio-outline-propertize
   (or beg (point-min)) (or end (point-max)) props 'undo))

(defun folio-outline-propertize-heading (section seq)
  (let (beg end props)
    (save-excursion
      (beginning-of-line)
      (set-marker folio-outline-marker (setq beg (point)))
      (skip-chars-forward "\n")
      (setq end (line-end-position)
            props (plist-put props section seq))
      ;; Propertize leading blank lines and the first non-empty line
      ;; as the chapter.
      (folio-outline-propertize beg end props))))

(defun folio-outline-process-buffer ()
  "Index document structure for use in outline views.

The actual process is just to propertize the section beginning
including the first non-empty line of a heading."
  ;; Yield for updating the display.
  (folio-yield (/ 2 (1+ folio-outline-sequence)))
  (let ((section (cdr (assoc folio-outline-level
                             folio-outline-level-alist))))
    (save-excursion
      ;; Scanning the buffer bottom-up is slightly faster because
      ;; moving forward requires two jumps for this purpose.
      (unless folio-outline-marker
        (folio-outline-unpropertize)
        (setq folio-outline-marker (make-marker)
              folio-outline-sequence 0)
        (set-marker folio-outline-marker (point-max)))
      (goto-char (marker-position folio-outline-marker))
      ;; Defer C-g quitting to keep marker and meta data in
      ;; sync.
      (let ((inhibit-quit nil))
        (while (not (folio-activity-interrupted-p
                     'outline (or (bobp)
                                  (not (zerop
                                        (folio-forward-section-thing
                                         section -1))))))
          (folio-outline-propertize-heading
           section folio-outline-sequence)
          (setq folio-outline-sequence
                (1+ folio-outline-sequence)))))))


(provide 'folio-outline)

;;; folio-outline.el ends here
