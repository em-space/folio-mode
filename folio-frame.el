;;; folio-frame.el --- Folio mode frames and windows

;; Copyright (C) 2012, 2013  Christoph Kluge

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines general functionality related to frames and
;; windows.

;;; Code:



(defun folio-fringe-mark (bitmap pos &optional side face)
  "Insert a fringe bitmap at buffer position POS.
BITMAP is a symbol identifying a bitmap defined with
`define-fringe-bitmap', or a stock bitmap from `fringe-bitmaps'.
SIDE defaults to 'right-fringe; an alternative is 'left-fringe.
FACE is used to determine the bitmap's foreground and background
color.  The function returns an overlay that should be removed
with `folio-fringe-unmark' unless `remove-overlays' is used to
clean up."
  (let* ((display-string `(,(or side 'right-fringe) ,bitmap .
                           ,(when face (cons face nil))))
          (before-string (propertize "!" 'display display-string))
          (overlay (make-overlay pos pos)))
    (overlay-put overlay 'before-string before-string)
    (overlay-put overlay 'folio-fringe t)
    overlay))

(defun folio-fringe-unmark (overlay)
  "Remove a fringe bitmap by removing its OVERLAY."
  (when (and overlay
             (overlay-buffer overlay)
             (overlay-get overlay 'folio-fringe))
    (delete-overlay overlay)))

(defun folio-fringe-move-mark (overlay pos)
  "Move the fringe OVERLAY to position POS."
  (when (and (overlay-buffer overlay)
             (overlay-get overlay 'folio-fringe))
    (move-overlay overlay pos pos)))

;; XXX save `current-window-configuration' and restore it with
;; `set-window-configuration

(defun folio-hscroll-sync-point ()
  "Place point within the horizontal visibility of its window area."
  ;; Stolen from ispell.el.
  (if truncate-lines  ; continuation lines not displayed
      ;; See if display needs to be scrolled.
      (let* ((hscroll (window-hscroll))
             (column (- (current-column) (max hscroll 1))))
        (if (and (< column 0) (> hscroll 0))
            (scroll-right (max (- column) 10))
          (let ((width (window-width)))
            (if (>= column (- width 2))
                (scroll-left (max (- column width -3) 10))))))))

(defun folio-window-resize (&optional arg)
  "Interactively resize the current window vertically.
If used with prefix argument resize the window horizontally."
  (interactive "@P")
  (let ((prompt (format "%s window (+/-)? "
                        (if arg "Widen/Narrow" "Enlarge/Shrink")))
        event)
    (while (progn
             (setq event (read-event prompt t 3.0))
             (cond
              ((eq event ?+)
               (enlarge-window 1 arg) t)
              ((eq event ?-)
               (enlarge-window -1 arg) t)
              (t nil))))
    (if event
        (push event unread-command-events)
      (message nil))))


(provide 'folio-frame)

;;; folio-frame.el ends here
