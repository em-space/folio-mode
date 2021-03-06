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

(defvar folio-outline-level 2
  "The current outline level.

Chapters commonly are associated with level 2, sections with
level 3.  This variable can be let-bound in the indexing
process.")

(defvar folio-outline-sequence 0
  "Running sequence number for sections when indexing.")

(defvar folio-outline-headings nil
  "*List caching outline headings.")
(make-variable-buffer-local 'folio-outline-headings)

(defconst folio-outline-skip-page-regexp
  "^\\(-----File: \\(?:[pi]_\\)?[0-9]+[^-]+\\)"
  "Regexp for skipping page separators when outline sectioning.")

(defconst folio-outline-skip-regexp
  (concat "^\\(/[*#FPL]\\)\\|^\\([*#FPL]/\\)\\|"
          folio-outline-skip-page-regexp)
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
  :repeat t
  :secs (lambda () folio-outline-delay)
  :pause (lambda () folio-outline-pause))

(defun folio-outline-skip (&optional regexp)
  "Move forward to the first non-empty line of a heading.
An empty line apart from the trivial is one that matches
`folio-outline-skip-regexp'.  Return the number of leading
newlines lines skipped.  If REGEXP is non-nil use that for
matching and always return zero."
  (let ((n 0))
    (if regexp
        (while (when (looking-at-p regexp)
                 (forward-line)
                 t))
      (while (or (> (setq n (skip-chars-forward "\n")) 0)
                 (when (looking-at-p folio-outline-skip-regexp)
                   (forward-line)
                   t))))
    n))


(defun folio-outline-section-hidden-p (&optional section)
  "Return t if the current section is hidden.
If SECTION is non-nil check that instead.  SECTION should be a
cons of buffer start and end positions."
  (let ((bounds (or section (folio-section-bounds)))
        ov)
    (when (and bounds (setq ov (car (overlays-at
                                     (1- (cdr bounds))))))
      (eq (overlay-get ov 'invisible) 'folio-outline))))

(defun folio-outline-section-heading ()
  "Return outline heading for section at point."
  (let* ((section (folio-current-section))
         (level (cdr (assq (car section) folio-section-alist)))
         (index (cdr section))
         (heading (or (cdr (elt folio-outline-headings index))
                      "<Unknown Section>")))
    (concat
     (propertize (make-string level ?*)
                 'face 'folio-outline-section-title-mark)
     " "
     (propertize heading
                 'face 'folio-outline-section-title)
     (propertize "..."
                 'face 'folio-outline-section-title-mark))))

(defun folio-outline-section-set-hidden (section hidden)
  "Hide SECTION if HIDDEN is not nil, show it otherwise.
SECTION is a cons of buffer start and end positions."
  (let* ((beg (car section))
         (end (cdr section))
         ov display)
    (if hidden ;; hide
        (progn
          (goto-char beg)
          (setq ov (car (overlays-at beg))
                display (folio-outline-section-heading))
          (move-overlay ov beg end)
          (with-silent-modifications
            (put-text-property
             (max (1- beg) (point-min)) end 'read-only t))
          (dolist (prop `((invisible . folio-outline)
                          (face . nil)
                          (display . ,display)))
            (overlay-put ov (car prop) (cdr prop))))
      ;; show
      (goto-char beg)
      (setq ov (car (overlays-at beg)))
      (folio-outline-skip)
      (move-overlay ov beg (line-end-position))
      (dolist (prop `((invisible . nil)
                      (face . folio-outline-section-title)
                      (display . nil)))
        (overlay-put ov (car prop) (cdr prop)))
      (beginning-of-line)
      (with-silent-modifications
        (remove-text-properties
         (max (1- beg) (point-min)) end '(read-only))))))

(defun folio-outline-section-hideshow (flag-or-func)
  "Show or hide current section depending on FLAG-OR-FUNC.
If FLAG-OR-FUNC is a function, it will be run for the current
section, i.e. with a cons of the section boundaries for the
argument.  If FLAG-OR-FUNC is a boolean, the section will be
hidden if it is t, or shown otherwise."
  (let* ((bounds (folio-section-bounds))
         (flag (if (functionp flag-or-func)
                   (and (funcall flag-or-func bounds) t)
                 flag-or-func)))
      (when bounds
        (folio-outline-section-set-hidden
         bounds flag))))

(defun folio-outline-propertize-section (section seq-num last-pos)
  "Propertize a section at point for outline processing.

SECTION is a well-known symbol indicating the type of the
section.  This symbol also should be member of
`folio-section-alist'.  SEQ-NUM is a running sequence number;
whether ascending naturally or descending is implementation
defined.  It should be unique within this section level.

Return a cons of point positions marking the beginning and end of
the first non-empty line within this heading.  An empty line
apart from the trivial is one that matches
`folio-outline-skip-regexp'."
  (let (beg head-beg head-end props)
    (folio-with-disabled-undo
      (save-excursion
        (beginning-of-line)
        (set-marker folio-outline-marker (setq beg (point)))
        (folio-outline-skip)
        (unless (eobp) ;; sanity
          (setq head-beg (line-beginning-position)
                head-end (line-end-position))
          (when (= beg (point-min))
            ;; Exclude any redundant first page separator
            (goto-char beg)
            (skip-chars-forward "\n")
            (folio-outline-skip folio-outline-skip-page-regexp)
            (setq beg (point)))
          (folio-index-section section seq-num beg last-pos props)
          ;; Propertize leading blank lines and the first non-empty line
          ;; as the chapter.
          (let ((ov (make-overlay
                     beg head-end nil nil 'rear-advance)))
            (dolist (prop `((folio-outline . t)
                            (evaporate . t)
                            (modification-hooks
                             . (folio-outline-modification-hook))
                            (face . folio-outline-section-title)
                            (help-echo . "TAB to cycle visibility")))
              (overlay-put ov (car prop) (cdr prop)))))))
    (cons head-beg head-end)))

(defun folio-outline-unpropertize (&optional beg end props)
  "Remove any text properties of the outline scanner.
BEG and END restrict the operation to a region.  If omitted the
respective buffer beginning or end position is used."
  (or beg (setq beg (point-min)))
  (or end (setq end (point-max)))
  (folio-with-disabled-undo
    (remove-overlays beg end 'folio-outline t)
    (let ((types (mapcar (lambda (x)
                           (car x))
                         folio-section-alist))
          (props (cons 'read-only props)))
      (folio-unindex-sections types beg end props))))

(defun folio-outline-process-buffer ()
  "Index document structure for use in outline views.
The actual process is just to propertize the section beginning
including the first non-empty line of a heading."
  ;; Yield for updating the display.
  (folio-yield (/ 2 (1+ folio-outline-sequence)))
  (let ((section (car (rassoc folio-outline-level
                              folio-section-alist))))
    (save-excursion
      ;; Scanning the buffer bottom-up is slightly faster because
      ;; moving forward requires two jumps for this purpose.
      (when (and folio-outline-marker
                 (= (marker-position
                     folio-outline-marker) (point-min)))
        (setq folio-outline-marker nil))
      (unless folio-outline-marker
        (folio-outline-unpropertize)
        (setq folio-outline-marker (make-marker)
              folio-outline-sequence 0
              folio-outline-headings
              (delq nil (mapcar (lambda (x)
                                  (when (/= (car x) 1)
                                    x)) folio-outline-headings)))
        (set-marker folio-outline-marker (point-max)))
      ;; Defer C-g quitting to keep marker and meta data in
      ;; sync.
      (let ((inhibit-quit nil)
            (looking-at-section-p t)
            (last-pos (marker-position
                       (goto-char folio-outline-marker)))
            (resume t) pos heading)
        (while (and (not (folio-activity-interrupted-p
                          'outline (bobp)))
                    looking-at-section-p)
          (when (setq looking-at-section-p
                      (zerop (folio-forward-section-thing
                              section -1)))
            (setq pos (folio-outline-propertize-section
                       section folio-outline-sequence last-pos)
                  heading (replace-regexp-in-string
                           folio-outline-kill-regexp ""
                           (buffer-substring-no-properties
                            (car pos) (cdr pos))))
            (push (cons folio-outline-level heading)
                  folio-outline-headings)
            (setq last-pos
                  (marker-position folio-outline-marker)
                  folio-outline-sequence
                  (1+ folio-outline-sequence))))
        (when (bobp)
          (setq folio-outline-headings
                (nreverse folio-outline-headings)
                resume nil)
          (folio-cancel-timer 'outline))
        resume))))

(defun folio-outline-cycle (&optional arg)
  "Cycle visibility of the current section."
  (interactive "P")
  (folio-outline-section-hideshow
   (lambda (x)
     (not (folio-outline-section-hidden-p x)))))


(defvar folio-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-i" 'folio-outline-cycle)
    (define-key map [(tab)] 'folio-outline-cycle)
    map)
  "Keymap for Folio outline mode.")

(defun folio-outline-mode-enable ()
  "Turn on Folio outline mode."
  (folio-schedule-timer 'outline))

(defun folio-outline-mode-disable ()
  "Turn off Folio outline mode."
  (folio-cancel-timer 'outline)
  (save-excursion
    (with-silent-modifications
      (folio-outline-unpropertize)))
  (setq folio-outline-marker nil))

;;;###autoload
(define-minor-mode folio-outline-mode
  "Toggle Folio outline mode.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  :lighter nil
  :keymap folio-outline-mode-map
  (if folio-outline-mode
      (condition-case err
          (progn
            (folio-outline-mode-enable))
        (error (message "Error enabling Folio outline mode:\n%s"
                        (cdr err)))
        (folio-outline-mode -1))
    (folio-outline-mode-disable)
    (setq folio-outline-mode nil)))


(provide 'folio-outline)

;;; folio-outline.el ends here
