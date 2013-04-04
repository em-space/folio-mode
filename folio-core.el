;;; folio-core.el --- Folio mode core

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

;; The Folio mode core is what all major Folio mode text modes are
;; built around.  The core in particular is dedicated to the hud, page
;; tracking, and import and export of essential document meta data
;; that is not already handled by the `folio-base' package.

;;; Code:

(require 'cl)

(require 'folio-atoms)
(require 'folio-base)
(require 'folio-roman)

(defvar folio-page-markers nil
  "*List of page markers.
The page marker or number is equivalent to the folio number.  It
is also referred to as the technical page number.")
(make-variable-buffer-local 'folio-page-markers)

(defvar folio-scan-from-page nil
  "Associates page number to page scan image.")
(make-variable-buffer-local 'folio-scan-from-page)

(defvar folio-page-label-rule nil
  "A list defining the numbering of page labels.")

(defvar folio-label-from-page nil
  "Caches the page label for each page.")
(make-variable-buffer-local 'folio-label-from-page)

(defvar folio-page-from-label nil
  "Inverted index to `folio-label-from-page'.")
(make-variable-buffer-local 'folio-page-from-label)

(defvar folio-proofer-from-page nil
  "Caches proofer and foofer names for each page.")
(make-variable-buffer-local 'folio-proofer-from-page)

(defvar folio-text-author nil
  "The e-text author in UTF-8 plain text.
The variable normally is set from the author element of
`folio-pgdp-dc', see which.")
(make-variable-buffer-local 'folio-text-author)

(defvar folio-text-title nil
  "The e-text title in UTF-8 plain text.
The variable normally is set from the title element of
`folio-pgdp-dc', see which.")
(make-variable-buffer-local 'folio-text-title)

;;;###autoload
(defun folio-project-auto-magic-p ()
  "Auto mode magic predicate for folio-mode project files.
The predicate condition is satisfied if at the beginning of the
buffer file certain syntactic elements can be identifed.  The
look-ahead is restricted by the variable
`magic-mode-regexp-match-limit'."
  (let ((file (folio-save-file-name)))
    (if (and file (file-readable-p file))
        (with-temp-buffer
          (insert-file-contents
           file nil 0 magic-mode-regexp-match-limit)
          (goto-char (point-min))
          (and (re-search-forward
                "^;; Emacs folio-mode Project File" nil t) t))
      nil)))

(defun folio-save-page-markers ()
  "Marshaller function for page markers as cached by the
buffer-local `folio-page-markers' variable at run-time for use
with `folio-save-hook' (which see)."
  (let ((positions ()))
    (mapc (lambda (marker)
            (push (marker-position marker) positions))
          folio-page-markers)
    (nreverse positions)))

(defun folio-restore-page-markers (positions)
  "Demarshaller function for page markers as cached by the
`folio-page-markers' variable for use with `folio-restore-hook'.
The inverse operation is defined by `folio-save-page-markers'."
  (let ((buffer (current-buffer)) markers)
    (mapc (lambda (pos)
            (let ((marker (make-marker)))
              (set-marker marker pos buffer)
              ;; advance marker if text is inserted at marker
              ;; position
              ;; (set-marker-insertion-type marker t)
              (push marker markers))) positions)
    (setq folio-page-markers (vconcat (nreverse markers)))
    (add-to-list 'folio-save-list 'folio-page-markers)))

;; register page markers as project state variable with persistence
(put 'folio-page-markers
     'folio-save-value 'folio-save-page-markers)
(put 'folio-page-markers
     'folio-restore-value 'folio-restore-page-markers)

(defun folio-restore-page-labels (rule)
  "Demarshaller function for page labels as cached by the
buffer-local variable `folio-label-from-page' at run-time for use
with `folio-save-hook' (which see).  The reverse operation is the
identity.  This function also should be called whenever the page
label rule has changed."
  (let ((page-labels (folio-page-label-rule-unfold rule)))
    (setq folio-page-label-rule rule
          folio-label-from-page (vconcat page-labels))
    ;; inverted index
    (if folio-page-from-label
        (clrhash folio-page-from-label) ; discard all existing entries
      (setq folio-page-from-label (make-hash-table :test 'equal)))
    (let ((page 1))
      (mapc
       (lambda (x)
         (unless (null x)
           (puthash x page folio-page-from-label))
         (setq page (1+ page))) folio-label-from-page))
    (add-to-list 'folio-save-list 'folio-page-label-rule)))

;; register page labels as project state variable with persistence
(put 'folio-page-label-rule
     'folio-restore-value 'folio-restore-page-labels)

(defun folio-page-label-at-page (&optional page)
  "Return the page label (folio number) for PAGE.
PAGE is the technical page number.  If omitted assume the current
page at point."
  (interactive "nPage number: ")
  (let ((page (max 0 (1- (or page (folio-page-at-point))))))
    (unless (or (null folio-label-from-page)
                (> page (length folio-label-from-page)))
      (aref folio-label-from-page page))))

(defun folio-save-page-scans ()
  "Marshaller function for page scan file names as cached by the
buffer-local `folio-scan-from-page' variable at run-time for use
with `folio-save-hook' (which see)."
  (coerce folio-scan-from-page 'list))

(defun folio-restore-page-scans (scans)
  "Demarshaller function for page scan file names as cached by the
buffer-local `folio-scan-from-page' variable at run-time for use
with `folio-save-hook' (which see)."
  (setq folio-scan-from-page (coerce scans 'vector))
  (add-to-list 'folio-save-list 'folio-scan-from-page))

(put 'folio-scan-from-page
     'folio-save-value 'folio-save-page-scans)
(put 'folio-scan-from-page
     'folio-restore-value 'folio-restore-page-scans)

(defgroup folio-hud nil
  "Settings related to graphical elements."
  :tag "Folio HUD"
  :group 'folio
  :version "24.1")

(defcustom folio-show-header-line t
  "Display header line when activating the mode.
The header line also can be enabled or disabled interactively by
calling the function `folio-header-enable'."
  :tag "Folio Show header line"
  :type 'boolean
  :group 'folio-hud)

(defcustom folio-blank-page-tag (string #x25AF)
  "The header line symbol indicating a blank page.
The default is the Unicode code point WHITE VERTICAL RECTANGLE
#x25AF, displayed as `▯'."
  :tag "Folio Blank page tag"
  :type 'string
  :group 'folio-hud)

(defun folio-header-line ()
  "Return a propertized string for display in the header line."
;; XXX cache frame width, last page--last header line
;; XXX activity token
  (let* ((line "Page ")
         (page (folio-page-at-point (point) 'as-list))
         (label (folio-page-label-at-page (car page)))
         (debug-on-error t))
    (if (zerop (car page))
        (setq line (propertize (concat line "<unknown>")
                               'help-echo
                               "No page information available"))
      (setq line (concat (propertize
                          (concat line
                                  (cond
                                   ((null (cdr page))
                                    (format "%d" (car page)))
                                   (t
                                    (format "%d %s"
                                            (car page) (cdr page)))))
                          'help-echo "Folio")
                         (cond
                          ((null label) ; none assigned
                           ;; XXX mouse goto assignment
                           (propertize " *" 'help-echo
                                       "No page labels assigned"))
                          ((eq label t) ; blank page
                           (propertize (format ": %s"
                                               folio-blank-page-tag)
                                       'help-echo
                                       "Blank or unlabeled page"))
                          (t
                           (propertize (format ": %s" label)
                                       'help-echo
                                       "Folio label"))))))
      (concat (propertize " "
               'display `((space :align-to
                                 (- right-margin 2 ,(length line)))))
              line)))

;; XXX face from defcustom

(defvar folio-old-header-line-format nil
  "Buffer local variable for storing the previous mode header
line format, such as '(:eval (tabbar-line).")

(defun folio-header-enable (&optional arg)
  "Enable the Folio mode header line.
The header line is used to display information about current
activities and state like page number, label and other
information for the current point.  When called with a negative
prefix argument restore the previous header line, if any."
  (interactive "p")
  (setq arg (or arg 1))
  (unless folio-old-header-line-format
    (set (make-local-variable 'folio-old-header-line-format)
         header-line-format))
  (if (not (natnump arg))
      (setq header-line-format folio-old-header-line-format)
    (setq header-line-format '(:eval (folio-header-line))))
  (redraw-display)
  t)

(defun folio-page-location (page)
  "Retrieve the point position for the technical page number PAGE.
The argument must be in the range [0, N-1].

See also `folio-page-at-point'."
  (marker-position (elt folio-page-markers page)))

(defsubst folio-first-page ()
  "Return the technical page number of the first page."
  0)

(defsubst folio-last-page ()
  "Return the technical page number of the last page."
  (1- (length folio-page-markers)))

(defsubst folio-count-pages ()
  "Return the number of pages."
  (length folio-page-markers))

(defun folio-page-region (&optional page)
  "Return the region occupied by the page with number PAGE.
The result is a cons cell of the form (BEG . END)."
  (let* ((page (or page (folio-page-at-point)))
         (beg (folio-page-location (max 0 (1- page))))
         (end (1- (folio-page-location
                   (min (folio-last-page) (1+ page))))))
    (cons beg end)))

(defun folio-page-at-point (&optional point as-list)
  "Find and return the technical page number for the buffer
position POINT as an integer beginning with 1.

For point positions before the actual beginning of the first
page, or if no page information is available, 0 is returned, never
nil.  If called without argument, the current position of point
is used.

If the optional argument AS-LIST is non-nil, return the page as a
list including any pages sharing the same buffer location.
Colliding page locations are possible for one page if the
previous page is a blank page and page separators have been
removed.

See also `folio-page-location', and `folio-page-scan-separators'."
  (interactive)
  (let ((point (or point (point)))
        (debug-on-error t))
    (cond
     ((or (not (boundp 'folio-page-markers))
          (zerop (length folio-page-markers))
          (< point (folio-page-location 0)))
      (if as-list
          (list 0)
        0))
     ((>= point (folio-page-location
                 (1- (length folio-page-markers))))
      (if as-list
          (list (length folio-page-markers))
        (length folio-page-markers)))
     (t
      (let ((page (1+ (folio-upper-bound
                       point folio-page-markers
                       :value-extract #'marker-position))))
        (if as-list
          (let ((previous (- page 2))
                (pos (marker-position
                      (aref folio-page-markers (1- page)))))
            (setq page (list page))
            (while (and (>= previous 0)
                        (= pos
                           (marker-position
                            (aref folio-page-markers previous))))
              (setq page (cons (1+ previous) page)
                    previous (1- previous)))
            (nreverse page))
          (when (called-interactively-p 'any)
            (message "Page %d" page))
          page))))))

(defun folio-page-label-rule-unfold (rule &optional num-pages)
  "Evaluate the compact form of a page label RULE."
  ;; as with (folio-page-label-rule-unfold
  ;;    '(307 (4 1 folio-arabic-numeral nil)))
  (let* ((num-pages (or num-pages (car rule)))
         (rule (cdr (if (symbolp rule) (symbol-value rule) rule)))
         (lower (pop rule))
         (page-labels (make-vector num-pages nil)))
    (while lower
      (let* ((page (1- (car lower)))
             (label (cadr lower))
             (label-style (caddr lower))
             (upper (car rule))
             (page-upper (if upper
                             (min (1- (car upper)) num-pages)
                           num-pages)))
        (cond
         ((numberp label)
          (cond
           ((eq label-style 'folio-arabic-numeral)
            (while (< page page-upper)
              (aset page-labels page (format "%d" label))
              (setq page (1+ page))
              (setq label (1+ label))))
           ((eq label-style 'folio-roman-numeral)
            (while (< page page-upper)
              (aset page-labels page
                    (downcase (folio-arabic-to-roman label)))
              (setq page (1+ page))
              (setq label (1+ label))))
           (t
            (error "Unsupported page label style `%s'"
                   label-style))))
         ((eq label nil)
          (setq page (1- page-upper))
          t)
         (t
          (error "Page label style neither \
numeric nor constant string")))
        (setq lower upper))
      (pop rule))
    page-labels))

(defun folio-page-scan-at-point (&optional point)
  "Return the image file name of the page scan at POINT.
If POINT is omitted use current point.  Return nil if not known."
  (interactive)
  (let ((page (folio-page-at-point (or point (point)))))
    (if (or (zerop page) (null folio-scan-from-page))
        (progn
          (when (called-interactively-p 'any)
            (message "Page scan is unknown"))
          nil)
      (let ((scan (aref folio-scan-from-page (1- page))))
        (when (called-interactively-p 'any)
          (message "Scan \"%s\"" scan))
        scan))))

(defun folio-assign-page-to-point (page &optional point)
  "Assign the page number PAGE.
If POINT is omitted use the current point."
  (interactive (list (read-number
                      "Assign page: " (folio-page-at-point))
                     (if current-prefix-arg
                         (read-number "Point: ")
                       nil)))
  (unless folio-page-markers
    (error "Insufficient page information available"))
  (let* ((marker (aref folio-page-markers (1- page)))
         (old-pos (marker-position marker))
         (new-pos (or point (point))))
    (unless (eq old-pos (marker-position
                         (set-marker marker new-pos)))
      (set-buffer-modified-p t))))

(defun folio-assign-page-scan-to-point (file-name &optional point)
  "Set the image file name of the page scan at POINT.
If POINT is omitted use current point.

Return nil if point happens to be before the first page, or t
otherwise."
  (interactive "sFile name (sans directory): ")
  (unless folio-scan-from-page
    (error "Insufficient page information available"))
  (let ((page (folio-page-at-point point)))
    (if (zerop page)
        nil
      (aset folio-scan-from-page (1- page) file-name))))

(defun folio-goto-page (page)
  "Go to the technical page number PAGE.
Numbering starts at 1. `folio-goto-folio' should be used to jump to
a particular folio label instead."
  (interactive "nPage number: ")
  (let ((page (1- (max (min page
                            (length folio-page-markers)) 1))))
    (goto-char (folio-page-location page))))

(defun folio-goto-folio (folio)
  "Go to the folio with label FOLIO.
FOLIO is a string and case matters, i.e. \"iiv\" and \"IIV\" are
two different labels.  `folio-goto-page' should be used to jump
to the technical page number instead.  This function cannot be
used to go to blank pages."
  (interactive "sFolio label: ")
  (unless folio-page-from-label
    (error "Insufficient page information available"))
  (let ((page (gethash folio folio-page-from-label)))
    (if page
        (goto-char (folio-page-location page))
      (when (folio-called-interactively-p 'any)
        (folio-user-error "Folio `%s' not found" folio))
      nil)))

;; XXX TODO folio-goto-next-blank-page

;; (defun folio-page-separators-find-file (file)
;;   "XXX Read the contents of a file into a temp buffer and then do
;;  something there."
;; ;;  (interactive "fFile: \nr")
;;   (when (file-readable-p file)
;;     (with-temp-buffer
;;       (insert-file-contents file)
;;       (folio-page-scan-separators (current-buffer)))))

(defun folio-core-setup ()
  "Initialize the core of a Folio mode."
  (when folio-show-header-line
    (folio-header-enable)))

(defun folio-core-teardown ()
  "Shutdown the core of a Folio mode."
  (folio-header-enable -1))


(provide 'folio-core)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-core.el ends here
