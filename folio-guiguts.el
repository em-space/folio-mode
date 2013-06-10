;;; folio-guiguts.el --- GuiGuts support for Folio mode

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

;; Import and export from and to GuiGuts bin-files.  Import and export
;; only handles essential project meta data, in particular the page
;; markers.  Because Folio mode maintains much more project auxiliary
;; data, state information and indexes, import and export never will
;; be lossless.  Import from GuiGuts is meant to ease a transition
;; into the new environment provided by Folio mode; export is meant
;; for users wishing to return to GuiGuts without losing to much work
;; after a trial period.  Export also is meant to help PPVers doing
;; their checks in GuiGuts.

;;; Code:

(require 'cl)

(require 'folio-atoms)
(require 'folio-base)
(require 'folio-core)
(require 'folio-compat)

(defgroup folio-guiguts nil
  "Customize support for GuiGuts."
  :tag "GuiGuts Support"
  :group 'folio)

(defcustom folio-guiguts-support t
  "Enable or disable general support for GuiGuts...
This option affects auto-detection of GuiGuts .bin files and
import and export of project data."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (folio-guiguts-support-enable value))
  :group 'folio-guiguts
  :require 'folio-base)

;; XXX:TODO (defcustom folio-guiguts-maintain-backup-files nil "")
;; XXX:TODO defcustom coding system for save

(defun folio-guiguts-support-enable (enable)
  "Enable or disable support for GuiGuts."
  ;; XXX edit GuiGuts import/export menu
  (if enable
      (add-hook 'folio-restore-fallback-functions 'folio-guiguts-import)
    (remove-hook 'folio-restore-fallback-functions 'folio-guiguts-import))
  enable)

;;;###autoload
(defun folio-guiguts-bin-file-name (&optional buffer)
  "Find the name of a GuiGuts .bin file.
BUFFER is the project buffer or its name."
  (let ((file-name (buffer-file-name
                    (get-buffer (or buffer (current-buffer))))))
    (when file-name
      (concat file-name ".bin"))))

;;;###autoload
(defun folio-guiguts-import-magic-p (&optional buffer ignore-mtimes)
  "Test whether or not a GuiGuts .bin file can be imported.
BUFFER is the project buffer or its name; if nil use the current
buffer.  If IGNORE-MTIMES is non-nil ignore file modification
times.

Unless the customization variable `folio-guiguts-support' says
otherwise also register GuiGuts import and export handlers.

Return the GuiGuts .bin file name on success, or nil otherwise."
  (if folio-guiguts-support
      (let* ((lhs (buffer-file-name (or buffer (current-buffer))))
             (rhs (folio-guiguts-bin-file-name buffer))
             (rhs-attrs (and rhs (file-attributes rhs))))
        (if (and lhs rhs
                 (file-readable-p rhs)
                 (eq (car rhs-attrs) nil))
            (let ((lhs-mtime (nth 5 (file-attributes lhs)))
                  (rhs-mtime (nth 5 rhs-attrs)))
              (if (and (not ignore-mtimes)
                       (< (float-time lhs-mtime)
                          (float-time rhs-mtime)))
                  nil
                (and (folio-guiguts-support-enable
                      (with-temp-buffer
                        (insert-file-contents
                         rhs nil 0 magic-mode-regexp-match-limit)
                        (goto-char (point-min))
                        (re-search-forward "^%::pagenumbers" nil t)))
                     rhs)))
          nil))))

(defvar folio-guiguts-syntax-table
  (let ((syntax-table (make-syntax-table nil)))

    ;; comment: "# ..."
    (modify-syntax-entry ?# "< " syntax-table)
    (modify-syntax-entry ?\n ">b" syntax-table)

    ;; symbol constituents
    (modify-syntax-entry ?$ "." syntax-table)
    (modify-syntax-entry ?% "." syntax-table)
    (modify-syntax-entry ?& "." syntax-table)
    (modify-syntax-entry ?= "." syntax-table)
    (modify-syntax-entry ?> "." syntax-table)

    ;; string quote
    (modify-syntax-entry ?\' "\"" syntax-table)

    ;; word: underscore, minus
    (modify-syntax-entry ?_ "w" syntax-table)
    (modify-syntax-entry ?- "w" syntax-table)
    syntax-table)
  "Syntax table for reading GuiGuts bin-files.
Those basically are serialized Perl data structures.")

(defun xxx-folio-guiguts-syntax-trim (str &optional forward backward)
  "From string STR left and right trim according to syntax.
The optional argument FORWARD lists character syntax classes to use
for left trimming; the third optional argument BACKWARD accordingly
lists character syntax classes to use for right trimming.  Both
arguments must be strings.

Return the trimmed string.  This may be nil (rather than the empty
string) if the original STR contains no other characters."
    (with-temp-buffer
      (let ((pos (point-min)))
        (insert str)
        (when forward
          (goto-char pos)
          (with-syntax-table folio-guiguts-syntax-table
            (skip-syntax-forward forward))
          (setq pos (point)))
        (goto-char (point-max))
        (when backward
          (with-syntax-table folio-guiguts-syntax-table
            (skip-syntax-backward backward)))
        (if (< pos (point))
            (buffer-substring-no-properties pos (point))
          nil))))

(defun folio-guiguts-syntax-trim (str &optional forward backward)
  "From string STR left and right trim according to syntax.
The optional argument FORWARD lists character syntax classes to
use for left trimming; the third optional argument BACKWARD
accordingly lists character syntax classes to use for right
trimming.  Both arguments must be strings.

Return the trimmed string.  This may be nil (rather than the
empty string) if the original STR contains no other characters."
  (with-syntax-table folio-guiguts-syntax-table
    (let ((from 0)
          (to (1- (length str))))
      (when forward
        (if (eq (aref forward 0) ?^)
            (while (and (<= from to)
                        (null (cl-find (char-syntax (aref str from))
                                       forward :test 'eq :start 1)))
              (setq from (1+ from)))
          (while (and (<= from to)
                      (cl-find (char-syntax (aref str from))
                               forward :test 'eq))
            (setq from (1+ from)))))
      (if (>= from to)
          nil
        (when backward
          (if (eq (aref backward 0) ?^)
              (while (and (> to from)
                          (null (cl-find (char-syntax (aref str to))
                                         backward :test 'eq :start 1)))
                (setq to (1- to)))
            (while (and (> to from)
                        (cl-find (char-syntax (aref str to))
                                 backward :test 'eq))
              (setq to (1- to)))))
        (if (>= from to)
            nil
          (substring str from (1+ to)))))))

;; XXX bookmarks, proofers (folio-guiguts-parse-sexp "\\[\\([[:digit:]]+\\)\\]")

;;;###autoload
(defun folio-guiguts-parse-sexp (key)
  "XXX docs"
  (if (re-search-forward key nil t)
      (let ((parse-sexp-ignore-comments t)
            containing-sexp)
        (save-excursion
          (with-syntax-table folio-guiguts-syntax-table
            (skip-syntax-forward " <>.")
            (forward-sexp 1)
            (let ((pos (point)))
              (forward-sexp -1)
              (setq containing-sexp
                    (buffer-substring-no-properties (point) pos))
              (setq containing-sexp
                    (folio-guiguts-syntax-trim
                     containing-sexp " .(" "). "))
              (cons (match-string-no-properties 1)
                    containing-sexp))))) nil))

;;;###autoload
(defun folio-guiguts-update-sexp (key str &rest args)
  "XXX docs"
  (if (re-search-forward key nil t)
      (let ((parse-sexp-ignore-comments t))
        (with-syntax-table folio-guiguts-syntax-table
          (skip-syntax-forward " <>.")
          (forward-sexp 1)
          (let ((pos (point)))
            (forward-sexp -1)
            (delete-region (point) pos)
            (when str
              (apply #'insert str args))
            (goto-char (point)))))
    nil))

(defun xxx-folio-guiguts-parse-pages ()
  "Parse auxillary page information from a GuiGuts .bin file,
in particular the 'pagenumbers' hash map.

The result is a list of per page cons cells of the form

   (PAGE . (OFFSET LABEL STYLE ACTION BASE)),

starting with PAGE number 1.

This function is a detail of `folio-guiguts-import' (see which)."
  (goto-char (point-min))
  (let ((page-numbers (cdr (folio-guiguts-parse-sexp "^%::pagenumbers")))
        (page-key "'Pg\\([[:digit:]]+\\)'")
        pages)
      (with-temp-buffer
        (insert page-numbers)
        (goto-char (point-min))
        (let ((page (folio-guiguts-parse-sexp page-key)))
          (while (cdr page)
            (with-temp-buffer
              (insert (cdr page))
              (goto-char (point-min))
              (let* ((keys '("'offset'" "'label'" "'style'"
                             "'action'" "'base'"))
                     (values (mapcar (lambda (x)
                                       (folio-guiguts-syntax-trim
                                        (cdr (folio-guiguts-parse-sexp x))
                                        "\"" "\"")) keys)))
                (push (cons (string-to-number (car page))
                            (list values)) pages)))
            (setq page (folio-guiguts-parse-sexp page-key)))))
      (nreverse pages)))

;;;###autoload
(defun folio-guiguts-parse-pages ()
  "Parse auxillary page information from a GuiGuts .bin file,
in particular the 'pagenumbers' hash map.

The result is a list of per page cons cells of the form

   (PAGE . (OFFSET LABEL STYLE ACTION BASE)),

starting with PAGE number 1.

This function is a detail of `folio-guiguts-import' (see which)."
  (goto-char (point-min))
  (let ((page-numbers (cdr (folio-guiguts-parse-sexp "^%::pagenumbers")))
        (page-key "'Pg\\([[:digit:]]+\\)'")
        pages)
      (with-temp-buffer
        (insert page-numbers)
        (goto-char (point-min))
        (let ((page (folio-guiguts-parse-sexp page-key)))
          ;; XXX(message "%S -> %S--%s" (car page) (cdr page) (read (car page)))
          (while (cdr page)
            (setcdr page (replace-regexp-in-string
                          "'offset'\\|'label'\\|'style'\\|'action'\\|\
'base'\\| +=>\\|,[ \n]\\|[{}]" "" (cdr page) t t))
            (setcdr page (replace-regexp-in-string
                          "'\"?'" "nil" (cdr page) t t))
            (setcdr page (replace-regexp-in-string
                          "'" "\"" (cdr page) t t))
            (setcdr page (concat "(" (cdr page) ")"))
            (push (cons (read (car page))
                        (list (read (cdr page)))) pages)
            (setq page (folio-guiguts-parse-sexp page-key)))))
      (nreverse pages)))


(defun folio-guiguts-import (&rest args)
  "XXX This function always operates on the current buffer; this
should be the buffer having the project loaded."
  (let* (page-markers
         page-label-rule
         page-scans
         (page 1)
         (pages (let ((bin-file (folio-guiguts-bin-file-name)))
                  (with-temp-buffer
                    (insert-file-contents bin-file)
                    (folio-guiguts-parse-pages))))
         (last-line 1)
         (last-col 0)
         (last-label '(0 nil nil))
         last-label-rebase
         last-label-style)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while pages
          (let ((value (cadr (folio-assoc-pop page pages))))
            (if value
                (progn
                  (let* ((offset (split-string (nth 0 value) "\\."))
                         (line (string-to-number (car offset)))
                         (col (string-to-number (cadr offset)))
                         (dl (- line last-line)))
                    (condition-case err
                        (progn
                          (vertical-motion (cons col dl))
                          (setq last-line line last-col col))
                      (end-of-buffer
                       (message "Line %d column %d: %s"
                                line col (error-message-string err))))
                    (push (point) page-markers))
                  (let ((style (nth 2 value))
                        (action (nth 3 value))
                        (base (nth 4 value))
                        expr)
                    (cond
                     ((eq style nil)
                      (when last-label-style
                        (setq style last-label-style)))
                     ((string= style "Arabic")
                      (setq style 'folio-arabic-numeral
                            last-label-style 'folio-arabic-numeral))
                     ((string= style "Roman")
                      (setq style 'folio-roman-numeral
                            last-label-style 'folio-roman-numeral))
                     (t
                      (error "GuiGuts import failed at page %d: \
bad page label style" page)))
                    (cond
                     ((or (null action) (string= action "No Count"))
                      (setq action nil style nil)
                      (when (stringp base)
                        (setq last-label-rebase (string-to-number base))))
                     ((stringp base)
                      (setq last-label-rebase (string-to-number base)
                            action last-label-rebase))
                     ((string= action "+1")
                      ;; this should accept arbitrary increments even
                      ;; though GG apparently only supports the +1 in
                      ;; the GUI
                      (unless last-label-rebase
                        ;; unsupported non-sense
                        (error "GuiGuts import failed at page %d: \
un-based page label increment action" page))
                      (setq last-label-rebase (1+ last-label-rebase))
                      (setq action (or (car last-label) last-label-rebase)))
                     (t
                      (error "GuiGuts import failed at page %d: \
unrecognized page label action `%s'" page action)))
                    ;; record transition to or from no count,
                    ;; transitions in count base, and transitions in
                    ;; numbering style
                    (let ((current-label (list action style expr)))
                      (when (not (or (equal current-label last-label)
                                     (and
                                      (= page 1)
                                      (equal current-label '(nil nil nil)))))
                        (push (cons page current-label) page-label-rule))
                      (setq last-label current-label)))
                  ;; image scans (file name of)--this (and the read
                  ;; form above) is cheating but then GuiGuts is not
                  ;; properly storing the actual file name, but uses
                  ;; some derivatives thereof for hash indices;
                  ;; folio-core will do this properly
                  (push (format "%03d.png" page) page-scans)
                  ;; XXX proofers
                  ;; XXX bookmarks
                  )
              (error "GuiGuts import failed at page %d: incomplete data" page)
              (setq pages nil))
            (setq page (1+ page))))))
    ;; XXX proofers
    ;; XXX bookmarks
    (folio-restore-page-markers (nreverse page-markers))
    (folio-restore-page-labels (cons (1- page) (nreverse page-label-rule)))
    (folio-restore-page-scans (nreverse page-scans))

    (set-buffer-modified-p t))
  t)

;; XXX TODO export stuff


(provide 'folio-guiguts)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-guiguts.el ends here
