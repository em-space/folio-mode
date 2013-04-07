;;; folio-dialog-forms.el --- Dialog forms for Folio mode

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

;; This package provides the framework for maintaining the pages in
;; the Folio mode project buffer.

;;; Code:

(require 'easymenu)
(require 'info) ;; defface required at run-time
(require 'wid-edit)

(defvar folio-dialog-form-page-list nil
  "*List of dialog pages.
The `Top' page is defined by the car of this list.")

(defvar folio-dialog-form-page-history nil
  "*List maintaining interactive page navigation.")

(defvar folio-dialog-form-current-page nil
  "Current page in `folio-dialog-form-page-list'.")
(make-variable-buffer-local 'folio-dialog-form-current-page)

(defvar folio-dialog-form-alist nil
  "Alist maintaining the widgets created in the current buffer.")

(defvar folio-dialog-form-anchor-alist nil
  "Alist maintaining link anchors in the current buffer.")

;; XXX for quit--see `view-quit'
(defvar folio-dialog-form-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'folio-dialog-form-next-page)
    (define-key map "p" 'folio-dialog-form-previous-page)
    (define-key map "u" 'folio-dialog-form-top-page)
    (define-key map "t" 'folio-dialog-form-top-page)
    (define-key map "q" 'folio-dialog-form-quit)
    (define-key map "r" 'folio-dialog-form-refresh)
    (define-key map "\C-c\C-n" 'folio-dialog-form-next-page)
    (define-key map "\C-c\C-p" 'folio-dialog-form-previous-page)
    (define-key map "\C-c\C-u" 'folio-dialog-form-top-page)
    (define-key map "\C-c\C-t" 'folio-dialog-form-top-page)
    map)
  "Keymap to use in the Folio mode project buffer.")

(defvar folio-dialog-form-menu nil)
(unless folio-dialog-form-menu
  (easy-menu-define
    folio-dialog-form-menu folio-dialog-form-mode-map "Folio Project"
    `("Folio"
      ["Next Page" folio-dialog-form-next-page t]
      ["Previous Page" folio-dialog-form-previous-page t]
      ["Top" folio-dialog-form-top-page t]
      ["Refresh" folio-dialog-form-refresh t]
      "--"
      ,@(mapcar (lambda (p)
                  (vector (car p) 'folio-dialog-form-menu-goto t))
                folio-dialog-form-page-list))))

(defmacro folio-dialog-form (id &rest form)
  "Register the dialog form FORM under id ID.
The form later on can be referenced using
`folio-dialog-form-get'."
  (let ((old-form (make-symbol "folio-dialog-form")))
    `(let ((old-form (assoc ,id folio-dialog-form-alist)))
       (if old-form
           (setcdr old-form (progn ,@form))
         (setq old-form (progn ,@form))
         (push (cons ,id old-form) folio-dialog-form-alist))
       old-form)))

(defun folio-dialog-form-get (id)
  "Retrieve the dialog form with identifier ID.
The form must have been previously registered by calling
`folio-dialog-form'."
  (cdr (assoc id folio-dialog-form-alist)))

(defun folio-dialog-form-reset ()
  "Reset the buffer by erasing all content."
  (setq widget-field-list nil
        folio-dialog-form-alist nil)
  (remove-overlays)
  (erase-buffer))

;;;###autoload
(define-derived-mode folio-dialog-form-mode special-mode "Folio"
  (set (make-local-variable 'folio-dialog-form-alist) nil)
  (set (make-local-variable 'folio-dialog-form-anchor-alist) nil)
  ;; Read-only but not since input fields have to work.
  (setq buffer-read-only nil))

;;;###autoload
(defun folio-dialog-form-resolve-link (name)
  "Resolve the page link NAME.
Return a next, top and previous page tuple as a list."
  (let* ((page (assoc name folio-dialog-form-page-list))
         (rest (member page folio-dialog-form-page-list))
         (top (car folio-dialog-form-page-list))
         next previous)
    ;; If NAME resolves to top, only next is returned.
    (if (eq page top)
        (list (cadr folio-dialog-form-page-list))
      (unless (= (length rest) 1)
        (setq next (cadr rest)))
      (setq previous
            (nth (- (length folio-dialog-form-page-list)
                    (length rest)
                    1)
                 folio-dialog-form-page-list))
      (list next top previous))))

;;;###autoload
(defun folio-dialog-form-goto (link)
  "Go to the link specified by LINK."
  (interactive
   (list (completing-read
          "Goto: " folio-dialog-form-page-list
          nil t nil folio-dialog-form-page-history)))
  (setq link (split-string link "#"))
  (let* ((name (car link))
         (anchor (cadr link))
         (page (assoc name folio-dialog-form-page-list)))
    (setq folio-dialog-form-current-page name)
    (with-silent-modifications
      (folio-dialog-form-reset)
      ;; Buttons for navigation.
      (let ((links (folio-dialog-form-resolve-link name))
            (label '("Next" "Contents" "Previous")))
        (dolist (link links)
          (when link
            (widget-create 'push-button
                           :format
                           (if (string= (car label) "Contents")
                               "%[Contents%]"
                             (format "%%t: %%[%s%%]" (car link)))
                           ;; XXX maybe should be info-header-xref /
                           ;; info-header-node
                           :button-face 'info-xref
                           :tag (car label)
                           :notify (lambda (widget &rest ignore)
                                     (folio-dialog-form-goto
                                      (widget-value widget)))
                           (car link))
            (widget-insert "  "))
          (setq label (cdr label))))
      ;; Page title and header.
      (widget-insert "\n\n\n ")
      (widget-insert
       (propertize (or (plist-get page :header)
                       (car page)) 'face 'info-title-2))
      (widget-insert "\n\n")
      (funcall (cadr page))
      ;; If there is an anchor jump or otherwise move to
      ;; start of buffer.
      (if (and anchor
               (setq anchor
                     (assoc-default
                      anchor folio-dialog-form-anchor-alist)))
          (goto-char anchor)
        (goto-char (point-min)))
      (widget-setup))
    (use-local-map folio-dialog-form-mode-map)))

;;;###autoload
(defun folio-dialog-form-quit ()
  "Exit the current Folio project buffer."
  (interactive)
  (quit-window t))

;;;###autoload
(defun folio-dialog-form-top-page ()
  "Go to the top page.
This currently always is the table of contents."
  (interactive)
  (folio-dialog-form-goto "Contents"))

;;;###autoload
(defun folio-dialog-form-next-page ()
  "Go to next page."
  (interactive)
  (let ((links (folio-dialog-form-resolve-link
                folio-dialog-form-current-page)))
    (if (car links)
        (folio-dialog-form-goto (caar links))
      (message "No further page"))))

;;;###autoload
(defun folio-dialog-form-previous-page ()
  "Go to previous page."
  (interactive)
  (let ((links (folio-dialog-form-resolve-link
                folio-dialog-form-current-page)))
    (if (nth 2 links)
        (folio-dialog-form-goto (car (nth 2 links)))
      (message "No previous page"))))

;;;###autoload
(defun folio-dialog-form-refresh ()
  "Refresh the current page."
  (interactive)
  (if folio-dialog-form-current-page
      (folio-dialog-form-goto
       folio-dialog-form-current-page)
    (folio-dialog-form-top-page)))

(defalias 'folio-widget-insert 'widget-insert)

(defsubst folio-widget-indent (columns)
  "Indent by COLUMNS columns using spaces."
  (widget-insert (make-string columns ?\ )))

(defsubst folio-dialog-form-indent (columns)
  "Indent by COLUMNS columns using spaces."
  (widget-insert (make-string columns ?\ )))


;;;; Predefined pages.

;;;###autoload
(defun folio-dialog-form-contents ()
  "Create a table of contents for the Folio project buffer."
  (let ((index 1))
    (dolist (page (cdr folio-dialog-form-page-list))
      (widget-insert (format "%3d. " index))
      (widget-create 'link
                     :format "%[%t%]"
                     :tag (or (plist-get page :header) (car page))
                     :button-prefix ""
                     :button-suffix ""
                     :notify (lambda (widget &rest ignore)
                               (folio-dialog-form-goto
                                (widget-value widget)))
                     (car page))
      (widget-insert "\n")
      (setq index (1+ index)))))


;;;; Widgets.

(define-widget 'folio-menu-choice 'menu-choice
  "A dynamic dropdown list widget."
  :value-create 'folio-menu-choice-value-create
  :match 'folio-menu-choice-match
  :match-inline 'folio-menu-choice-match-inline)

(defun folio-menu-choice-match (widget value)
  (let ((choices (widget-apply widget :choices))
        current
        match)
    (while (and choices (not match))
      (setq current (car choices)
            choices (cdr choices)
            match (widget-apply current :match value)))
    match))

(defun folio-menu-choice-match-inline (widget value)
  (let ((choices (widget-apply widget :choices))
        current
        match)
    (while (and choices (not match))
      (setq current (car choices)
            choices (cdr choices)
            match (widget-match-inline current value)))
    match))

(defun folio-menu-choice-value-create (widget)
  (let ((choices (widget-apply widget :choices))
        (value (widget-get widget :value)))
    (unless value
      (widget-put widget :value (widget-value (car choices))))
    (widget-put widget :args choices)
    (widget-choice-value-create widget)))

(defun folio-widget-field-value-set (widget value)
  "Set an editable text field WIDGET to VALUE.
This defun is like `widget-field-value-set' except that it
ensures that markers do not collapse."
  (let ((from (widget-field-start widget))
        (to (widget-field-text-end widget))
        (buffer (widget-field-buffer widget)))
    (when (and from to (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char from)
        (atomic-change-group
          (insert value)
          (let ((old-len (- to from))
                (new-len (- (point) from)))
            (delete-char (max old-len 1))
            ;; retain layout even with `undo'; `undo' itself still
            ;; doesn't always work properly with `wid-edit', though
            (widget-clear-undo)
            (when (> old-len new-len)
              (insert-char ? (- old-len new-len)))))))))

(define-widget 'folio-widget-integer 'integer
  "A restricted sexp input field for an integer."
  :value-face 'folio-widget-field
  :validate 'folio-widget-integer-validate
  :notify 'folio-widget-integer-notify
  :value-set 'folio-widget-field-value-set)

(defun folio-widget-integer-validate (widget)
  "Validate the field value of WIDGET."
  (let ((value (folio-chomp (widget-apply widget :value-get))))
    ;; Reimplemented to _not_ use sexp read value parsing as with the
    ;; standard implementation.  Do not use the bogus error messages
    ;; of the latter either; this is what the widget's :type-error is
    ;; for.
    (unless (string-match-p "\\`[0-9]+\\'" value)
      widget)))

(defun folio-widget-integer-notify (widget child &optional event)
  "Handle changes to the widget WIDGET.
Validate the field value and make sure the widget never contains
an invalid value."
  (let ((invalid (widget-apply widget :validate)))
    (if invalid
        (progn
          (let* ((err (widget-get invalid :type-error))
                 (old-value (or (widget-get invalid :old-value)
                                (widget-default-get widget))))
            (message err)
            (progn
              (widget-put invalid :error nil)
                (widget-apply
                 invalid :value-set (format "%s" old-value))))
          (goto-char (widget-field-text-end invalid)))
      (let ((value (or (widget-apply widget :value-get)
                       (widget-default-get widget))))
        (widget-put widget :old-value value))))
  (set-buffer-modified-p nil)
  (widget-default-notify widget child event))

(define-widget 'folio-widget-const 'const
  "An immutable character or string valued sexp."
  :value-create 'folio-widget-const-value-create
  :format "%[%v%]\n%d")

(defun folio-widget-const-value-create (widget)
  "Insert the widget value."
  (insert (widget-get widget :value)))

(defun xxx-widget-documentation-string-indent-to (col)
  (when (and (numberp col)
             (> col 0))
    (let ((opoint (point)))
      (indent-to col)
      (put-text-property opoint (point)
                         'display `(space :align-to ,col)))))

(defun folio-dialog-form-rule (len)
  "At current point draw a horizontal rule of length LEN."
  (let ((pos (1+ (point)))
        overlay)
    (insert "\n\n")
    (put-text-property pos (1+ pos) 'face '(:underline t))
    (setq overlay (make-overlay pos (1+ pos)))
    (overlay-put overlay
                 'before-string
                 (propertize "\n" 'face '(:underline t)
                             'display `(space :align-to ,len)))
    overlay))


(provide 'folio-dialog-forms)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-dialog-forms.el ends here
