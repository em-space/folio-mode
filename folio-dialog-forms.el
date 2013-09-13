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

(require 'folio-atoms)

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
      (widget-setup)
      ;; If there is an anchor jump or otherwise move to
      ;; start of buffer.
      (if (and anchor
               (setq anchor
                     (assoc-default
                      anchor folio-dialog-form-anchor-alist)))
          (goto-char anchor)
        (goto-char (point-min))))
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
  :default-get 'folio-menu-choice-default-get
  :mouse-down-action 'folio-menu-choice-mouse-down-action
  :choices 'folio-menu-choices
  :action 'folio-menu-choice-action
  :match 'folio-menu-choice-match
  :match-inline 'folio-menu-choice-match-inline)

(defun folio-menu-choices (widget)
  (mapcar (lambda (x)
            (widget-convert 'const
                            :value-face 'folio-widget-field
                            :format "%v"
                            :value x))
          (widget-apply widget :values)))

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
  "Insert the first choice that matches the value."
  (let ((value (widget-get widget :value))
        (args (widget-apply widget :choices))
        (explicit (widget-get widget :explicit-choice))
        current)
    (if explicit
        (progn
          ;; If the user specified the choice for this value,
          ;; respect that choice.
          (widget-put widget :children
                      (list (widget-create-child-value
                             widget explicit value)))
          (widget-put widget :choice explicit)
          (widget-put widget :explicit-choice nil))
      (while args
        (setq current (car args)
              args (cdr args))
        (when (widget-apply current :match value)
          (widget-put widget :children
                      (list (widget-create-child-value
                             widget current value)))
          (widget-put widget :choice current)
          (setq args nil
                current nil)))
      (when current
        (let ((void (widget-get widget :void)))
          (widget-put widget :children
                      (list (widget-create-child-and-convert
                             widget void :value value)))
          (widget-put widget :choice void))))))

(defun folio-menu-choice-default-get (widget)
  ;; Get default for the first choice.
  (widget-default-get
   (car (widget-apply widget :values))))

(defun folio-menu-choice-mouse-down-action (widget &optional _event)
  ;; Return non-nil if we need a menu.
  (let ((args (widget-apply widget :values))
        (old (widget-get widget :choice)))
    (cond ((not (display-popup-menus-p))
           ;; No place to pop up a menu.
           nil)
          ((< (length args) 2)
           ;; Empty or singleton list, just return the value.
           nil)
          ((> (length args) widget-menu-max-size)
           ;; Too long, prompt.
           nil)
          ((> (length args) 2)
           ;; Reasonable sized list, use menu.
           t)
          ((and widget-choice-toggle (memq old args))
           ;; We toggle.
           nil)
          (t
           ;; Ask which of the two.
           t))))

(defun folio-menu-choice-action (widget &optional event)
  ;; Make a choice.
  (let ((args (widget-apply widget :choices))
        (old (widget-get widget :choice))
        (tag (widget-apply widget :menu-tag-get))
        (completion-ignore-case (widget-get widget :case-fold))
        this-explicit
        current choices)
    ;; Remember old value.
    (if (and old (not (widget-apply widget :validate)))
        (let* ((external (widget-value widget))
               (internal (widget-apply
                          old :value-to-internal external)))
          (widget-put old :value internal)))
    ;; Find new choice.
    (setq current
          (cond ((= (length args) 0)
                 nil)
                ((= (length args) 1)
                 (nth 0 args))
                ((and widget-choice-toggle
                      (= (length args) 2)
                      (memq old args))
                 (if (eq old (nth 0 args))
                     (nth 1 args)
                   (nth 0 args)))
                (t
                 (while args
                   (setq current (car args)
                         args (cdr args))
                   (setq choices
                         (cons (cons (widget-apply
                                      current :menu-tag-get)
                                     current)
                               choices)))
                 (setq this-explicit t)
                 (widget-choose tag (reverse choices) event))))
    (when current
      ;; If this was an explicit user choice, record the choice,
      ;; so that widget-choice-value-create will respect it.
      (when this-explicit
        (widget-put widget :explicit-choice current))
      (widget-value-set widget (widget-default-get current))
      (widget-setup)
      (widget-apply widget :notify widget event)))
  (run-hook-with-args 'widget-edit-functions widget))

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

(define-widget 'folio-widget-repeat 'repeat
  "A scrollable list widget."
  :create 'folio-widget-repeat-create
  :value-create 'folio-widget-repeat-value-create
  :delete 'folio-widget-repeat-delete
  :insert-before 'folio-widget-repeat-insert-before
  :insert-after 'folio-widget-repeat-insert-after
  :notify 'folio-widget-repeat-notify
  :focus 'folio-widget-repeat-focus
  :format "\n%v\n"
  :entry-format "%v"
  :value-no-entry "         <no entries>"
  :offset 0
  :indent 6
  :num-entries 15
  :context-entries 1)

(defun folio-widget-repeat-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-default-create widget)
  (let ((from (widget-get widget :from))
        (to (widget-get widget :to))
        (keymap (widget-get widget :keymap)))
    (when keymap
      (let ((overlay (make-overlay
                      from to nil nil 'rear-sticky)))
        (widget-put widget :keymap-overlay overlay)
        (overlay-put overlay 'local-map keymap)))))

(defun folio-widget-repeat-value-create (widget)
  "Value create the widget WIDGET."
  (widget-put widget :value-pos (copy-marker (point)))
  (set-marker-insertion-type (widget-get widget :value-pos) t)
  (let* ((value (widget-get widget :value))
         (keys (if (listp (car value)) (car value) value))
         (current-key (when (listp (car value)) (cdr value)))
         (current-index 0)
         last-key i j children)
    ;; If the car of the widget's value is a list of items assume it's
    ;; cdr to be the item currently selected and put this at position
    ;; two, leaving a one-item context at position one.
    (when current-key
      (setq last-key (car keys))
      (while (not (equal (car keys) current-key))
        (setq last-key (car keys)
              keys (cdr keys)
              current-index (1+ current-index)))
      (unless (equal last-key (car keys))
        (setq keys (cons last-key keys))
        (setq current-index (1+ current-index))))
    (setq i 0
          j (min (length keys)
                 (+ i (or (widget-get widget :num-entries) 10))))
    (while (< i j)
      (push (widget-editable-list-entry-create
             widget (nth i keys) t) children)
      (setq i (1+ i)))
    (if children
        (progn
          (widget-put widget :entry-index
                      (max (1- (+ i current-index)) 0))
          (widget-put widget :children (nreverse children))
          (widget-apply widget :focus))
      (widget-create-child-and-convert
       widget `(const :tag ,(widget-get
                             widget :value-no-entry))))))

(defun folio-widget-repeat-delete (widget)
  "Remove WIDGET from the buffer."
  (let ((keymap-overlay (widget-get widget :keymap-overlay)))
    (when keymap-overlay
      (delete-overlay keymap-overlay))
    (widget-default-delete widget)))

(defun folio-widget-repeat-insert-before (widget value &optional before)
  "This is like `widget-editable-list-insert-before' except that a
list entry is created by value."
  ;; Insert a new child in the list of children.
  (save-excursion
    (let ((children (widget-get widget :children))
          (inhibit-read-only t)
          before-change-functions
          after-change-functions)
      (cond (before
             (goto-char (widget-get before :entry-from)))
            (t
             (goto-char (widget-get widget :value-pos))))
      ;; Ensure child always is created relative to the beginning of
      ;; the current line.
      (beginning-of-line)
      (let ((child (widget-editable-list-entry-create
                    widget value (not (null value)))))
        (when (< (widget-get child :entry-from)
                 (widget-get widget :from))
          (set-marker (widget-get widget :from)
                      (widget-get child :entry-from)))
        (if (eq (car children) before)
            (widget-put widget :children (cons child children))
          (while (not (eq (cadr children) before))
            (setq children (cdr children)))
          (setcdr children (cons child (cdr children)))))))
  (widget-setup)
  (widget-apply widget :notify widget))

(defun folio-widget-repeat-insert-after (widget value &optional after)
  "Insert a new child in the list of children."
  (save-excursion
    (let ((children (widget-get widget :children))
          (inhibit-read-only t)
          before-change-functions
          after-change-functions)
      ;; Position point.
      (cond (after
             (goto-char (widget-get after :entry-to)))
            (t
             (if children
                 (goto-char (widget-get
                             (car (last children)) :entry-to))
               (goto-char (widget-get widget :value-pos)))))
      ;; Ensure child always is created relative to the beginning of
      ;; the current line.
      (beginning-of-line)
      ;; Insert child widget.
      (let ((new-child (widget-editable-list-entry-create
                        widget value (not (null value)))))
        ;; Fixup end marker of parent WIDGET.
        (if (null (widget-get widget :to))
            (widget-put widget
                        :to (copy-marker (widget-get
                                          new-child :entry-to)))
          (if (> (widget-get new-child :entry-to)
                 (widget-get widget :to))
              (set-marker (widget-get widget :to)
                          (widget-get new-child :entry-to))))
        ;; Register new child widget with parent WIDGET.
        (if (eq (car (last children)) after)
            (widget-put
             widget :children (nconc children (list new-child)))
          (while (not (eq (cadr children) after))
            (setq children (cdr children)))
          (setcdr children (cons new-child (cdr children)))))))
  ;; Ensure edit fields in buffer keep on working, for obscure
  ;; reasons.  Also notify WIDGET self with a generic nil-event of
  ;; something having changed.
  (widget-setup)
  (widget-apply widget :notify widget))

(defun folio-widget-repeat-focus (widget &optional arg)
  "Re-focus the widget WIDGET after a scrolling event.
Re-focusing means to call the :focus function of the respective
child widget.  If ARG is nil assume no particular scrolling
direction and determine the child widget from :context-entries.
Otherwise re-focus for scrolling down if ARG < 0 or for scrolling
up if ARG > 0."
  (let ((children (widget-get widget :children)))
    (when children
      (let ((child-type (widget-type (car children)))
            focus unfocus)
        (mapc (lambda (x)
                (unless unfocus
                  (when (widget-get x :focus-entry)
                    (setq unfocus x)))
                (if (numberp arg)
                  (if (< arg 0)
                      (unless unfocus
                        (setq focus x))
                    (when (and unfocus
                               (null focus)
                               (not (eq unfocus x)))
                      (setq focus x)))
                  (unless focus
                    (setq focus arg)))) children)
        (when unfocus
          (widget-apply unfocus :focus nil))
        (cond
         ((and focus unfocus)
          (widget-apply focus :focus t))
         (t
          (setq focus (or (let ((entry (widget-get
                                        (widget-at (point)) :parent)))
                            (when (eq (widget-type entry)
                                      child-type) entry))
                          (elt children (min (or (widget-get
                                                  widget :context-entries) 0)
                                             (1- (length children))))))
          (widget-apply focus :focus t)))))))

(defun folio-widget-repeat-scroll-down (widget-type &optional pos arg)
  "Scroll down the widget of type WIDGET-TYPE at buffer position POS.
WIDGET-TYPE should be a scrollable widget like 'folio-repeat.  If
ARG is non-nil scroll up instead."
  (let ((widget (let ((widget (widget-at (or pos (point))))
                      found)
                  (while (and (setq widget
                                    (widget-get widget :parent))
                              (not (setq found
                                         (eq (widget-type widget)
                                             widget-type)))))
                  (and found widget)))
        (notify-eol t))
    (if widget
        (let ((index (widget-get widget :entry-index))
              (children (widget-get widget :children)))
          (when children
            (let* ((value (widget-get widget :value))
                   (keys (if (listp (car value)) (car value) value)))
              (if arg
                  (let ((new-index (min (1+ index)
                                        (1- (length keys)))))
                    (unless (= index new-index)
                      ;; Scroll up.
                      (widget-apply
                       widget :delete-at (car children))
                      (widget-apply
                       widget :insert-after (nth new-index keys))
                      (widget-apply widget :focus 1)
                      (widget-put widget :entry-index new-index)
                      (setq notify-eol nil)))
                (let ((new-index (max (1- index) 0))
                      (num-entries (max (or (widget-get
                                             widget :num-entries) 0) 1)))
                  (unless (or (= index new-index)
                              (< (1+ (- new-index num-entries)) 0))
                    ;; Scroll down.
                    (widget-apply widget :delete-at
                                  (car (last children)))
                    (setq children (widget-get widget :children))
                    (widget-apply widget :insert-before
                                  (elt keys (1+ (- new-index
                                                   num-entries)))
                                  (car children))
                    (widget-apply widget :focus -1)
                    (widget-put
                     widget :entry-index new-index)
                    (setq notify-eol nil))))
              (when notify-eol
                (message "No more entries")))))
      (message "No scrollable widget in focus"))))

(defun folio-widget-repeat-scroll-up (widget-type &optional pos arg)
  "Scroll up the widget of type WIDGET-TYPE.
The arguments have the same meaning like for
`folio-widget-repeat-scroll-down'."
  (folio-widget-repeat-scroll-down widget-type pos (not arg)))


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
    (overlay-put overlay 'evaporate t)
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
