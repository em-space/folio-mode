;;; folio-dialog-pages.el --- Folio project text structure and pages

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

;; Define the Folio project buffer page for the visualization of text
;; structure, setup of page numbering and the removal of page
;; separators.

;;; Code:

(require 'cus-edit)
(require 'wid-edit)

(require 'folio-core)
(require 'folio-dialog-forms)
(require 'folio-faces)
(require 'folio-text)

(define-widget 'folio-widget-page-rule 'group
  "A widget for editing a single page rule."
  :value-create 'folio-widget-page-rule-value-create
  :match 'folio-widget-page-rule-match
  :format "%v")

(defun folio-widget-format-page-label (folio-or-label width)
  (propertize (concat (propertize
     " " 'display `(space :width ,(- width (length
    folio-or-label)))) folio-or-label) 'face 'widget-inactive))

(defun folio-widget-page-rule-match (widget value)
  ;; Match if the value is the same.
  (equal (widget-get widget :value) value)
  (and (eq (length value) 4)
       (integerp (nth 0 value))))

(defun folio-widget-page-rule-value-create (widget)
  "Value create the page rule WIDGET."
  (let ((value (or (widget-get widget :value)
                   (list 1 "1" 1 "arabic")))
        children buttons)
    (push
     (widget-create-child-and-convert
      widget 'folio-widget-integer
      :tag "Folio"
      :help-echo "Folio and printed page number."
      :format "%t: %v"
      :value (or (nth 0 value) 1)
      :value-face 'folio-widget-field
      :size 4) children)
    (push
     (widget-create-child-and-convert
      widget 'const :tag " " :format "%t") buttons)
    (push
     (widget-create-child-and-convert
      widget 'folio-widget-const
      :tag " "
      :help-echo "Printed page number."
      :format "%[%v%]"
      :size 6
      :button-prefix ""
      :button-suffix ""
      :button-face 'widget-inactive
      :value (nth 1 value)
      :value-to-internal (lambda (widget value)
                           (let ((val (or value
                                          folio-blank-page-tag))
                                 (size (widget-get widget :size)))
                             (folio-widget-format-page-label
                              val size)))
      :value-to-external (lambda (_widget value)
                           (substring-no-properties value))
      :notify (lambda (widget &rest ignore)
                (let ((parent (widget-get widget :parent)))
                  (when parent
                    (widget-apply parent :notify
                                  parent '(label-goto)))))) children)
    (push
     (widget-create-child-and-convert
      widget 'choice
      :tag "Type"
      :help-echo "Choose numbering style."
      :format "  %[ %t %]: %v"
      :button-face 'custom-button
      :button-prefix ""
      :button-suffix ""
      :value (nth 2 value)
      :notify (lambda (widget child &optional event)
                (let ((parent (widget-get widget :parent)))
                  (when parent
                    (let ((source (car-safe child))
                          (value (widget-get widget :value)))
                      (cond
                       ((eq source 'folio-widget-integer)
                        (widget-apply parent :notify parent
                                      `(label-number ,(widget-value child))))
                       ((eq source 'choice)
                        (widget-apply parent :notify
                                      parent `(label-type ,value)))
                       (t
                        (widget-default-notify widget child event)))))))
      '(const
        :tag "Blind"
        :format "%t"
        :help-echo "Blind page without printed page number."
        nil)
      ;; XXX TODO `Number Format' as in 'A-12' where '12' is read as both the numeric value of the page label
      `(folio-widget-integer
        :tag "Numeral"
        :help-echo "Page number."
        :format "%t: %v"
        :value ,(or (nth 2 value) 1)
        :old-value ,(or (nth 2 value) 1)
        :size 6)) children)
    ;; Hack! The const separator prevents the editable-field's end
    ;; marker to jump into the choice item, for whatever reasons.
    (push
     (widget-create-child-and-convert
      widget 'const :tag " " :format "%t") buttons)
    (folio-widget-insert "\n")
    (folio-widget-indent 48)
    (unless (null (nth 2 value))
      (push
       (widget-create-child-and-convert
        widget 'choice
        :tag "Style"
        :help-echo "Choose numbering style."
        :format "%t: %v\n"
        :button-face 'custom-button
        :notify (lambda (widget &rest ignore)
                  (let ((parent (widget-get widget :parent)))
                    (when parent
                      (widget-apply
                       parent :notify parent
                       `(label-style ,(widget-value widget))))))
        :value (nth 3 value)
        '(choice-item
          :tag "Arabic"
          :help-echo "Arabic numerals."
          :format "%[%t%]"
          folio-arabic-numeral)
        '(choice-item
          :tag "roman "
          :help-echo "Roman numerals (minuscule)."
          :format "%[%t%]"
          folio-roman-numeral-minuscule)
        '(choice-item
          :tag "ROMAN "
          :format "%[%t%]"
          :help-echo "Roman numerals (majuscule)."
          folio-roman-numeral-majuscule)) children))
    (widget-put widget :children (nreverse children))
    (widget-put widget :buttons buttons)
    children))

(define-widget 'folio-widget-page-rules 'repeat
  "A widget for editing the page numbering rule."
  :tag "Page numbering"
  :format "%{%t%}:\n\n%v  %i\n"
  :entry-format "  %i %d   %v\n"
  :insert-button-args '(:button-face custom-button)
  :delete-button-args '(:button-face custom-button)
  :append-button-args '(:button-face custom-button)
  :notify 'folio-widget-page-rules-notify
  :args '(folio-widget-page-rule))

(defun folio-widget-page-rules-changed (widget)
  "Signal an update to the page label rule."
  (let ((page-count (folio-with-parent-buffer
                      (folio-count-pages)))
        (value (widget-value widget))
        rule)
    (mapc (lambda (x)
            (push (list (nth 0 x) (nth 2 x) (nth 3 x)) rule))
          value)
    (folio-with-parent-buffer
      (condition-case-unless-debug err
          (progn
            (run-hook-with-args
             'folio-page-label-changed-functions
             (cons page-count (nreverse rule)))
            (message "Page labels updated"))
        (error (message "Invalid page label definition"))))))

(defun folio-widget-page-rules-notify (widget child &optional event)
  "Handle notification event EVENT from child widget CHILD."
  (let ((value (widget-value child))
        (event-type (car-safe event)))
    (cond
     ((eq event-type 'label-goto)
      (folio-with-parent-buffer
        (save-selected-window
          (switch-to-buffer-other-window (current-buffer))
          (folio-goto-page (nth 0 value))
          (recenter)))
      (widget-default-notify widget child event))
     ((or (eq event-type 'label-number)
          (eq event-type 'label-style))
      (let* ((style (nth 3 value))
             (number (nth 2 value))
             (label (cond
                     ((eq style 'folio-arabic-numeral)
                      (format "%s" number))
                     ((eq style 'folio-roman-numeral-minuscule)
                      (downcase (folio-arabic-to-roman number)))
                     (t
                      (folio-arabic-to-roman number)))))
        (widget-value-set
         (nth 1 (widget-get child :children)) label))
        (widget-default-notify widget child event))
     ((eq event-type 'label-type)
      (save-excursion
        (if (null (cadr event)) ;; blank
            (widget-value-set
             child `(,(or (nth 0 value) 1) nil nil nil))
          (widget-value-set
           child `(,(or (nth 0 value) 0) "1" 1 folio-arabic-numeral)))
        (widget-setup)
        (widget-default-notify widget child event)))
     (t
      (widget-default-notify widget child event))))
  (folio-widget-page-rules-changed widget))

(defun folio-dialog-pages-widget-value ()
  "Determine the pages widget value."
  (let ((rule (cdr folio-page-label-rule))
        pages)
    (if rule
        (mapc (lambda (x)
                (let ((page (nth 0 x))
                      (label (nth 1 x))
                      (style (nth 2 x))
                      entry)
                    (cond
                     ((null label)
                      (setq entry (list page nil nil nil)))
                     ((numberp label)
                      (setq entry
                            (list page
                                  (format "%d" label)
                                  label
                                  style))))
                    (setq pages (cons entry pages)))) rule)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((blanks (folio-forward-blank-page
                           (point-max-marker))))
              (unless (memq 1 blanks)
                (setq pages '((1 "1" 1 folio-arabic-numeral))))
              (mapc (lambda (x)
                      (let* ((page (folio-page-at-point (car x)))
                             (next (unless (memq (1+ page) blanks)
                                     (1+ page))))
                        (if pages
                            (push `(,page nil nil nil) pages)
                          (setq pages `((,page nil nil nil))))
                        (when next
                          (push `(,next "1" 1 folio-arabic-numeral)
                                pages))))
                    blanks)))))
      (nreverse pages)))

(defun folio-widget-page-join-apply ()
  "Timer function for joining pages."
  (save-selected-window
    (switch-to-buffer-other-window folio-parent-buffer)
    (folio-join-pages)))

(folio-define-timer 'join-pages
    "Timer for joining pages."
  :function 'folio-widget-page-join-apply
  :repeat nil
  :buffer-local t
  :secs 0)

(defun folio-dialog-pages-page ()
  "Create the dialog for page setup."
  ;; XXX Outline
  (widget-insert "\n\n")
  (widget-create 'const
                 :value ""
                 :format "%h"
                 :create (lambda (widget)
                           (let ((widget-documentation-face 'default))
                             (widget-default-create widget)))
                 :doc "Verify text structure, setup pagination and cleanup page separators.\n\
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.")

  (widget-insert "\n\n\n")
  (folio-dialog-form
   'pages (widget-create 'folio-widget-page-rules
                         :value (folio-with-parent-buffer
                                  (folio-dialog-pages-widget-value))))
  (insert-char ?\s 45)
  (widget-create 'push-button
                 :format "%[%t%]"
                 :tag (format "%15s " "Join pages")
                 :button-face 'custom-button
                 :notify (lambda (&rest _ignore)
                           (unless (folio-timer-running-p 'join-pages)
                             (folio-schedule-timer 'join-pages)))
                 :help-echo (lambda (_widget)
                              "Push to remove page separators."))
  (widget-setup))



(provide 'folio-dialog-pages)

;;; folio-dialog-pages.el ends here
