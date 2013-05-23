;;; folio-dialog-vocabulary.el --- Word frequency analysis for Folio mode

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

;;

;;; Code:

(require 'tree-widget)

(require 'folio-dialog-forms)


(defun folio-widget-vocabulary-value (&optional regexp)
  "Return the value for the vocabulary widget.
If the regexp REGEXP is non-nil filter out any words in the
vocabulary not matching.  If the GWL widget is toggled on filter
out any word that is in the `good word' list."
  (let* ((filters (when (widget-value-value-get
                         (folio-dialog-form-get 'dict-gwl))
                    '(good-words)))
         (words (folio-with-parent-buffer
                  (folio-vocabulary-list 'lexicographic filters))))
    (when (and (stringp regexp)
               (not (string-equal regexp "")))
      (setq words
            (folio-filter-list
             words (lambda (x)
                     (string-match-p regexp x)))))
    words))

(defun folio-widget-vocabulary-frequency-lookup (_widget word)
  "Adapt `folio-vocabulary-word-count' for use with widgets."
  (folio-with-parent-buffer
    (folio-vocabulary-word-count word)))

(defun folio-widget-vocabulary-soundslike-lookup (_widget word)
  "Return similar spellings of WORD in the vocabulary.
This is an adaption of `folio-soundslikes' for use with the
`folio-widget-vocabulary' widget."
  (folio-with-parent-buffer
    (remove word (folio-soundslikes word))))

(defun folio-widget-vocabulary-good-words-lookup (_widget word)
  "Return non-nil if WORD is in the project local `good word' list."
  (folio-with-parent-buffer
    (folio-vocabulary-good-word-p word)))

(defvar folio-widget-vocabulary-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "C-e") 'widget-end-of-line)
    (define-key map (kbd "<M-right>")
      'folio-widget-vocabulary-entry-next)
    (define-key map (kbd "<M-left>")
      'folio-widget-vocabulary-entry-previous) map)
  "Keymap for the vocabulary widget.")

(define-widget 'folio-widget-vocabulary-item 'item
  "Widget for an item in a vocabulary entry.
The widget maintains a word and its frequency count as a button."
  :value-create 'folio-widget-vocabulary-item-value-create
  :tag ""
  :format "%[%v%]\n"
  :action 'folio-widget-vocabulary-item-action
  :frequency-lookup 'folio-widget-vocabulary-frequency-lookup
  :good-word-lookup 'folio-widget-vocabulary-good-words-lookup)

(defun folio-widget-vocabulary-item-value-create (widget)
  "Value create the widget WIDGET.
WIDGET should be of the type `folio-widget-vocabulary-item'."
  (let* ((word (widget-get widget :value))
         (frequency (or (widget-apply
                         widget :frequency-lookup word)
                        0))
         (gwl (when (widget-get widget :good-word-lookup)
                (widget-apply widget :good-word-lookup word)))
         (tag (concat (propertize word 'face 'folio-dict-entry)
                      " "
                      (if (zerop frequency)
                          (propertize " -" 'help-echo
                                      "No occurrences in text")
                        (let ((count (number-to-string frequency)))
                          (propertize
                           (concat ":" count)
                           'face 'folio-frequency-tag
                           'help-echo (concat count " "
                                              (folio-pluralize
                                               "occurrence" frequency)
                                              " in text"))))
                      (when gwl
                        (concat " "
                                (propertize ":gwl"
                                            'face 'folio-gwl-tag
                                            'help-echo
                                            "Listed in `good words'"))))))
    (widget-insert tag)))

(defun folio-widget-vocabulary-item-action (widget &optional event)
  "Handle user initiated events."
  (let ((parent (widget-get widget :parent)))
    (cond
     ((widget-get parent :open)
      (let ((value (widget-get widget :value)))
        (widget-apply
         parent :notify widget `(vocabulary-choice ,value))))
     (t
      (widget-apply parent :notify widget 'vocabulary-focus)))))

(define-widget 'folio-widget-soundslike-node 'tree-widget
  "A sounds-like section in the vocabulary widget."
  :value-create 'folio-widget-soundslike-node-value-create
  :keep '(:soundslike-value)
  :expander 'folio-widget-soundslike-node-expand
  :keymap folio-widget-vocabulary-keymap
;;  :notify 'folio-widget-soundslike-node-notify
  :frequency-lookup 'folio-widget-vocabulary-frequency-lookup)

(defun folio-widget-soundslike-node-notify (widget _child &optional event) ;; XXX remove
  (message "XXX vocabulary item notify value %s--event %S"
           (list (widget-get widget :vocabulary-value)) event)
  (widget-default-notify widget _child event))

(defun folio-widget-soundslike-node-value-create (widget)
  "Value create the widget WIDGET for a vocabulary sounds-like node.
The widget's value is expected in the :vocabulary-value
property."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      (let* ((value (widget-get widget :vocabulary-value))
             (tag (propertize "Similar spellings"
                              'face 'folio-soundslike)))
        (setq node `(const :tag ,tag
                           :format "%[%t%]\n")))
      (widget-put widget :node node)
      (widget-put widget :open
                  (widget-get (widget-get
                               widget :parent) :open))))
  (tree-widget-value-create widget))

(defun folio-widget-soundslike-node-expand (widget)
  "Expand the tree-node widget WIDGET.
For words with spellings similar to the
widget's :vocabulary-value assign child nodes of type
`folio-widget-vocabulary-item'.  Return the child nodes of
WIDGET."
  (or (widget-get widget :args) ;; re-use cache if exists
      (let* ((value (widget-get widget :vocabulary-value))
             ;; sort-order possibly should have a control widget or
             ;; defcustom setting
             (items (folio-widget-vocabulary-soundslike-lookup
                     widget value))
             children)
        (while items
          (let* ((word (car items))
                 (count (widget-apply
                         widget :frequency-lookup word)))
            (push `(folio-widget-vocabulary-item
                    :value ,word) children))
          (pop items))
        (unless children
          (push '(const :tag "<no entries>") children))
        (nreverse children))))

(define-widget 'folio-widget-vocabulary-entry 'tree-widget
  "An entry in the vocabulary widget."
  :value-create 'folio-widget-vocabulary-entry-value-create
  :keep '(:vocabulary-value :focus-entry)
  :expander 'folio-widget-vocabulary-entry-expand
  :notify 'folio-widget-vocabulary-entry-notify
  :keymap folio-widget-vocabulary-keymap
  :focus 'folio-widget-vocabulary-entry-focus
  :focus-entry nil)

(defun folio-widget-vocabulary-entry-value-create (widget)
  "Value create the widget WIDGET.
Set up a node item which tag is the dictionary entry.  The
widget :value should be a word from the text vocabulary."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      (let* ((value (widget-get widget :value)))
        (setq node `(folio-widget-vocabulary-item
                     :value ,value
                     :keymap ,(widget-get widget :keymap)))
        (widget-put widget :node node)
        (widget-put node :vocabulary-value value))))
  (tree-widget-value-create widget))

(defun folio-widget-vocabulary-entry-notify (widget child &optional event)
  "Handle a state change of WIDGET's CHILD widget."
  (if (eq (widget-type child) 'folio-widget-soundslike-node)
      (cond
       ((eq (car-safe event) 'dict-substitute) ;; XXX replace/all
        (let* ((choice (widget-get
                        (widget-get widget :node) :vocabulary-value))
               (event (cons choice (cadr event))))
          (widget-default-notify
           widget child `(dict-substitute ,event))))
       (t
        (widget-default-notify widget child event)))
    (widget-default-notify widget child event)))

(defun folio-widget-vocabulary-entry-expand (widget)
  "Expand the vocabulary entry node widget WIDGET.
The :vocabulary-value property should be a word from the text's
vocabulary.  The expansion creates child widgets of type
`folio-widget-soundslike-node' for every dictionary language.
Return the children of WIDGET."
  ;; Reuse :args cache if it exists.
  (or (widget-get widget :args)
      (let* ((node (widget-get widget :node))
             (value (widget-get node :vocabulary-value))
             children)
        (push `(folio-widget-soundslike-node
                :vocabulary-value ,value) children)
        children)))

(defun folio-widget-vocabulary-entry-focus (widget &optional arg)
  "Set focus for WIDGET according to ARG."
  (widget-put widget :focus-entry (and arg t))
  ;; Expand when in focus or otherwise collapse the node by calling
  ;; the tree-widget's action function.
  (unless (eq (and (widget-get widget :open) t) (and arg t))
    (widget-apply widget :action))
  ;; Position cursor at the beginning of the node item.
  (when arg
    (goto-char (widget-get
                (car (widget-get widget :children)) :from))))

(define-widget 'folio-widget-vocabulary 'folio-widget-repeat
  "A scrollable widget for word and word frequency checks."
  :notify 'folio-widget-vocabulary-notify
;; XXX  :keymap folio-widget-vocabulary-keymap
  :offset 0
  :indent 6)

(defun folio-widget-vocabulary-notify (widget child &optional event)
  "Handle notification event EVENT from child widget CHILD."
  (cond
   ((eq (widget-type child) 'folio-widget-vocabulary-entry)
    (cond
     ((eq event 'vocabulary-focus)
      (widget-default-notify widget child event)
      (widget-apply widget :focus child))
     ((eq (car-safe event) 'vocabulary-choice)
      (save-selected-window
        (switch-to-buffer-other-window folio-parent-buffer)
        (run-hook-with-args
         'folio-word-occurrence-functions (cadr event)))
      ;; (re-)focus the child up-stack that originally produced the
      ;; event
      (widget-apply widget :focus child))
     (t nil)))
   (t
    (widget-default-notify widget child event))))

(defun folio-widget-vocabulary-entry-next ()
  "Scroll the word-widget focusing on the next entry."
  (interactive)
  (folio-widget-repeat-scroll-up
   'folio-widget-vocabulary (point)))

(defun folio-widget-vocabulary-entry-previous ()
  "Scroll the widget focusing on the previous entry."
  (interactive)
  (folio-widget-repeat-scroll-down
   'folio-widget-vocabulary (point)))


(defconst folio-widget-vocabulary-filter-alist
  '(("<none>" . nil)
    ("Upper Case" . upper-case)
    ("Lower Case" . lower-case)
    ("Numeric" . numeric)
    ("Alpha-Numeric" . alpha-numeric)))

(define-widget 'folio-widget-vocabulary-filter 'folio-menu-choice
  "A widget for setting a predefined word filter."
  :format "%v"
  :value `,(caar folio-widget-vocabulary-filter-alist)
  :values 'folio-widget-vocabulary-filter-values
  :choices 'folio-widget-vocabulary-filter-choices
  :button-face custom-button)

(defun folio-widget-vocabulary-filter-choices (widget)
  (mapcar (lambda (x)
            (widget-convert 'const
                            :value-face 'folio-widget-field
                            :format "%v"
                            :value x))
          (widget-apply widget :values)))

(defun folio-widget-vocabulary-filter-values (widget)
  (let* ((form (folio-dialog-form-get 'vocabulary-filters))
         (selected (when form
                     (widget-get form :choices)))
         available)
    (mapc (lambda (x)
            (unless (member (car x) selected)
              (push (car x) available)))
          folio-widget-vocabulary-filter-alist)
    (setq available (nreverse available))
    (unless (memq (caar folio-widget-vocabulary-filter-alist)
                  available)
      (push (caar folio-widget-vocabulary-filter-alist)
            available))
    available))

(define-widget 'folio-widget-vocabulary-filters 'repeat
  "A widget for specifying predefined word filters."
  :tag "Quick filters"
  :format "%{%t%}:\n\n%v  %i\n"
  :entry-format "  %i %d   %v\n"
  :value `(,(caar folio-widget-vocabulary-filter-alist))
  :insert-button-args '(:button-face custom-button)
  :delete-button-args '(:button-face custom-button)
  :append-button-args '(:button-face custom-button)
  :notify 'folio-widget-vocabulary-filters-notify
  :args '(folio-widget-vocabulary-filter))

(defun folio-widget-vocabulary-filters-notify (widget child
                                               &optional event)
  "Pass notification to parent."
  (widget-put widget :choices (widget-value widget)))

(defun folio-dialog-vocabulary-page ()
  "Create the dialog page for the word frequency analysis."
  (widget-create 'const
                 :value ""
                 :format "%h"
                 :create (lambda (widget)
                           (let ((widget-documentation-face 'default))
                             (widget-default-create widget)))
                 :doc "Identify and locate spelling inconsistencies, typographic errors,\n\
exotic or erroneous characters.

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.")

  (widget-insert "\n\n\n")
  (folio-dialog-form 'vocabulary-filters
                     (widget-create 'folio-widget-vocabulary-filters
                                    `(folio-widget-vocabulary-filter
                                        :tag "Filter"
                                        :format "%[ %v %]")))
  (widget-insert "\n\n\n")
  ;; Pretty much like 'regexp but validated a little differently.
  (folio-dialog-form 'vocabulary-filter-regexp
                     (widget-create 'string
                                    :tag "Filter"
                                    :format "%t: %v"
                                    :size 14
                                    :value-face 'folio-widget-field
                                    :notify 'folio-widget-vocabulary-filter-apply))
  (widget-insert " ")
  (widget-create 'push-button
                 :format "%[%t%]"
                 ;; :tag-glyph (find-image
                 ;;    `((:type jpg :file "delete-small.jpg" :ascend 5)))
                 :tag "Reset"
                 :button-face 'custom-button
                 :notify 'folio-widget-vocabulary-filter-reset)

  (folio-dialog-form-rule 32)

  (folio-dialog-form 'vocabulary
                     (widget-create 'folio-widget-vocabulary
                                    :value (folio-widget-vocabulary-value)
                                    `(folio-widget-vocabulary-entry)))
  (widget-setup))

(defun folio-widget-vocabulary-filter-apply (widget child
                                                    &optional _event)
  (let* ((value (widget-value child))
         (regexp (when (stringp value) (folio-chomp value))))
    (if (folio-regexp-valid-p regexp)
        (let ((filtered (folio-widget-vocabulary-value regexp)))
          (when (not (string-equal value regexp))
            (widget-value-set child regexp))
          (widget-value-set
           (folio-dialog-form-get 'vocabulary) filtered)
          (widget-setup))
      ;; Apparently neither `error' nor `user-error' do play well with
      ;; widgets, leaving things in a half-operational state; the use
      ;; of the widget's `:error' property is unclear.
      (message "Invalid filter expression"))))

(defun folio-widget-vocabulary-filter-value-reset ()
  (let ((filter (folio-dialog-form-get 'vocabulary-filter-regexp)))
    (widget-apply filter :notify filter)))

(defun folio-widget-vocabulary-filter-reset (widget child
                                                    &optional _event)
  (let* ((filter (folio-dialog-form-get 'vocabulary-filter-regexp))
         (value (widget-value filter)))
    (when (or (null value)
              (not (stringp value))
              (not (string-equal value "")))
      (widget-value-set filter "")
      (widget-apply filter :notify filter))))



(provide 'folio-dialog-vocabulary)

;;; folio-dialog-vocabulary.el ends here
