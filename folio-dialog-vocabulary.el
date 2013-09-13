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

(require 'folio-base)
(require 'folio-dialog-forms)
(require 'folio-etaoin-shrdlu)
(require 'folio-time)

(defcustom folio-vocabulary-filter-delay 0.24
  "Time in seconds to wait before updating the vocabulary view."
  :group 'folio-technical
  :tag "Folio refresh delay for the vocabulary view"
  :type 'number)

(folio-define-timer 'vocabulary-filter
    "Idle timer for refreshing the vocabulary widget."
  :function 'folio-widget-vocabulary-filter-apply
  :repeat nil
  :buffer-local t
  :secs (lambda () folio-vocabulary-filter-delay))

(defvar folio-widget-vocabulary-entry-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap
          widget-keymap folio-dialog-form-mode-map))
    (define-key map (kbd "C-e") 'widget-end-of-line)
    (define-key map (kbd "<wheel-down>")
      'folio-widget-vocabulary-entry-next)
    (define-key map (kbd "<wheel-up>")
      'folio-widget-vocabulary-entry-previous)
    (define-key map (kbd "<M-right>")
      'folio-widget-vocabulary-entry-next)
    (define-key map (kbd "<M-left>")
      'folio-widget-vocabulary-entry-previous)
    (define-key map [down-mouse-1]
      'widget-move-and-invoke)
    map)
  "Keymap for the entry of the vocabulary widget.")

(defvar folio-widget-vocabulary-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap
          widget-keymap folio-dialog-form-mode-map))
    (define-key map (kbd "<wheel-down>")
      'folio-widget-vocabulary-entry-next)
    (define-key map (kbd "<wheel-up>")
      'folio-widget-vocabulary-entry-previous)
    ;; XXX "fast" scroll by jumping to lexicographic next/previous
    ;; initial letter
    ;; (define-key map (kbd "<S-wheel-down>") 'folio-widget-dict-entry-next)
    ;; (define-key map (kbd "<S-wheel-up>") 'folio-widget-dict-entry-previous
    map)
  "Keymap for the vocabulary widget.")

(defun folio-widget-vocabulary-current-word ()
  "Retrieve the current/previously selected word for the vocabulary view."
  (let ((scope (cond ((memq (eval folio-sync-current-word)
                            '(all dictionary))
                      'dictionary)
                     ((eq (eval folio-sync-current-word)
                          'vocabulary)
                      'vocabulary)
                     (t 'vocabulary))))
  (folio-with-parent-buffer
    (folio-vocabulary-current-word scope))))

(defun folio-widget-vocabulary-set-current-word (word)
  "Adapt `folio-vocabulary-set-current-word' for use with the
vocabulary widget.  WORD is the key currently in focus."
  (folio-with-parent-buffer
   (folio-vocabulary-set-current-word word 'vocabulary)))

(defun folio-widget-vocabulary-value (&optional regexp filters)
  "Return the value for the vocabulary widget.
If the regexp REGEXP is non-nil filter out any words in the
vocabulary not matching.  If the GWL widget is toggled on filter
out any word that is in the `good word' list."
  (when (and (stringp regexp)
             (not (string-equal regexp "")))
    (setq filters (cons regexp filters)))
  (let ((words (folio-with-parent-buffer
                 (folio-vocabulary-list 'lexicographic filters)))
        (current-word (folio-widget-vocabulary-current-word)))
    (if (and current-word (member current-word words))
        (cons words current-word)
      words)))

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

(define-widget 'folio-widget-vocabulary-item 'item
  "Widget for an item in a vocabulary entry.
The widget maintains a word and its frequency count as a button."
  :create 'folio-widget-vocabulary-item-create
  :value-create 'folio-widget-vocabulary-item-value-create
  :delete 'folio-widget-vocabulary-item-delete
  :tag ""
  :format "%[%v%]\n"
  :action 'folio-widget-vocabulary-item-action
  :keymap folio-widget-vocabulary-entry-keymap
  :frequency-lookup 'folio-widget-vocabulary-frequency-lookup
  :good-word-lookup 'folio-widget-vocabulary-good-words-lookup)

(defun folio-widget-vocabulary-item-create (widget)
  "Create WIDGET at point in the current buffer."
  (widget-default-create widget)
  (let ((from (widget-get widget :from))
        (to (widget-get widget :to))
        (keymap (widget-get widget :keymap)))
    (when keymap
      (let ((overlay (make-overlay
                      from to nil nil 'rear-sticky)))
        (widget-put widget :keymap-overlay overlay)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'local-map keymap)))))

(defun folio-widget-vocabulary-item-value-create (widget)
  "Value create the widget WIDGET.
WIDGET should be of the type `folio-widget-vocabulary-item'."
  (let* ((word (widget-get widget :value))
         (frequency (or (widget-apply
                         widget :frequency-lookup word)
                        0))
         (gwl (when (widget-get widget :good-word-lookup)
                (widget-apply widget :good-word-lookup word)))
         (tag (concat (propertize
                       (folio-left-to-right-safe word)
                       'face 'folio-dict-entry)
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

(defun folio-widget-vocabulary-item-delete (widget)
  "Remove WIDGET from the buffer."
  (let ((keymap-overlay (widget-get widget :keymap-overlay)))
    (when keymap-overlay
      (delete-overlay keymap-overlay))
    (widget-default-delete widget)))

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
  :frequency-lookup 'folio-widget-vocabulary-frequency-lookup)

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
  :focus 'folio-widget-vocabulary-entry-focus
  :focus-entry nil
  :keymap folio-widget-vocabulary-entry-keymap)

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
                     :value ,value))
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
    (let ((current-word (widget-get (widget-get widget :node)
                                    :vocabulary-value)))
      (folio-widget-vocabulary-set-current-word current-word))
    (goto-char (widget-get
                (car (widget-get widget :children)) :from))))

(define-widget 'folio-widget-vocabulary 'folio-widget-repeat
  "A scrollable widget for word and word frequency checks."
  :notify 'folio-widget-vocabulary-notify
  :keymap folio-widget-vocabulary-keymap
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
         'folio-word-occurrence-functions (cadr event))))
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

(defconst folio-widget-vocabulary-sfilter-alist
  (let ((init '("<any>" . nil)))
    (cons init
          (sort (copy-sequence folio-script-alist)
                (lambda (x y)
                  (folio-uca-lessp (car x) (car y))))))
  "*List of script names for the script filter menu choice.")

(define-widget 'folio-widget-vocabulary-script-filter 'folio-menu-choice
  "A widget for setting a predefined script filter."
  :format "%v"
  :value `,(caar folio-widget-vocabulary-sfilter-alist)
  :values 'folio-widget-vocabulary-sfilter-values
  :notify 'folio-widget-vocabulary-sfilter-notify
  :button-face 'custom-button)

(defun folio-widget-vocabulary-sfilter-values (widget)
  (let* ((form (folio-dialog-form-get 'vocabulary-script-filter))
         (selected (when form
                     (widget-get form :choice)))
         available)
    (mapc (lambda (x)
            (unless (member (car x) selected)
              (push (car x) available)))
          folio-widget-vocabulary-sfilter-alist)
    (setq available (nreverse available))
    (unless (member (caar folio-widget-vocabulary-sfilter-alist)
                    available)
      (push (caar folio-widget-vocabulary-sfilter-alist)
            available))
    available))

(defun folio-widget-vocabulary-sfilter-notify (widget child
                                                      &optional event)
  "Handle notifications for the quick filter widget."
  (folio-schedule-timer 'vocabulary-filter))

(defconst folio-widget-vocabulary-qfilter-alist
  '(("<none>" . nil)
    ("Upper Case" . upper-case)
    ("Lower Case" . lower-case)
    ("Title Case" . title-case)
    ("Numeric" . numeric)
    ("Alpha-Numeric" . alpha-numeric)
    ("Diacritics" . diacritics)))

(define-widget 'folio-widget-vocabulary-qfilter 'folio-menu-choice
  "A widget for setting a predefined word filter."
  :format "%v"
  :value `,(caar folio-widget-vocabulary-qfilter-alist)
  :values 'folio-widget-vocabulary-qfilter-values
  :button-face 'custom-button)

(defun folio-widget-vocabulary-qfilter-values (widget)
  (let* ((form (folio-dialog-form-get 'vocabulary-quick-filters))
         (selected (when form
                     (widget-get form :choices)))
         available)
    (mapc (lambda (x)
            (unless (member (car x) selected)
              (push (car x) available)))
          folio-widget-vocabulary-qfilter-alist)
    (setq available (nreverse available))
    (unless (member (caar folio-widget-vocabulary-qfilter-alist)
                    available)
      (push (caar folio-widget-vocabulary-qfilter-alist)
            available))
    available))

(define-widget 'folio-widget-vocabulary-qfilters 'repeat
  "A widget for specifying predefined word filters."
  :tag "Quick filters"
  :format "%{%t%}:\n\n%v  %i\n"
  :entry-format "  %i %d   %v\n"
  :value `(,(caar folio-widget-vocabulary-qfilter-alist))
  :insert-button-args '(:button-face custom-button)
  :delete-button-args '(:button-face custom-button)
  :append-button-args '(:button-face custom-button)
  :notify 'folio-widget-vocabulary-qfilters-notify
  :args '(folio-widget-vocabulary-qfilter))

(defun folio-widget-vocabulary-qfilters-notify (widget child
                                                       &optional event)
  "Handle notifications for the quick filter widget."
  (folio-schedule-timer 'vocabulary-filter))

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
  (folio-dialog-form 'vocabulary-script-filter
                     (widget-create '(folio-widget-vocabulary-script-filter
                                      :tag "Script"
                                      :format "%t:  %[ %v %]")))
  (widget-insert "\n\n\n")
  (folio-dialog-form 'vocabulary-quick-filters
                     (widget-create 'folio-widget-vocabulary-qfilters
                                    `(folio-widget-vocabulary-qfilter
                                        :tag "Filter"
                                        :format "%[ %v %]")))
  (widget-insert "\n\n\n")
  ;; Pretty much like 'regexp but validated a little differently.
  (folio-dialog-form
   'vocabulary-regexp-filter (widget-create
                              'string :tag "Filter" :format "%t: %v" :size 14
                              :value-face 'folio-widget-field
                              :notify 'folio-widget-vocabulary-rfilter-notify))
  (widget-insert " ")
  (widget-create 'push-button
                 :format "%[%t%]"
                 ;; :tag-glyph (find-image
                 ;;    `((:type jpg :file "delete-small.jpg" :ascend 5)))
                 :tag "Reset"
                 :button-face 'custom-button
                 :notify 'folio-widget-vocabulary-rfilter-reset)

  (folio-dialog-form-rule 32)

  (folio-dialog-form
   'vocabulary (widget-create 'folio-widget-vocabulary
                              :value (folio-widget-vocabulary-value)
                                     `(folio-widget-vocabulary-entry)))
  (widget-setup))

(defun folio-widget-vocabulary-filter-apply ()
  "Update the vocabulary widget to the current filter settings."
  (let ((swidget (folio-dialog-form-get 'vocabulary-script-filter))
        (qwidget (folio-dialog-form-get 'vocabulary-quick-filters))
        (rwidget (folio-dialog-form-get 'vocabulary-regexp-filter)))
    (when (and swidget qwidget rwidget) ;; unlikely race-condition
      (let* ((old-svalue (widget-get swidget :filter-value))
             (old-qvalue (widget-get qwidget :filter-value))
             (old-rvalue (widget-get rwidget :filter-value))
             (rvalue (widget-value rwidget))
             new-svalue new-qvalue new-rvalue)
        (setq new-svalue
              (cdr (assoc (widget-value swidget)
                          folio-widget-vocabulary-sfilter-alist))
              new-qvalue (delq nil
                               (mapcar (lambda (x)
                                         (cdr (assoc x folio-widget-vocabulary-qfilter-alist)))
                                       (widget-value qwidget))))
        (when (stringp rvalue)
          (setq new-rvalue (folio-chomp rvalue)))
        (when (and (folio-regexp-valid-p new-rvalue)
                   (or (not (equal old-svalue new-svalue))
                       (not (equal old-rvalue new-rvalue))
                       (not (equal old-qvalue new-qvalue))))
          (let* ((folio-vocabulary-script new-svalue)
                 (filtered (folio-widget-vocabulary-value
                            new-rvalue (if new-svalue
                                           (cons 'script new-qvalue)
                                         new-qvalue))))
            (widget-value-set
             (folio-dialog-form-get 'vocabulary) filtered))
          (widget-put qwidget :filter-value new-qvalue)
          (widget-put qwidget :filter-value new-qvalue)
          (widget-put rwidget :filter-value new-rvalue)
          (widget-setup))))))

(defun folio-widget-vocabulary-rfilter-notify (widget child &optional event)
  "Handle notification event EVENT from child widget CHILD."
  (let* ((old-value (widget-get widget :filter-value))
         (value (widget-value child))
         (new-value (when (stringp value) (folio-chomp value))))
    (when (folio-timer-running-p 'vocabulary-filter)
      (folio-cancel-timer 'vocabulary-filter))
    (if (folio-regexp-valid-p new-value)
        (unless (string-equal old-value new-value)
          (folio-schedule-timer 'vocabulary-filter))
      ;; Apparently neither `error' nor `user-error' do play well with
      ;; widgets, leaving things in a half-operational state; the use
      ;; of the widget's `:error' property is unclear.
      (message "Invalid filter expression"))))

(defun folio-widget-vocabulary-rfilter-reset (widget child &optional _event)
  "Reset the regexp filter widget and update views."
  (let* ((filter (folio-dialog-form-get 'vocabulary-regexp-filter))
         (value (widget-value filter)))
    (when (or (null value)
              (not (stringp value))
              (not (string-equal value "")))
      (widget-value-set filter "")
      (folio-widget-vocabulary-filter-apply))))


(provide 'folio-dialog-vocabulary)

;;; folio-dialog-vocabulary.el ends here
