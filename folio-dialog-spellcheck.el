;;; folio-dialog-spellcheck.el --- Folio project spell-checking page

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

;; Define the Folio project buffer page spell-checking.

;;; Code:


(require 'tree-widget)

(require 'folio-atoms)
(require 'folio-babel)
(require 'folio-etaoin-shrdlu)
(require 'folio-image)


(defun folio-filter-good-words (words)
  (if folio-vocabulary-good-words
      (folio-filter-list
       words (lambda (x)
               (not (cdr-safe
                     (gethash x folio-vocabulary-good-words)))))
    words))


(defun folio-widget-dict-value (&optional regexp gwl)
  (folio-with-parent-buffer
    (let ((words (folio-vocabulary-list-words)))
      (when gwl
        (setq words (folio-filter-good-words words)))
      (when (and (stringp regexp)
                 (not (string-equal regexp "")))
        (setq words
              (folio-filter-list
               (lambda (x)
                 (string-match-p regexp x)) words)))
      (sort words 'string-lessp))))

(defun folio-widget-dict-lookup (_widget word)
  "Adapt `folio-vocabulary-dict-list' for use with widgets."
  (folio-with-parent-buffer
    (folio-vocabulary-dict-list word)))

(defun folio-widget-frequency-lookup (_widget word)
  "Adapt `folio-vocabulary-word-count' for use with widgets."
  (folio-with-parent-buffer
    (folio-vocabulary-word-count word)))

(defun folio-widget-good-words-lookup (_widget word)
  "Return non-nil if WORD is in the project local `good word' list."
  (folio-with-parent-buffer
    (folio-vocabulary-good-word-p word)))

(define-widget 'folio-widget-dict-item 'push-button
  "An entry within a dictionary section."
  :value-create 'folio-widget-dict-item-value-create
  :value-to-internal 'folio-widget-dict-item-value-to-internal
  :notify 'folio-widget-dict-item-notify
  ;; :keymap folio-widget-dict-keymap ;; XXX
  :format "%[%v%]\n")

(defun folio-widget-dict-item-notify (widget _child &optional event) ;; XXX remove
  (message "XXX dict item notify value %s--event %S"
           (list (widget-get widget :dict-value)) event)
  (widget-default-notify widget _child event))

(defun folio-widget-dict-item-value-create (widget)
  "Insert text representing the `on' and `off' states."
  (let* ((tag (or (widget-get widget :tag)
                  (widget-get widget :value)))
         (tag-glyph (widget-get widget :tag-glyph))
         (text (concat (or (widget-get widget :button-prefix)
                           widget-push-button-prefix)
                       tag
                       (or (widget-get widget :button-suffix)
                           widget-push-button-suffix))))
    (if tag-glyph
        (widget-image-insert widget text tag-glyph)
      (insert text))))

(defun folio-widget-dict-item-value-to-internal (widget value)
  "Convert the widget value to internal representation.
WIDGET is the widget to convert, VALUE should be a cons of a word
suggested by a spellchecker and its occurence count in the text.
VALUE also can be a string if the widget already has been
converted."
  (cond
   ((consp value)
    (let ((word (car value))
          (count (when (and (cadr value)
                            (not (zerop (cadr value))))
                   (number-to-string (cadr value)))))
      (widget-put widget :dict-value (list word count))
      (widget-put widget :value word)
      (concat (or word "<unknown>")
              " "
              (if count
                (propertize (concat ":" count)
                            'face 'folio-frequency-tag
                            'help-echo (concat count " "
                                        (folio-pluralize
                                         "occurrence"
                                         (cadr value))
                                        " in text"))
                (propertize " -"
                            'help-echo
                            "No occurrences in text")))))
   ((stringp value)  ; Already converted.
    value)
   (t
    (error "Invalid widget value"))))


(define-widget 'folio-widget-dict-node 'tree-widget
  "A dictionary section."
  :value-create 'folio-widget-dict-node-value-create
  :keep '(:dict-value)
  :expander 'folio-widget-dict-node-expand
;;  :keymap folio-widget-dict-keymap
  :notify 'folio-widget-dict-node-notify
  :frequency-lookup 'folio-widget-frequency-lookup)

(defun folio-widget-dict-node-value-create (widget)
  "Create the widget WIDGET for a dictionary node.
The widget's value is expected in the :dict-value property.  It
should be a list with the dictionary language in the car and any
spellchecker suggestions in the cdr."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      (let* ((value (widget-get widget :dict-value))
             (tag (concat (propertize "Dictionary "
                                      'face 'folio-widget-dict)
                          (propertize (concat
                                       ":" (car value))
                                      'face 'folio-widget-dict-tag))))
        (setq node `(const
           :tag ,tag
           :format "%[%t%]\n")))
      (widget-put widget :node node)
      (widget-put widget :open
                  (widget-get (widget-get
                               widget :parent) :open))))
  (tree-widget-value-create widget))

(defun folio-widget-dict-node-expand (widget)
  "Return the child nodes of WIDGET.
Reuse :args cache if it exists."
  (or (widget-get widget :args)
      (let* ((value (widget-get widget :dict-value))
             (items (cdr value))
             children)
        (while items
          (let* ((word (car items))
                 (count (widget-apply
                         widget :frequency-lookup word)))
            (push
             `(folio-widget-dict-item
               :value ,(list word count)
               :action (lambda (widget &optional event)
                         (let ((value (car (widget-get
                                            widget :dict-value))))
                           (widget-apply
                            (widget-get widget :parent)
                            :notify widget
                            `(dict-substitute ,value)))))
             children))
          (pop items))
        (unless children
          (push '(const :tag "<no entries>") children))
        (nreverse children))))

(defun folio-widget-dict-node-notify (widget child &optional event) ;; XXX remove?
  "Pass notification to parent."
  (message "dict node notify child %S event %S" (car-safe child) (car-safe event))
  (if nil ;;(eq (car-safe event) 'substitute)
      (widget-apply (widget-get widget :parent) :notify widget event)
    (message "XXX --- dict node default action")
    (widget-default-action widget event)))

(defvar folio-widget-dict-entry-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "C-e") 'widget-end-of-line)
    (define-key map (kbd "<M-right>") 'folio-widget-dict-entry-next)
    (define-key map (kbd "<M-left>") 'folio-widget-dict-entry-previous)
    map)
  "Keymap for the dictionary widget.")

(define-widget 'folio-widget-dict-entry 'tree-widget
  ""
  :value-create 'folio-widget-dict-entry-value-create
  :keep '(:dict-value :focus-entry)
  :expander 'folio-widget-dict-entry-expand
  :notify 'folio-widget-dict-entry-notify
  :keymap folio-widget-dict-entry-keymap
  :dict-lookup 'folio-widget-dict-lookup
  :focus 'folio-widget-dict-entry-focus
  :focus-entry nil
  :frequency-lookup 'folio-widget-frequency-lookup)

(define-widget 'folio-widget-dict-entry-item 'item
  "Widget for an item in a dictionary entry.
The widget maintains a misspelled word and its frequency count."
  :value-create 'folio-widget-dict-entry-item-value-create
  :tag ""
  :format "%[%v%]\n"
  :action 'folio-widget-dict-entry-item-action
  :frequency-lookup 'folio-widget-frequency-lookup
  :good-word-lookup 'folio-widget-good-words-lookup)

(defvar folio-widget-dict-entry-choice 'accept-session
  "Save action chosen for the last dictionary entry.")
(make-variable-buffer-local 'folio-widget-dict-entry-choice)

(defun folio-widget-dict-entry-item-value-create (widget)
  (let* ((word (widget-get widget :value))
         (focused (widget-get (widget-get widget :parent) :focus-entry))
         (frequency (or (widget-apply widget :frequency-lookup word) 0))
         (gwl (when (widget-get widget :good-word-lookup)
                (widget-apply widget :good-word-lookup word)))
         (tag (concat (propertize word 'face 'folio-widget-dict-entry)
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
    (widget-insert tag)
    ;; XXX dynamically add language choice
    (when focused
      (let (buttons)
        (widget-insert
         (propertize " " 'display '(space :align-to 34)))
        (push
         (widget-create-child-and-convert
          widget 'choice
          :tag "Entry"
          :format "%[%t%] %v"
          :button-face 'custom-button
          :button-prefix ""
          :button-suffix ""
          :value 'save-local
          :value-face 'folio-widget-field
          `(const :tag ,(propertize " accept this session "
                                    'face 'folio-widget-field)
                  :format "%t" accept-session)
          `(const :tag ,(propertize " add to project dictionary "
                                    'face 'folio-widget-field)
                  :format "%t" save-local)
          `(const :tag ,(propertize " add to global dictionary "
                                    'face 'folio-widget-field)
                  :format "%t" save-global)) buttons)
        (widget-insert "  ")
        (push
         (widget-create-child-and-convert
          widget 'push-button
          :button-face 'custom-button
          :button-prefix ""
          :button-suffix ""
          :format "%[%t%]"
          :action (lambda (widget &optional event)
                    (widget-parent-action widget `(dict-apply)))
          "Apply") buttons)
        (widget-put widget :buttons buttons)))))

(defun folio-widget-dict-entry-item-action (widget &optional event)
  "Handle user initiated events."
  (message "--- XXX widget item action EVENT %S" event)
;  (message "--- XXX widget item action BUTTONS %S" (widget-get widget :buttons))
  (let ((parent (widget-get widget :parent)))
    (cond
     ((eq (car-safe event) 'dict-apply)
      (let ((buttons (widget-get widget :buttons))
            (entry (widget-get widget :dict-value))
            action)
        (mapc (lambda (x)
                (when (eq (widget-type x) 'choice)
                  (setq action (widget-get x :value)))) buttons)
        (when action
          (widget-apply
           parent :notify widget `(dict-apply ,action ,entry)))))
     ((widget-get parent :open)
      (let ((entry (widget-get widget :dict-value)))
        (widget-apply parent :notify widget `(dict-choice ,entry))))
     (t
      (widget-apply parent :notify widget 'dict-focus)))))

(defun folio-widget-dict-entry-value-create (widget)
  "Value create the widget WIDGET.
Set up a node item which tag is the dictionary entry.  The
widget :value should be a word from the text vocabulary."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      (let* ((value (widget-get widget :value))
             (frequency (or (widget-apply
                             widget :frequency-lookup value) 0))
             (tag (concat (propertize
                           value 'face 'folio-widget-dict-entry)
                          " "
                          (if (zerop frequency)
                              (propertize " -"
                                          'help-echo
                                          "No occurrences in text")
                            (let ((count (number-to-string
                                          frequency)))
                              (propertize
                               (concat ":" count)
                               'face 'folio-frequency-tag
                               'help-echo (concat count " "
                                                  (folio-pluralize
                                                   "occurrence"
                                                   frequency)
                                                  " in text")))))))
        (setq node `(folio-widget-dict-entry-item
                     :tag ""
                     :value ,value
                     :keymap ,(widget-get widget :keymap)))
        (widget-put widget :node node)
        (widget-put node :dict-value value))))
  (tree-widget-value-create widget))

(defun folio-widget-dict-entry-notify (widget child &optional event)
  "Handle a state change of WIDGET's CHILD widget."
  (message "XXX dict entry notify child %S event %S" (car-safe child) (car-safe event))
  (message "XXX dict entry from node %s is child %s" (eq (car-safe child) 'folio-widget-dict-entry-item)
           (car-safe (widget-get widget :node)))
  (if (eq (widget-type child) 'folio-widget-dict-node)
      (cond
       ((eq (car-safe event) 'dict-substitute)
        (let* ((choice (widget-get
                        (widget-get widget :node) :dict-value))
               (event (cons choice (cadr event))))
          (widget-default-notify
           widget child `(dict-substitute ,event))))
       (t
        (widget-default-notify widget child event)))
    (widget-default-notify widget child event)))

(defun folio-widget-dict-entry-expand (widget)
  "Expand the dictionary entry node widget WIDGET.
The :dict-value property should be a word from the text's
vocabulary.  The expansion creates child widgets of type
`folio-widget-dict-node' for every dictionary language.  Return the
children of WIDGET."
  ;; Reuse :args cache if it exists.
  (or (widget-get widget :args)
      (let* ((node (widget-get widget :node))
             (value (widget-get node :dict-value))
             (dicts (widget-apply widget :dict-lookup value))
             children)
        (while dicts
          (push `(folio-widget-dict-node
                  :dict-value ,(car dicts)) children)
          (pop dicts))
        (nreverse children))))

(defun folio-widget-dict-entry-focus (widget &optional arg)
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

(define-widget 'folio-widget-dict 'repeat
  ""
  :value-create 'folio-widget-dict-value-create
  :insert-before 'folio-widget-dict-insert-before
  :insert-after 'folio-widget-dict-insert-after
  :notify 'folio-widget-dict-notify
  :focus 'folio-widget-dict-focus
;; XXX  :keymap folio-widget-dict-keymap
  :format "\n%v\n"
  :entry-format "%v"
  :offset 0
  :indent 6
  :num-keys 15
  :context-keys 1)

(defun folio-widget-dict-value-create (widget)
  "XXX"
  (widget-put widget :value-pos (copy-marker (point)))
  (set-marker-insertion-type (widget-get widget :value-pos) t)
  (let* ((keys (delq nil (widget-get widget :value)))
         (i 0)
         (j (min (length keys)
                 (+ i (or (widget-get widget :num-keys) 10))))
         children)
    (while (< i j)
      (push (widget-editable-list-entry-create
             widget
             (nth i keys) t) children)
      (setq i (1+ i)))
      (if children
          (progn
            (widget-put widget :key-index (max (1- i) 0))
            (widget-put widget :children (nreverse children))
            (widget-apply widget :focus))
        (widget-create-child-and-convert widget
         `(const :tag "         <no entries>")))))


;;  (remove-hook 'folio-word-occurrence-functions 'folio-locate-word t)


(defun folio-widget-dict-notify (widget child &optional event)
  "Pass notification to parent."
  (message "dict notify XXX child %S event %S" (car-safe child) event)

  (cond
   ((eq (widget-type child) 'folio-widget-dict-entry)
    (cond
     ((eq event 'dict-focus)
      (widget-default-notify widget child event)
      (widget-apply widget :focus child))

     ((eq (car-safe event) 'dict-choice)
      (message "XXX dict notify focus child %s" (car-safe child))
      (widget-apply widget :focus child)
      (save-selected-window
        (switch-to-buffer-other-window folio-parent-buffer)
        (run-hook-with-args
         'folio-word-occurrence-functions (cadr event))))

     ((eq (car-safe event) 'dict-substitute)
      (folio-with-parent-buffer
        (run-hook-with-args 'folio-word-substitution-functions
                            (caadr event) (cdadr event))))

     ((eq (car-safe event) 'dict-apply)
      ;;dict notify XXX child folio-widget-dict-entry event (dict-apply accept-session "Alleyne")
      ;;  (widget-apply widget :focus 1)
      ;; XXX 'delete word
      (widget-apply widget :delete-at child)
      (folio-with-parent-buffer
        (run-hook-with-args 'folio-widget-dict-maintainance-functions
                            `(,(cadr event) ,(caddr event))))
      (message "XXX dict apply word %s" (caddr event))
      (folio-widget-dict-filter-value-reset)
      t)
     ((eq (car-safe event) 'dict-focus)
      (message "XXX dict notify focus 2 child %s" (car-safe child))
      (widget-apply widget :focus child))
     (t
      nil)))
   (t
    (message "XXX dict notify default")
    (widget-default-notify widget child event))))

(defun folio-widget-dict-insert-before (widget value &optional before)
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

(defun folio-widget-dict-insert-after (widget value &optional after)
  ;; Insert a new child in the list of children.
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
  ;; Ensure edit fields in buffer keep on working, for obscure reasons
  ;; and notify WIDGET self with a generic nil-event of something
  ;; having changed.
  (widget-setup)
  (widget-apply widget :notify widget))

(defun folio-widget-dict-focus (widget &optional arg)
  ;; < arg 0 scroll down
  ;; > arg 0 scroll up
  (message "XXX dict focus--arg %s" (car-safe arg))
  (let ((children (widget-get widget :children)))
    (when children
      ;; The child of an `folio-widget-dict-entry' is the `item' displaying
      ;; the word unknown or misspelled.
      (let (focus unfocus)
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
                                      'folio-widget-dict-entry) entry))
                          (elt children (min (or (widget-get
                                                  widget :context-keys) 0)
                                             (1- (length children))))))
          (widget-apply focus :focus t)))))))

(defun folio-widget-dict-scroll-down (&optional pos arg)
  (let ((widget (let ((widget (widget-at pos))
                      found)
                  (while (and (setq widget
                                    (widget-get widget :parent))
                              (not (setq found
                                         (eq (widget-type widget)
                                             'folio-widget-dict)))))
                  (and found widget))))
    (if widget
        (let ((index (widget-get widget :key-index))
              (children (widget-get widget :children)))
          (when children
            (let ((keys (widget-get widget :value)))
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
                      (widget-put widget :key-index new-index)))
                (let ((new-index (max (1- index) 0))
                      (num-keys (max (or (widget-get
                                          widget :num-keys) 0) 1)))
                  (unless (or (= index new-index)
                              (< (1+ (- new-index num-keys)) 0))
                    ;; Scroll down.
                    (widget-apply widget :delete-at
                                  (car (last children)))
                    (setq children (widget-get widget :children))
                    (widget-apply widget :insert-before
                                  (elt keys (1+ (- new-index
                                                   num-keys)))
                                  (car children))
                    (widget-apply widget :focus -1)
                    (widget-put
                     widget :key-index new-index)))))))
      (message "No scrollable widget in focus"))))

(defun folio-widget-dict-scroll-up (&optional pos arg)
  (folio-widget-dict-scroll-down pos (not arg)))

(defun folio-widget-dict-entry-next ()
  (interactive)
  (folio-widget-dict-scroll-up (point)))

(defun folio-widget-dict-entry-previous ()
  (interactive)
  (folio-widget-dict-scroll-down (point)))

(defun folio-dialog-spellcheck-page ()
  "Create the spell-checking page for the Folio mode project buffer."
  (widget-create 'const
                 :value ""
                     :format "%h"
                     :create (lambda (widget)
                               (let ((widget-documentation-face 'default))
                                 (widget-default-create widget)))
                     :doc "Find and correct misspellings.\n\
Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Donec hendrerit tempor tellus. Donec pretium posuere tellus. Proin quam nisl, tincidunt et, mattis eget, convallis nec, purus. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla posuere. Donec vitae dolor. Nullam tristique diam non turpis. Cras placerat accumsan nulla. Nullam rutrum. Nam vestibulum accumsan nisl.")

    (widget-insert "\n\n\n")
    (let* ((dicts (sort (folio-dictionary-list) 'string-lessp))
           (default-dict (car dicts)))
      (insert-char ?\s 13)

      (folio-dialog-form
       'primary-dictionary
       (widget-create 'folio-menu-choice
                      :tag "Primary dictionary"
                      :format "%[ %t   %] %v"
                      :button-face 'custom-button
                      :notify (lambda (widget &rest ignore)
                                (let ((value (widget-value widget)))
                                  (folio-with-parent-buffer
                                    (folio-change-dictionary value)))
                                (let ((secondary (folio-dialog-form-get
                                                  'secondary-dictionary)))
                                  (widget-value-set secondary
                                                    (or (remove (widget-value widget)
                                                                (widget-value secondary))
                                                        (list "<none>")))
                                  (widget-setup)))
                      :offset 14
                      :value-face 'folio-widget-field
                      :value (or (folio-with-parent-buffer
                                   (folio-primary-dictionary))
                                 (car (folio-dictionary-list)))
                      :values dicts
                      :choices (lambda (widget)
                                 (mapcar (lambda (x)
                                           (widget-convert 'const
                                                           :value-face 'folio-widget-field
                                                           :format "%v"
                                                           :value x))
                                         (widget-get widget :values)))))

      (insert-char ?\s 6)
      (widget-create 'push-button
                     :format "%[%t%]"
                     :tag-glyph (folio-create-image
                                 'xpm "refresh.xpm" '(:ascent center))
                     :tag "Refresh"
                     :notify (lambda (widget _child &optional event)
                               (let ((buffer folio-parent-buffer))
                                 (if (folio-vocabulary-build-active-p buffer)
                                     (folio-spellcheck buffer 'cancel)
                                   (folio-spellcheck buffer)
                                   (folio-widget-dict-filter-value-reset))))
                     :help-echo (lambda (_widget)
                                  (let ((buffer folio-parent-buffer))
                                    (if (folio-vocabulary-build-active-p buffer)
                                        "Push to stop spell-checking."
                                      "Push to start spell-checking."))))
      (widget-insert "\n\n")
      (folio-dialog-form 'secondary-dictionary
                         (widget-create 'repeat
                                        :insert-button-args '(:button-face custom-button)
                                        :delete-button-args '(:button-face custom-button)
                                        :append-button-args '(:button-face custom-button)
                                        :format "%v"
                                        :value '("<none>")
                                        :notify (lambda (widget &rest ignore)
                                                  (folio-with-parent-buffer
                                                    (folio-change-dictionary
                                                     (folio-primary-dictionary)
                                                     (remove "<none>" (widget-value widget)))))
                                        `(folio-menu-choice
                                          :tag "Secondary dictionary"
                                          :format "%[ %t %] %v"
                                          :value "<none>"
                                          :values ,(cons "<none>" dicts)
                                          :button-face custom-button
                                          :choices (lambda (widget)
                                                     (let ((values
                                                            (cons "<none>"
                                                                  (folio-dictionary-choices-list))))
                                                       (mapcar (lambda (x)
                                                                 (widget-convert 'const :value x))
                                                               values))))))

      (widget-insert "\n\n              Accept `good words' ")
      (folio-dialog-form 'dict-gwl
                         (widget-create 'checkbox
                                        :value nil
                                        :notify (lambda (&rest ignore)
                                                  (folio-widget-dict-filter-value-reset))))

      (widget-insert "\n\n\n\n")
      ;; Pretty much like 'regexp but validated a little differently.
      (folio-dialog-form 'dict-filter
                         (widget-create 'string
                                        :tag "Filter"
                                        :format "%t: %v"
                                        :size 14
                                        :value-face 'folio-widget-field
                                        :notify 'folio-widget-dict-filter-apply))
      (widget-insert " ")
      (widget-create 'push-button
                     :format "%[%t%]"
                     ;; :tag-glyph (find-image
                     ;;    `((:type jpg :file "delete-small.jpg" :ascend 5)))
                     :tag "Reset"
                     :button-face 'custom-button
                     :notify 'folio-widget-dict-filter-reset)

      (folio-dialog-form-rule 32)

      (folio-dialog-form 'dictionary
                         (widget-create 'folio-widget-dict
                                        :value (folio-widget-dict-value)
                                        `(folio-widget-dict-entry)))
      (widget-setup)))


(defun folio-widget-dict-filter-apply (widget child &optional event)
  (let* ((value (widget-value child))
         (regexp (when (stringp value) (folio-chomp value)))
         (gwl (widget-value-value-get (folio-dialog-form-get 'dict-gwl))))
    (if (folio-regexp-valid-p regexp)
        (let ((filtered (folio-widget-dict-value regexp gwl)))
          (when (not (string-equal value regexp))
            (widget-value-set child regexp))
          (widget-value-set
           (folio-dialog-form-get 'dictionary) filtered)
          (widget-setup))
      ;; Apparently neither `error' nor `user-error' do play well with
      ;; widgets, leaving things in a half-operational state; the use
      ;; of the widget's `:error' property is unclear.
      (message "Invalid filter expression"))))

(defun folio-widget-dict-filter-value-reset ()
  (let ((filter (folio-dialog-form-get 'dict-filter)))
    (widget-apply filter :notify filter)))

(defun folio-widget-dict-filter-reset (widget child &optional _event)
  (let* ((filter (folio-dialog-form-get 'dict-filter))
         (value (widget-value filter)))
    (when (or (null value)
              (not (stringp value))
              (not (string-equal value "")))
      (widget-value-set filter "")
      (widget-apply filter :notify filter))))


(provide 'folio-dialog-spellcheck)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-dialog-spellcheck.el ends here
