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

(require 'cus-edit)

(require 'tree-widget)

(require 'folio-atoms)
(require 'folio-babel)
(require 'folio-dialog-forms)
(require 'folio-etaoin-shrdlu)
(require 'folio-faces)
(require 'folio-image)
(require 'folio-time)
(require 'folio-uca)

(defvar folio-widget-dict-entry-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap
          widget-keymap folio-dialog-form-mode-map))
    (define-key map (kbd "C-e") 'widget-end-of-line)
    (define-key map (kbd "<M-right>")
      'folio-widget-dict-entry-next)
    (define-key map (kbd "<M-left>")
      'folio-widget-dict-entry-previous)
    (define-key map (kbd "A-<return>")
      'folio-widget-dict-entry-menu)
    (define-key map [down-mouse-1]
      'widget-move-and-invoke)
    map)
  "Keymap for an entry of the dictionary widget.")

(defvar folio-widget-dict-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap
          widget-keymap folio-dialog-form-mode-map))
    (define-key map (kbd "<wheel-down>") 'folio-widget-dict-entry-next)
    (define-key map (kbd "<wheel-up>") 'folio-widget-dict-entry-previous)
    ;; XXX "fast" scroll by jumping to lexicographic next/previous
    ;; initial letter
    ;; (define-key map (kbd "<S-wheel-down>") 'folio-widget-dict-entry-next)
    ;; (define-key map (kbd "<S-wheel-up>") 'folio-widget-dict-entry-previous
    map)
  "Keymap for the dictionary widget.")

(defun folio-widget-dict-value (&optional regexp)
  "Return the value for the dictionary widget.
If the regexp REGEXP is non-nil filter out any words in the
vocabulary not matching.  If the GWL widget is toggled on filter
out any word that is in the `good word' list."
  (let* ((filters (append '(misspellings)
                          (when (widget-value-value-get
                                 (folio-dialog-form-get 'dict-gwl))
                            '(good-words))))
         (words (folio-with-parent-buffer
                  (folio-vocabulary-list 'lexicographic filters))))
    (when (and (stringp regexp)
               (not (string-equal regexp "")))
      (setq words
            (folio-filter-list
             words (lambda (x)
                     (string-match-p regexp x)))))
    words))

(defun folio-widget-dict-lookup (_widget word)
  "Adapt `folio-vocabulary-spellchecker-data' for use with
widgets."
  (folio-with-parent-buffer
    (folio-vocabulary-spellchecker-data word)))

(defun folio-widget-dict-frequency-lookup (_widget word)
  "Adapt `folio-vocabulary-word-count' for use with widgets."
  (folio-with-parent-buffer
    (folio-vocabulary-word-count word)))

(defun folio-widget-dict-good-words-lookup (_widget word)
  "Return non-nil if WORD is in the project local `good word' list."
  (folio-with-parent-buffer
    (folio-vocabulary-good-word-p word)))

(define-widget 'folio-widget-dict-item 'push-button
  "An entry within a dictionary section."
  :value-create 'folio-widget-dict-item-value-create
  :value-to-internal 'folio-widget-dict-item-value-to-internal
  :format "%[%v%]\n")

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
      (concat (if word
                  (folio-left-to-right-safe word)
                "<unknown>")
              " "
              (if count
                  (propertize
                   (concat ":" count)
                   'face 'folio-frequency-tag
                   'help-echo (concat
                               count " "
                               (folio-pluralize
                                "occurrence" (cadr value))
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
  :frequency-lookup 'folio-widget-dict-frequency-lookup)

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
                                      'face 'folio-dict)
                          (propertize (concat
                                       ":" (car value))
                                      'face 'folio-dict-tag))))
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
             (items (sort (cdr value) 'folio-uca-lessp))
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

(defun folio-widget-dict-entry-menu ()
  "Handle the popup-menu for a dictionary entry."
  ;; XXX TODO sub-menus for multi-dicts
  (interactive)
  (let* ((widget (widget-at))
         (parent (widget-get widget :parent))
         (word (widget-get widget :value))
         (menu (make-sparse-keymap (concat "`" word "'")))
         choice)
    (define-key menu [save-global]
      `(menu-item "Add to global dictionary" save-global
                  :key-sequence ,[(control x) (g)]
                  :help "Add current word to global dictionary"))
    (define-key menu [save-local]
      `(menu-item "Add to project dictionary" save-local
                  :key-sequence ,[(control x) (p)]
                  :help "Add current word to project dictionary"))
    (define-key menu [accept-session]
      `(menu-item "Accept this session" accept-session
                  :key-sequence ,[(control x) (a)]
                  :keys "\\[accept-session]"
                  :help "Accept current word this session"))
    ;; There appears to be no way to pre-select an item in the menu
    ;; (push (listify-key-sequence (kbd "<down>")) unread-command-events)
    (when (setq choice (car (x-popup-menu t menu)))
      (widget-apply
       parent :notify parent `(dict-apply ,choice ,word)))))

(define-widget 'folio-widget-dict-entry-item 'item
  "Widget for an item in a dictionary entry.
The widget maintains a misspelled word and its frequency count."
  :value-create 'folio-widget-dict-entry-item-value-create
  :tag ""
  :format "%[%v%]\n"
  :action 'folio-widget-dict-entry-item-action
  :keymap folio-widget-dict-entry-keymap
  :frequency-lookup 'folio-widget-dict-frequency-lookup
  :good-word-lookup 'folio-widget-dict-good-words-lookup)

(defun folio-widget-dict-entry-item-value-create (widget)
  "Value create the widget WIDGET.
WIDGET must be of type `folio-widget-dict-entry-item'."
  (let* ((word (widget-get widget :value))
         (focused (widget-get (widget-get widget :parent) :focus-entry))
         (frequency (or (widget-apply widget :frequency-lookup word) 0))
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

(defun folio-widget-dict-entry-item-action (widget &optional event)
  "Handle user initiated events."
  (let ((parent (widget-get widget :parent)))
    (cond
     ((widget-get parent :open)
      (let ((entry (widget-get widget :dict-value)))
        (widget-apply
         parent :notify widget `(dict-choice ,entry))))
     (t
      (widget-apply parent :notify widget 'dict-focus)))))

(define-widget 'folio-widget-dict-entry 'tree-widget
  "A dictionary entry for `folio-widget-dict'.
The widget maintains a instance of `folio-widget-dict-entry-item'
for displaying a misspelled word and one or more child nodes each
again tree-widgets for suggestions of a spellchecker run."
  :value-create 'folio-widget-dict-entry-value-create
  :keep '(:dict-value :focus-entry)
  :expander 'folio-widget-dict-entry-expand
  :notify 'folio-widget-dict-entry-notify
  :dict-lookup 'folio-widget-dict-lookup
  :focus 'folio-widget-dict-entry-focus
  :focus-entry nil
  :keymap folio-widget-dict-entry-keymap
  :frequency-lookup 'folio-widget-dict-frequency-lookup)

(defun folio-widget-dict-entry-value-create (widget)
  "Value create the widget WIDGET.
Set up a node item which tag is the dictionary entry.  The
widget :value should be a word from the text vocabulary."
  (let ((node (widget-get widget :node)))
    (if node
        ;; Check that the :node widget is not a tree-widget.
        (and (tree-widget-p node)
             (error "Invalid tree-widget :node %S" node))
      (let* ((value (widget-get widget :value)))
        (setq node `(folio-widget-dict-entry-item
                     :value ,value))
        (widget-put widget :node node)
        (widget-put node :dict-value value))))
  (tree-widget-value-create widget))

(defun folio-widget-dict-entry-notify (widget child &optional event)
  "Handle a state change of WIDGET's CHILD widget."
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

(define-widget 'folio-widget-dict 'folio-widget-repeat
  "A scrollable dictionary widget."
  :notify 'folio-widget-dict-notify
  :keymap folio-widget-dict-keymap
  :offset 0
  :indent 6)

;;  (remove-hook 'folio-word-occurrence-functions 'folio-locate-word t)

(defun folio-widget-dict-notify (widget child &optional event)
  "Handle notification event EVENT from child widget CHILD."
  (cond
   ((eq (widget-type child) 'folio-widget-dict-entry)
    (cond
     ((eq event 'dict-focus)
      (widget-default-notify widget child event)
      (widget-apply widget :focus child))
     ((eq (car-safe event) 'dict-choice)
      (save-selected-window
        (switch-to-buffer-other-window folio-parent-buffer)
        (run-hook-with-args
         'folio-word-occurrence-functions (cadr event)))
      ;; (re-)focus the child up-stack that originally produced the
      ;; event
      (widget-apply widget :focus child))
     ((eq (car-safe event) 'dict-substitute)
      (folio-with-parent-buffer
        (run-hook-with-args 'folio-word-substitution-functions
                            (caadr event) (cdadr event))))

     ((eq (car-safe event) 'dict-apply)
      (widget-apply widget :delete-at child)
      (folio-with-parent-buffer
        (run-hook-with-args 'folio-dict-maintainance-functions
                            (cadr event) (caddr event)))
      (widget-apply widget :focus nil)
      t)
     (t
      nil)))
   (t
    (widget-default-notify widget child event))))

(defun folio-widget-dict-entry-next ()
  "Scroll the dictionary widget focusing on the next entry."
  (interactive)
  (folio-widget-repeat-scroll-up 'folio-widget-dict (point)))

(defun folio-widget-dict-entry-previous ()
  "Scroll the dictionary widget focusing on the previous entry."
  (interactive)
  (folio-widget-repeat-scroll-down 'folio-widget-dict (point)))

(defun folio-widget-primary-dictionary-values (widget)
  (let* ((form (folio-dialog-form-get 'primary-dictionary))
         (selected (when form
                     (list (widget-get form :value))))
         (dicts (sort (folio-with-parent-buffer
                        (folio-dictionary-list)) 'string-lessp))
         available)
    (mapc (lambda (x)
            (unless (member x selected)
              (push x available)))
          dicts)
    (setq available (nreverse available))
    available))

(defun folio-widget-primary-dictionary-notify (widget &rest ignore)
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

(defun folio-widget-secondary-dictionary-values (_widget)
  (cons "<none>"
        (sort (folio-with-parent-buffer
                (folio-dictionary-choices-list)) #'string-lessp)))

(defun folio-widget-secondary-dictionary-notify (widget &rest ignore)
  (folio-with-parent-buffer
    (folio-change-dictionary
     (folio-primary-dictionary)
     (remove "<none>" (widget-value widget)))))

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
                    :format "    %[ %t   %] %v"
                    :button-face 'custom-button
                    :notify 'folio-widget-primary-dictionary-notify
                    :value-face 'folio-widget-field
                    :value (or (folio-with-parent-buffer
                                 (folio-primary-dictionary))
                               (car (folio-dictionary-list)))
                    :values 'folio-widget-primary-dictionary-values))

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
                                 (folio-widget-dict-filter-apply))))
                   :help-echo (lambda (_widget)
                                (let ((buffer folio-parent-buffer))
                                  (concat "Push to "
                                          (if (folio-vocabulary-build-active-p buffer)
                                              "stop" "start")
                                          " spell-checking."))))
    (widget-insert "\n\n")
    (folio-dialog-form 'secondary-dictionary
                       (widget-create 'repeat
                                      :insert-button-args '(:button-face custom-button)
                                      :delete-button-args '(:button-face custom-button)
                                      :append-button-args '(:button-face custom-button)
                                      :format "%v  %i"
                                      :entry-format "  %i %d   %v"
                                      :value '("<none>")
                                      :notify 'folio-widget-secondary-dictionary-notify
                                      '(folio-menu-choice
                                        :tag "Secondary dictionary"
                                        :format "%[ %t %] %v\n"
                                        :value "<none>"
                                        :button-face custom-button
                                        :values folio-widget-secondary-dictionary-values)))

    (widget-insert "\n\n                 Accept `good words' ")
    (folio-dialog-form
     'dict-gwl (widget-create
                'checkbox :value nil
                :notify (lambda (&rest ignore)
                          (folio-widget-dict-filter-apply))))

    (widget-insert "\n\n\n\n")
    ;; Pretty much like 'regexp but validated a little differently.
    (folio-dialog-form
     'dict-filter (widget-create
                   'string :tag "Filter" :format "%t: %v" :size 14
                   :value-face 'folio-widget-field
                   :notify 'folio-widget-dict-filter-notify))
    (widget-insert " ")
    (widget-create 'push-button
                   :format "%[%t%]"
                   ;; :tag-glyph (find-image
                   ;;    `((:type jpg :file "delete-small.jpg" :ascend 5)))
                   :tag "Reset"
                   :button-face 'custom-button
                   :notify 'folio-widget-dict-filter-reset)

    (folio-dialog-form-rule 32)

    (folio-dialog-form
     'dictionary (widget-create
                  'folio-widget-dict :value (folio-widget-dict-value)
                  `(folio-widget-dict-entry)))

    (widget-setup)))

(defcustom folio-dict-filter-delay 0.24
  "Time in seconds to wait before updating the dictionary view."
  :group 'folio-technical
  :tag "Folio Dictionary View Refresh Delay"
  :type 'number)

(folio-define-timer 'dict-filter
    "Idle timer for refreshing the dictionary widget."
  :function 'folio-widget-dict-filter-apply
  :repeat nil
  :buffer-local t
  :secs (lambda () folio-dict-filter-delay))

(defun folio-widget-dict-filter-apply ()
  "Update the dictionary widget to the current regexp filter."
  (let ((widget (folio-dialog-form-get 'dict-filter)))
    (when widget ;; unlikely race-condition
      (let* ((old-value (widget-get widget :filter-value))
             (value (widget-value widget))
             (new-value (when (stringp value) (folio-chomp value))))
        (when (folio-regexp-valid-p new-value)
          (let ((filtered (folio-widget-dict-value new-value)))
            (widget-value-set
             (folio-dialog-form-get 'dictionary) filtered))
          (widget-put widget :filter-value new-value)
          (widget-setup))))))

(defun folio-widget-dict-filter-notify (widget child &optional event)
  "Handle notification event EVENT from child widget CHILD."
  (let* ((old-value (widget-get widget :filter-value))
         (value (widget-value child))
         (new-value (when (stringp value) (folio-chomp value))))
    (when (folio-timer-running-p 'dict-filter)
      (folio-cancel-timer 'dict-filter))
    (if (folio-regexp-valid-p new-value)
        (unless (string-equal old-value new-value)
          (folio-schedule-timer 'dict-filter))
      ;; Apparently neither `error' nor `user-error' do play well with
      ;; widgets, leaving things in a half-operational state; the use
      ;; of the widget's `:error' property is unclear.
      (message "Invalid filter expression"))))

(defun folio-widget-dict-filter-reset (widget child &optional _event)
  "Reset the filter widget and update views."
  (let* ((filter (folio-dialog-form-get 'dict-filter))
         (value (widget-value filter)))
    (when (or (null value)
              (not (stringp value))
              (not (string-equal value "")))
      (widget-value-set filter "")
      (folio-widget-dict-filter-apply))))


(provide 'folio-dialog-spellcheck)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-dialog-spellcheck.el ends here
