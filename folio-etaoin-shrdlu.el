;;; folio-etaoin-shrdlu.el --- Folio mode word frequencies & spelling

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

;; Provide facilities for word frequency analysis, spell-checking,
;; dictionary and transcriber's log maintenance.  Potentially long
;; running tasks for one or more buffers are maintained asynchronously
;; yet efficient by low latency idle timers.  A 500 pages history text
;; book for instance with 164840 words, 9970 of which are unique and
;; with 1688 misses is spell-checked in 20 seconds using the aspell
;; engine.  Determining word frequencies lasts 8 seconds in this
;; example.
;;
;; [Etaoin Shrdlu, pronounced "eh-tay-oh-in shird-loo", is a somewhat
;; infamous phrase from the times of Linotype typesetter keyboards
;; that has become part of the printer's lore.  It is believed to be
;; the twelve most common letters in English, in order of most
;; frequently used to least frequently used.]

;;; Code:

(require 'custom)

(require 'folio-atoms)
(require 'folio-babel)
(require 'folio-base)
(require 'folio-core)
(require 'folio-dictionary)
(require 'folio-faces)
(require 'folio-font-lock)
(require 'folio-frame)
(require 'folio-levenshtein)
(require 'folio-log)
(require 'folio-spellcheck)
(require 'folio-time)
(require 'folio-uca)
(require 'folio-word-break)

(defconst folio-vocabulary-default-regexp
  "\\<\\(\\sw+\\)\\>\\([ \n\t\f]+\\(\\1\\>\\)\\)?"
  "The default regexp for building the vocabulary table.
This regexp is used if `folio-spellcheck-doublons' is non-nil.")

(defconst folio-vocabulary-words-only-regexp
  "\\<\\(\\sw+\\)\\>"
  "Alternative regexp for building the vocabulary table.
This regexp is used if `folio-spellcheck-doublons' is nil.")

(defvar folio-vocabulary-regexp folio-vocabulary-default-regexp
  "The regexp used for building the vocabulary table.")

(defcustom folio-spellcheck-doublons t
  "If non-nil also check for doublons (doubloons) when
spell-checking.  This is the default."
  :tag "Check for doublons"
  :group 'folio-spellcheck
  :type 'boolean
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (eq value t)
                       (setq folio-vocabulary-regexp
                             folio-vocabulary-default-regexp)
                     (setq folio-vocabulary-regexp
                           folio-vocabulary-words-only-regexp)))))

(defcustom folio-apostrophe-always-word-constituent nil
  "If non-nil the apostrophe character \(displayed as ')
\(codepoint 39, #o47, #x27) always is treated as a word
constituent unless the current language default says different.
If this option is enabled the apostrophe only is treated as a
word constituent if preceded by whitespace or punctuation, or of
the punctuation category otherwise.  This option should be set to
nil \(off) if the apostrophe character in texts mainly is used
for both apostrophe and single quotation mark."
  :tag "Apostrophe always is word constituent"
  :group 'folio-spellcheck
  :type 'boolean)

(defcustom folio-dictionary-size 16384
  "Initial pre-allocation size of an in-memory dictionary.
A textbook of 500 pages has about 150000 words, 10% of which
maybe are unique."
  :tag "Default dictionary size"
  :group 'folio-technical
  :type 'number)

(defcustom folio-case-folded-dictionary t
  "If non-nil use case folded dictionary keys."
  :tag "Use case-folded dictionary keys"
  :group 'folio-technical
  :type 'boolean)

(defcustom folio-vocabulary-build-delay 0.3
  "Time in seconds to wait before resuming vocabulary build and
spell-check."
  :group 'folio-spellcheck
  :tag "Vocabulary build delay"
  :type 'number)

(defcustom folio-vocabulary-build-pause 0.1
  "Time in seconds to pause a vocabulary build or spell-check run."
  :group 'folio-spellcheck
  :tag "Vocabulary Build Pause"
  :type 'number)

(defcustom folio-vocabulary-build-chunks most-positive-fixnum
  "Number of text chunks to process at a time."
  :type 'number
  :group 'folio-spellcheck) ;; XXX should be '(choice (const nil) (number))

(defvar folio-vocabulary nil
  "Hash table storing the buffer local vocabulary.
The value may include word frequency counts and suggestions from
a spell checker.")
(make-variable-buffer-local 'folio-vocabulary)

(defvar folio-next-vocabulary nil
  "Hash table storing the buffer local vocabulary.
The value may include word frequency counts and suggestions from
a spell checker.")
(make-variable-buffer-local 'folio-next-vocabulary)

(defvar folio-vocabulary-session nil
  "Hash table storing session local words.")
(make-variable-buffer-local 'folio-vocabulary-session)

(defvar folio-vocabulary-project nil
  "Hash table storing project local words.")
(make-variable-buffer-local 'folio-vocabulary-project)

(defvar folio-vocabulary-good-words nil
  "Hash table storing project local words marked good.
These might be merged with `folio-vocabulary-project'.")
(make-variable-buffer-local 'folio-vocabulary-good-words)

(defvar folio-vocabulary-global nil
  "Hash table storing project global words.")

(defvar folio-vocabulary-marker nil
  "Marker for the last buffer scan position.")
(make-variable-buffer-local 'folio-vocabulary-marker)

(folio-define-timer 'vocabulary
  "Idle timer driving the vocabulary collector."
  :function 'folio-vocabulary-process-buffer
  :repeat 'repeat
  :secs (lambda () folio-vocabulary-build-delay)
  :pause (lambda () folio-vocabulary-build-pause))

(defvar folio-vocabulary-build-interrupted nil
  "Non-nil when input has arrived while assembling a vocabulary.
This state is switched by `folio-vocabulary-build-interrupted-p'.")

(defvar folio-word-marker nil
  "Overlay highlighting the currently selected word.")

(defvar folio-vocabulary-build-progress-hooks nil
  "Abnormal hook run to report progress.
The hook is run with the arguments BUFFER and PROGRESS with
PROGRESS in the format \(POINT-MIN POINT POINT-MAX).  Return
values of hook functions are ignored.")

(defvar folio-vocabulary-build-done-hooks nil
  "Abnormal hook run to report a buffer as completely processed
The hook is run with the buffer object as its sole argument.
Return values are ignored.")

(defvar folio-word-occurrence-functions nil
  "Abnormal hook run when a dictionary entry is selected interactively.")

(defvar folio-word-substitution-functions nil
  "Abnormal hook run when a word replacement is chosen interactively.")

(defvar folio-dict-maintainance-functions nil
  "Abnormal hook run for dictionary maintenance.")

(defsubst folio-make-vocabulary ()
  "Create and return a new hash table for a vocabulary."
  (make-hash-table :test (if folio-case-folded-dictionary
                             'folio-case-fold-hash-table-test
                           'equal)
                   :size folio-dictionary-size))

(defun folio-vocabluary-update-entry (entry count
                                            &optional dict sort-key)
  "Update the vocabulary entry ENTRY with the word count COUNT.
If DICT is non-nil update the dictionary data with DICT, too.
DICT should be a cons of current spellchecker language and the
list of suggestions.  DICT also can be a symbol marking the entry
for inclusion into the session local, project local, or global
dictionary.  The respective symbol names then should be session,
project or global."
  (let ((entry (or entry (make-vector 4 nil))))
    (aset entry 0 count)
    (when dict
      (if (symbolp dict)
          (aset entry 2 dict)
        (aset entry 1 (append (aref entry 1) (list dict)))))
    (when sort-key
      (aset entry 3 sort-key))
    entry))

(defsubst folio-vocabulary-entry-count (entry)
  "Return the word count for the vocabulary entry ENTRY."
  (if entry (aref entry 0) 0))

(defsubst folio-vocabulary-entry-spellchecker-data (entry)
  "Return the spellchecker data for the vocabulary entry ENTRY.
This defun is meant for de-structuring the entry only.  Higher
level functions most certainly want to make use of
`folio-vocabulary-word-count', `folio-vocabulary-miss-count' and
`folio-vocabulary-spellchecker-data'."
  (when entry (aref entry 1)))

(defsubst folio-vocabulary-entry-misspelled-p (entry)
  "Return non-nil if ENTRY is that of a misspelled word."
  (and entry (null (aref entry 2)) (aref entry 1)))

(defsubst folio-vocabulary-entry-sort-key (entry)
  "Return the sort-key for the vocabulary entry ENTRY."
  (when entry (aref entry 3)))

(defsubst folio-vocabulary-sort-key (word)
  (folio-vocabulary-entry-sort-key
   (folio-lookup-dictionary word folio-vocabulary)))

(defun folio-vocabulary-spellchecker-data (word)
  "Return the spellchecker data for WORD."
  (folio-vocabulary-entry-spellchecker-data
   (cdar (folio-lookup-dictionary
          word folio-vocabulary))))

(defun folio-vocabulary-word-count (&optional word)
  "Return the number of unique words in the vocabulary.
If WORD is non-nil return its frequency count instead.  See also
`folio-vocabulary-miss-count'."
  (if folio-vocabulary
      (if word
          (folio-vocabulary-entry-count
           (cdar (folio-lookup-dictionary word folio-vocabulary)))
        (folio-dictionary-count folio-vocabulary))
    0))

(defun folio-vocabulary-miss-count ()
  "Return the number of misspelled words in the vocabulary.
See also `folio-vocabulary-word-count'."
  (let ((count 0))
    (when folio-vocabulary
      (folio-map-dictionary
       (lambda (k v)
         (when (folio-vocabulary-entry-misspelled-p v)
           (setq count (1+ count))))
       folio-vocabulary))
    count))

(defun folio-vocabulary-alphabet ()
  "Return the alphabet for the vocabulary as a char table.

The value of a table entry is an absolute occurrence count."
  (let ((alphabet (make-char-table 'vocabulary-alphabet 0)))
    (folio-map-dictionary (lambda (k)
                            (dotimes (i (1- (length k)))
                              (incf (aref alphabet (aref k i))
                                    1)))
                          folio-vocabulary
                          :no-values t)
    alphabet))

(defun folio-maintain-dictionary (command word)
  "Maintain dictionaries by applying COMMAND.

COMMAND is one of the symbols accept-session, save-local, or
save-global.  WORD identifies the dictionary entry."
  ;; XXX TODO undo-history
  (let* ((dict-alist '((accept-session
                        . (session "accepted this session"))
                       (save-local
                        . (local "saved to project dictionary"))
                       (save-global
                        . (global "saved to global dictionary"))))
         (entry (folio-get-dictionary-entry word folio-vocabulary))
         (select (assq command dict-alist))
         (dict (cadr select)))
    (folio-vocabluary-update-entry
     entry (folio-vocabulary-entry-count entry) dict)))

(defun folio-vocabulary-filter-misspellings (k v)
  "Return t if the vocabulary entry K, V is marked misspelled.
Also see `folio-vocabulary-apply-filters'."
  (and (folio-vocabulary-entry-misspelled-p v) t))

(defun folio-vocabulary-filter-good-words (k v)
  "Return t if the word for vocabulary entry K, V is not also in
the list of `good words'.  Also see `folio-vocabulary-apply-filters'."
  (if folio-vocabulary-good-words
      (not (cdr-safe
            (gethash k folio-vocabulary-good-words)))
    t))

(defun folio-vocabulary-filter-upper-case (k v)
  "Return t if the word for the vocabulary entry K, V starts
with an upper-case or title-case letter."
  (memq (get-char-code-property
         (aref k 0) 'general-category) '(Lu Lt)))

(defun folio-vocabulary-filter-lower-case (k v)
  "Return t if the word for the vocabulary entry K, V starts
lower-case."
  (eq (get-char-code-property
       (aref k 0) 'general-category) 'Ll))

(defun folio-vocabulary-filter-numeric (k v)
  "Return non-nil if the word for the vocabulary entry K, V has a
numeric value or builds a numeric sequence.  Include
compatibility mappings of Roman numerals."
  (let* ((i 1)
         (j (1- (length k)))
         (numericp (and (get-char-code-property
                         (aref k 0) 'numeric-value)
                        (get-char-code-property
                         (aref k j) 'numeric-value)))
         c)
    (while (and numericp (< i j))
      (setq c (aref k i) i (1+ i)
            numericp (or (get-char-code-property c 'numeric-value)
                         (folio-mid-numeric-p c))))
    (if (null numericp)
        ;; non-strict Roman numeral compatibility mappings
        (or (string-match-p "\\`[ivxcdm]+\\'" k)
            (string-match-p "\\`[IVXCDM]+\\'" k))
      numericp)))

(defun folio-vocabulary-filter-alpha-numeric (k v)
  "Return non-nil if the vocabulary entry K, V is alpha-numeric.
By this definition it must include at least one letter and one
character with a numeric value."
  (let ((alpha '(Lu Ll Lt Lm Lo))
        (i 1)
        (j (length k))
        c alphap numericp)
    (while (and (not (and alphap numericp)) (< i j))
      (setq c (aref k i) i (1+ i))
      (unless alphap
        (setq alphap (memq (get-char-code-property
                            c 'general-category) alpha)))
      (unless numericp
        (setq numericp (get-char-code-property
                        c 'numeric-value))))
    (and alphap numericp)))

(defun folio-vocabulary-filter-title-case (k v)
  "Return non-nil if the vocabulary entry K, V contains
characters in title case.  Title case characters are in ligatures
containing uppercase followed by lowercase letters (e.g., ǅ, ǈ,
ǋ, and ǲ) or Greek script."
  (let ((i 1)
        (j (length k))
        c titlecasep)
    (while (and (not titlecasep) (< i j))
      (setq c (aref k i) i (1+ i)
            titlecasep (eq (get-char-code-property
                            c 'general-category) 'Lt)))
    titlecasep))

(defvar folio-vocabulary-script nil
  "A script symbol that if non-nil selects words from the current
vocabulary that have at least one character from to chosen
script.")

(defun folio-vocabulary-filter-script (k v)
  "Return non-nil if the vocabulary entry K, V contains one or
more characters from the script `folio-vocabulary-script'."
  (if folio-vocabulary-script
      (let ((i 0)
            (j (length k))
            scriptp)
        (while (and (not scriptp) (< i j))
          (when (eq (aref char-script-table (aref k i))
                    folio-vocabulary-script)
            (setq scriptp t))
          (setq i (1+ i)))
        scriptp)
    t))

(defun folio-vocabulary-filter-diacritics (k v)
  "Return non-nil if the vocabulary entry K, V contains
characters with diacritical marks."
  (let ((quick-check (folio-dictionary-extra-slot
                      folio-vocabulary 0))
        (i 0)
        (j (length k))
        pass)
    (unless quick-check
      (let ((alphabet (mapcar #'car (folio-dictionary-alphabet
                                     folio-vocabulary 'alist)))
            nfd char gc ccc)
        (setq nfd (string-to-list
                   (ucs-normalize-NFD-string
                    (mapconcat #'string alphabet " ")))
              quick-check (make-char-table 'diacritic))
        (while alphabet
          (if (= (car alphabet) (car nfd))
              (progn
                (pop alphabet)
                (pop nfd))
            (pop nfd)
            (catch 'break
              (while (and nfd (/= (setq char (car nfd)) ?\ ))
                (setq gc (get-char-code-property
                          char 'general-category)
                      ccc (get-char-code-property
                           char 'canonical-combining-class))
                ;; Include a character if its canonical decomposition
                ;; includes at least one non-spacing mark (Mn),
                ;; ignoring space combining (Mc), enclosing (Me), and
                ;; other (Mo) marks.  Also exclude any character with
                ;; non-zero character combining class including
                ;; joiners of grapheme clusters of cursive joining,
                ;; ligaturing, etc.
                (when (and (eq gc 'Mn)
                           (/= ccc 0))
                  (aset quick-check (car alphabet) t)
                  (throw 'break t))
                (pop nfd)))
            (while (and nfd (/= (car nfd) ?\ ))
              (pop nfd))
            (pop alphabet)
            (pop nfd)))
        (folio-dictionary-set-extra-slot
         folio-vocabulary 0 quick-check)))
    (catch 'break
      (while (< i j)
        (when (aref quick-check (aref k i))
          (throw 'break (setq pass t)))
        (setq i (1+ i))))
    pass))

(defconst folio-vocabulary-filter-alist
  '((misspellings . folio-vocabulary-filter-misspellings)
    (good-words . folio-vocabulary-filter-good-words)
    (upper-case . folio-vocabulary-filter-upper-case)
    (lower-case . folio-vocabulary-filter-lower-case)
    (title-case . folio-vocabulary-filter-title-case)
    (numeric . folio-vocabulary-filter-numeric)
    (alpha-numeric . folio-vocabulary-filter-alpha-numeric)
    (diacritics . folio-vocabulary-filter-diacritics)
    (script . folio-vocabulary-filter-script))
  "Alist mapping filter name to filter function.")

(defun folio-vocabulary-apply-filters (filters k v)
  "Apply the filter list FILTERS to the vocabulary entry K, V.

K is the entry's key, V its value.  FILTERS is a list of symbols
of binary filter functions \(K V) for filtering out an entry if
the return value is nil, or otherwise to include it in the result
set.  If FILTERS contains a string valued member then perform a
regexp match against K.  Return t if all filters from FILTERS let
pass, or nil otherwise."
  (let ((pass t))
    (while (and pass filters)
      (if (stringp (car filters))
          (setq pass (and (string-match-p (car filters) k) t))
        (setq pass (funcall (symbol-function (car filters)) k v)))
      (pop filters))
    pass))

(defun folio-vocabulary-list-lexicographic (&optional filters)
  "*List vocabulary entries for lexicographic comparisons.

Members are lists of the form \(WORD COUNT UCA-SORT-KEY).
FILTERS is a list of function symbols for use with
`folio-vocabulary-apply-filters'."
  (lexical-let (words)
    (folio-map-dictionary
     (lambda (k v)
       (when (folio-vocabulary-apply-filters filters k v)
         (let ((count (folio-vocabulary-entry-count v))
               (key (folio-vocabulary-entry-sort-key v)))
           (setq words
                 (cons (list k count key) words)))))
     folio-vocabulary)
    words))

(defun folio-vocabulary-sort-lexicographic (&optional filters)
  "Sort vocabulary entries lexicographic."
  ;; the dictionary is maintained in lexicographic ordering
  (let ((words (folio-vocabulary-list-lexicographic filters)))
    (mapcar (lambda (x)
              (car x)) words)))

(defun folio-vocabulary-sort-frequency (&optional filters)
  "Sort vocabulary entries by frequency.
FILTERS is a list of function symbols for use with
`folio-vocabulary-apply-filters'."
  (let ((words (folio-vocabulary-list-lexicographic filters)))
    (mapcar (lambda (x)
              (car x))
            (sort words
                  (lambda (x y)
                    (or (< (cadr x) (cadr y))
                        (and (= (cadr x) (cadr y))
                             (folio-uca-lessp
                              (caddr x) (caddr y)))))))))

(defun folio-vocabulary-sort-length (&optional filters)
  "Sort vocabulary entries by word length.
FILTERS is a list of function symbols for use with
`folio-vocabulary-apply-filters'."
  (let ((words (folio-vocabulary-list-lexicographic filters)))
    (mapcar (lambda (x)
              (car x))
            (sort words
                  (lambda (x y)
                    (let ((xlen (length (car x)))
                          (ylen (length (car y))))
                      (or (< xlen ylen)
                          (and (= xlen ylen)
                               (folio-uca-lessp
                                (caddr x) (caddr y))))))))))

(defconst folio-vocabulary-ordering-alist
  '((lexicographic . folio-vocabulary-sort-lexicographic)
    (frequency . folio-vocabulary-sort-frequency)
    (length . folio-vocabulary-sort-length))
  "Alist mapping sort order to sort function.")

(defun folio-vocabulary-list (&optional ordering filters)
  "Create a word list from `folio-vocabulary'.
If ORDERING is nil or omitted, the sort-order is undefined.
Otherwise it specifies the sort-order to use.

Supported values are

  'lexicographic sort lexicographic ascending.

  'frequency     sort by word frequency ascending; secondary ordering
                 is lexicographic ascending.

  'length        sort by word length ascending; secondary ordering is
                 lexicographic ascending."
  (when folio-vocabulary
    (let ((lister (cdr (assq ordering
                             folio-vocabulary-ordering-alist)))
          (filters (mapcar
                    (lambda (x)
                      (if (stringp x)
                          x
                        (cdr (assq x
                                   folio-vocabulary-filter-alist))))
                    (if (listp filters) filters (list filters)))))
      (if lister
          (funcall (symbol-function lister) filters)
        (if (null ordering)
            (let (words)
              (folio-map-dictionary
               (lambda (k v)
                 (when (folio-vocabulary-apply-filters
                        filters k v)
                   (setq words
                         (cons k words)))) folio-vocabulary)
              words)
          (signal 'wrong-type-argument
                  (cons ordering (type-of ordering))))))))

(defun folio-soundslikes (word &optional distance)
  "Look for soundslikes within two edit distance apart.
Soundslikes for WORD are searched in the vocabulary as collected
by the most recent spell-check or word-frequency run.  DISTANCE
if non-nil overrides the default maximal edit distance of one."
  (interactive "M")
  (when folio-vocabulary
    (folio-lookup-dictionary word folio-vocabulary
                             :max-distance (or distance 1)
                             :no-values t)))

;;;###autoload
(defun folio-load-good-words (&optional no-error)
  "Load the project's `good words' dictionary."
  (interactive)
  (let ((file-name (concat default-directory "good_words.txt"))
        words)
    (condition-case err
        (with-temp-buffer
          ;; Perform code conversion according to
          ;; `coding-system-for-read'.
          (insert-file-contents file-name)
          (goto-char (point-min))
          (while (re-search-forward "^\\(\\sw+\\)\\>" nil 'noerror)
            (setq words (cons (match-string-no-properties 1) words))))
      (error (unless no-error
               (signal (car err) (cdr err)))))
    (with-current-buffer (current-buffer)
      (if folio-vocabulary-good-words
          (clrhash folio-vocabulary-good-words)
        ;; The `good words' dictionary is maintained without
        ;; case-folding even if the project dictionary is used with
        ;; case-folding.
        (setq folio-vocabulary-good-words
            (make-hash-table :test 'equal
                             :size folio-dictionary-size)))
      (mapc (lambda (x)
              (puthash x '(:gwl t) folio-vocabulary-good-words))
            words)
      folio-vocabulary-good-words)))

(defun folio-vocabulary-good-word-p (word)
  "Return non-nil if WORD is in the `good word' list."
  (when folio-vocabulary-good-words
    (cdr-safe (gethash word folio-vocabulary-good-words))))

(defun folio-filter-good-words (words)
  "Filter the word list WORDS by excluding a member if it is in
the `good word' list."
  (if folio-vocabulary-good-words
      (folio-filter-list
       words (lambda (x)
               (not (cdr-safe
                     (gethash x folio-vocabulary-good-words)))))
    words))

(defun folio-known-misspelling (word &optional pos dict-entry)
  "Return non-nil if WORD is a known misspelling.
If POS is non-nil return WORD with `point' updated to the next
buffer location of WORD, or nil if WORD is not found.  If
DICT-ENTRY is non-nil return that instead."
  (let ((entry (folio-lookup-dictionary word folio-vocabulary)))
    (when (folio-vocabulary-entry-misspelled-p entry)
      (if pos
          (progn
            (goto-char pos)
            (and (search-forward word nil t)
                 (goto-char (match-beginning 0))
                 (or (and dict-entry entry) t)))
        (if dict-entry entry t)))))

(defun folio-word-marker-at (beg end)
  "Highlight the word occupying the buffer region BEG END."
  (if folio-word-marker
      (move-overlay folio-word-marker beg end)
    (setq folio-word-marker
          (make-overlay beg end (current-buffer)
                        'front-advance 'rear-advance))
    ;; (overlay-put overlay 'help-echo "mouse-2: correct word at point")
    ;; (overlay-put overlay 'keymap flyspell-mouse-map)
    (dolist (prop '((face . folio-word-marker)
                    (priority . 10)
                    (evaporate . t)))
      (overlay-put folio-word-marker (car prop) (cdr prop)))))

(defun folio-word-marker-recenter ()
  "Center the word marker in the selected window."
  (interactive)
  (when folio-word-marker
    (let ((pos (overlay-start folio-word-marker)))
      (when pos
        (overlay-recenter pos)
        (goto-char pos)
        (recenter (when (>= (window-height) 52) 10))
      t))))

(defun folio-word-marker-hide ()
  "Hide the word marker."
  (when folio-word-marker
    (delete-overlay folio-word-marker)))

(defcustom folio-sync-current-word nil
  "If non-nil synchronize the various project buffer views with the
currently selected word."
  :tag "Whether to synchronize dictionary and vocabulary views"
  :type '(choice
          (const :tag "Don't synchronize" nil)
          (const :tag "Synchronize all" 'all)
          (const :tag "Synchronize with vocabulary view" 'vocabulary)
          (const :tag "Synchronize with dictionary view" 'dictionary))
  :group 'folio-spellcheck)

(defun folio-vocabulary-current-word (scope)
  "Return the current word in the context or scope SCOPE.
SCOPE should be a symbol like 'dictionary' or 'vocabulary'."
  (cdr (assq scope (folio-dictionary-extra-slot
                    folio-vocabulary 1))))

(defun folio-vocabulary-set-current-word (word scope)
  "Set the current WORD in the context or scope SCOPE.
SCOPE should be a symbol like 'dictionary' or 'vocabulary'."
  (let ((current-word
         (assq-delete-all scope (folio-dictionary-extra-slot
                                 folio-vocabulary 1))))
    (folio-dictionary-set-extra-slot
     folio-vocabulary 1 (push (cons scope word)
                              current-word))))

(defun folio-locate-word (word &optional buffer-or-name)
  "Search for WORD in current buffer.
Move point to the new position possibly wrapping around.  Return
that position if another occurrence of WORD has been found, or
nil otherwise.  This is the hook function run from
`folio-word-occurrence-functions', or
`folio-word-substitution-functions'."
  (interactive "sWord: ")
  (let ((buffer (get-buffer (or buffer-or-name
                                (current-buffer))))
        (case-fold-search t)
        (word (regexp-opt (list word) 'words))
        (old-pos (point))
        beg end)
    (save-excursion
      (save-restriction
        (widen)
        (save-match-data
          (when (looking-at word)
            (goto-char (match-end 0)))
          (if (re-search-forward word nil t)
              (setq beg (match-beginning 0)
                    end (match-end 0))
            (goto-char (point-min))
            (if (and (re-search-forward word nil t)
                     (< (match-beginning 0) old-pos))
                (progn
                  (setq beg (match-beginning 0)
                        end (match-end 0))
                  (message "Searching from start of buffer"))
              (message "No more occurrences"))))))
    (when beg
      (folio-word-marker-at beg end)
      (folio-word-marker-recenter)
      (goto-char end))
    beg))

(defvar folio-word-min-context 4
  "")
(defvar folio-word-max-context 7
  "")

;; XXX defcustom folio-warn-replacement-altered-case--once, never
(defun folio-word-substitute-info (word)
  "XXX"
  (let ((page (folio-page-at-point))
;;        (replacement (match-string-no-properties 0))
        (replacement "FIXME")
        context)
    (save-excursion
      (let* ((bolp (line-beginning-position))
             (eolp (line-end-position 2))
             (pos (progn
                    (forward-word -1)
                    (point)))
             (left (progn
                     ;; Find beginning of subsentence for the left
                     ;; context.
                     (skip-syntax-backward "^." bolp)
                     (skip-syntax-forward " ")
                     (point)))
             (right (progn
                      ;; Find end of subsentence for the right
                      ;; context.  Seek and include punctuation but
                      ;; also include characters from word syntax
                      ;; class like apostrophe.
                      (skip-syntax-forward "^." eolp)
                      (skip-syntax-forward ".w" eolp)
                      (point)))
             (words 0))
        (goto-char pos)
;;        (forward-word -1)
        (message "left %s right %s: %d vs %d" (buffer-substring-no-properties
                                     left pos)
                 (buffer-substring-no-properties
                  pos right)
                 (- right pos) (- pos left))
        (message "pos %d left %d right %d" pos left right)
        (if (< (- right pos) (- pos left))
            (progn
              (message "XXX right")
              ;; Right context is the smaller part.  The context
              ;; should include no more than `folio-word-max-context'
              ;; words from the right context, filled up with some
              ;; words from the left context if the number of words is
              ;; less than `folio-word-min-context'.
              (setq left (point))
              (while (and (< (point) right)
                          (< words folio-word-max-context))
                (forward-word 1)
                (setq words (1+ words)))
              (setq right (point))
              (when (< words folio-word-min-context)
                (goto-char pos)
                (while (and (< words folio-word-min-context)
                            (> (point) left))
                  (forward-word -1)
                  (setq words (1+ words)))
                (setq left (point))))
          ;; Left context is the smaller part.
          (while (and (< words folio-word-max-context)
                      (> (point) left))
            (forward-word -1)
            (setq words (1+ words)))
          (message "XXX LEFT %d" words)
          (setq left (point))
          (goto-char pos)
          (when (< words folio-word-min-context)
            (while (and (< words folio-word-min-context)
                        (<= words folio-word-max-context)
                        (< (point) right))
              (forward-word 1)
              (setq words (1+ words))))
          ;; From right context again include punctuation and
          ;; characters from word syntax class like apostrophe.
          (skip-syntax-forward ".w" eolp)
          (setq right (point)))
        (setq context (buffer-substring-no-properties left right))
        (setq context (mapconcat (lambda (x) (if (string-equal x "\n") "" x))  (split-string context) " "))
))
    (message "GGG context %s" context)
    ;; Construct and report or file log entry.
;    (message "replace log %S" `(,page ,word ,replacement ,context))
    (list page word replacement context)))

(defun folio-replace-word (word replacement)
  "Replace WORD at point with REPLACEMENT.
If called interactively replace the word at or before point.  If
called from Lisp reposition the cursor at the next occurrence
possibly wrapping around if not looking exactly at WORD.  Leave
point after the replacement.  Propertize the replacement word for
visual effect and for use with `folio-next-correction' and
trigger a log entry.

With `folio-more-like-this' active repeat the command appropriately."
  (interactive (let ((word (substring-no-properties
                            (thing-at-point 'word))))
                 (list
                  (read-string (format
                                "Word (%s): " word) nil nil word)
                  (read-string "Replacement: "))))
  (when (called-interactively-p 'any)
    (folio-beginning-of-word))
  (let ((case-fold-search t))
    (save-match-data
      (if (looking-at (concat "\\<" (regexp-quote word) "\\>"))
          (atomic-change-group
            ;; Alter case of replacement text.  Leave point after
            ;; replacement.
            (replace-match replacement nil)
            (when (called-interactively-p 'any)
              (recenter))
            (folio-log-spelling-correction
             (folio-word-substitute-info word)))
        (if (called-interactively-p 'any)
            (error "No occurrence of `%s' at point" word)
          ;; Set point to the next occurrence possibly wrapping
          ;; around.  Let the user reissue the substitution.
          (push-mark)
          (folio-locate-word word))))))


(defun folio-word-marker-at-p (pos &optional end)
  "Check if the misspelling marker intersects with POS.
If END is non-nil read the arguments as a region."
  (let* ((mark-beg (and folio-word-marker
                        (overlay-start folio-word-marker)))
         (mark-end (and mark-beg (overlay-end folio-word-marker))))
    (when (and mark-beg
               (>= mark-beg pos)
               (if end
                   (<= mark-end end)
                 (<= pos mark-end)))
      (cons mark-beg mark-end))))

(defvar folio-spellcheck-props
  '(folio-spellcheck folio-misspelled folio-doublon)
  "*List of text properties used in spell-checking.")

(defun folio-spellcheck-propertize (beg end props)
  "Mark a region interesting by adding spellchecker properties.
BEG and END are buffer positions.  PROPS should be a plist with
text properties from `folio-spellcheck-props'.  Refontification
should be performed by calling `font-lock-fontify-region' or
`font-lock-fontify-buffer' at an appropriate time.  This is
omitted here for performance reasons."
  (folio-propertize-region
   beg end `(front-sticky ,folio-spellcheck-props
                          folio-spellcheck t ,@props)))

(defsubst folio-spellcheck-unpropertize (&optional beg end)
  "Remove any text properties of the spellchecker.
BEG and END restrict the operation to a region.  If omitted, the
respective buffer beginning or end position is used."
  (folio-unpropertize-region
   (or beg (point-min)) (or end (point-max))
   (append folio-spellcheck-props
           '(folio-spellcheck-skip)) 'refontify))

(defun folio-spellcheck-skip (objects)
  "Prepare buffer for regions not to spell-check.

OBJECTS should be a regexp matching semantic objects in the
buffer for which no spell-checking should be done, like proofer's
notes, page boundary markers, etc."
  (cond
   ((stringp objects)
    (folio-propertize-region
     (point-min) (point-max)
     '(rear-nonsticky (folio-spellcheck-skip)
                      folio-spellcheck-skip t)
     objects))
    (t
     (signal 'wrong-type-argument (list 'stringp objects)))))

(defconst folio-spellcheck-skip-keywords-default
  (let ((tags '("i" "b" "g" "f" "u" "sc" "tb"))
        (notes '("Blank Page" "Illustration"
                 "Footnote:" "Sidenote:"
                 "Greek:" "Hebrew:" "Arabic:" "Chaldee:" "Syriac:")))
    (regexp-opt
     (append (mapcar (lambda (x) (concat "<" x ">")) tags)
             (mapcar (lambda (x) (concat "</" x ">")) tags)
             (mapcar (lambda (x) (concat "[" x)) notes))))
  "Keywords, tags, and note-like markup to skip when spell-checking.")

(defconst folio-spellcheck-skip-regexp-default
  (let ((page-separator "\\(?:^-----File: .+$\\)")
        (proofer-note "\\[\\(?:\\*\\*[^]]+\\)"))
    (concat page-separator "\\|" proofer-note))
  "General regexp extending `folio-spellcheck-skip-keywords-default'
for identifying stretches of text to skip when spell-checking.")

(defsubst folio-vocabulary-build-interrupted-p ()
  "Return t when input has arrived while building the vocabulary.
Update `folio-vocabulary-build-interrupted' if input has arrived.  This
function is not idempotent since `folio-vocabulary-build-interrupted'
is reset when read."
  (or (when folio-vocabulary-build-interrupted
        (setq folio-vocabulary-build-interrupted nil) t)
      (setq folio-vocabulary-build-interrupted
            (and (or (input-pending-p)
                     (active-minibuffer-window)
                     executing-kbd-macro
                     defining-kbd-macro) t))))

(defun folio-spellcheck-word (word language) ;; XXX interactive
  (interactive (list (current-word nil t) (folio-primary-dictionary)))
  (folio-with-spellcheck-language language
    (folio-spellcheck-send-data word)
    (folio-spellcheck-receive-data)))

(defun folio-vocabulary-process-word (word pos doublon)
  "Process WORD at POS before updating the vocabulary table.
DOUBLON if non-nil marks WORD as a doublon."
  (let* ((entry (gethash word folio-next-vocabulary))
         (count (folio-vocabulary-entry-count entry))
         (dict (when (zerop count)
                 (let ((poss (progn
                               (folio-spellcheck-send-data word)
                               (folio-spellcheck-receive-data))))
                   (when poss
                     (cons folio-spellcheck-current-language
                           (cdr poss))))))  ; miss-list
         (sort-key (when (zerop count)
                     (folio-uca-sort-key word)))
         props)
    (when dict
      (setq props (plist-put props 'folio-misspelled t)))
    (when (or doublon (and entry
                           (folio-vocabulary-entry-misspelled-p
                            entry)
                           (not (zerop count))))
      (setq props (plist-put props 'folio-doublon t))
      ;; Propertize sibling ...
      (if doublon
          (folio-spellcheck-propertize
           doublon (+ doublon (length word)) props)
        ;; ... or a later occurrence
        (folio-spellcheck-propertize
         pos (+ pos (length word)) props)))
    ;; ... and the original itself.
    (when (or dict doublon)
      (folio-spellcheck-propertize
       pos (+ pos (length word)) props))
    ;; Update table data.
    (puthash word (folio-vocabluary-update-entry entry (1+ count)
                   dict sort-key) folio-next-vocabulary)))

(defun folio-vocabulary-process-chunk (buffer beg end)
  "Collect and process words in the region defined by BEG and END.
This function moves point.  Also match data is modified."
  (goto-char beg)
  (while (re-search-forward folio-vocabulary-regexp end t)
    (unless (and (null folio-apostrophe-always-word-constituent)
                 (eq (char-before) ?')
                 (member (char-syntax (or (char-before 2) ?w))
                         '(?\ ?.)))
      (let* ((check (match-string 1))
             (skip (get-text-property
                    0 'folio-spellcheck-skip check)))
        (unless skip
          (let ((word (substring-no-properties check))
                (pos (match-beginning 1))
                (doublon (match-beginning 3)))
            ;; font-lock fontification of the region is deferred to the
            ;; `folio-vocabulary-build-progress-hooks'.
            (folio-vocabulary-process-word word pos doublon)))))
    (set-marker folio-vocabulary-marker (point))))

;; XXX folio-dict-entry-substitute-functions

(defun folio-vocabulary-build-active-p (&optional buffer-or-name)
  "Return non-nil if a vocabulary build is in progress.
BUFFER-OR-NAME is the buffer object or buffer name to check.  If
omitted only the existence of an active process is checked."
  (and (folio-timer-running-p 'vocabulary)
       (or (null buffer-or-name)
           (memq (get-buffer buffer-or-name)
                 (folio-spellcheck-get-all :buffer)))))

(defun folio-vocabulary-process-chunks ()
  "Process a contiguous sequence of text chunks.
Return t if at end of buffer.  The size of a chunk is about the
length of a \"sentence\".  Processing is interrupted whenever
pending input is observed."
  (let* ((buffer (current-buffer))
         (chunk 0)
         (max-chunks (max (or folio-vocabulary-build-chunks
                              most-positive-fixnum) 1)))
    (unless folio-vocabulary-marker
      (setq folio-vocabulary-marker (make-marker))
      (set-marker folio-vocabulary-marker (point-min)))
    (folio-with-spellcheck-language
        (folio-spellcheck-current-dictionary-language buffer)
      (while (and (< chunk max-chunks)
                  (not (folio-vocabulary-build-interrupted-p)))
        ;; Yield for updating the display.
        (folio-yield chunk)
        (save-excursion
          (goto-char (marker-position folio-vocabulary-marker))
          ;; Defer C-g quitting to keep marker and table data in
          ;; sync.
          (let ((inhibit-quit nil))
            (unless (zerop (skip-syntax-forward "^w"))
              (set-marker folio-vocabulary-marker (point)))
            (unless (setq folio-vocabulary-build-interrupted (eobp))
              (let* ((beg (point))
                     (end (progn
                            ;; Faster than `forward-sentence' and serves
                            ;; the purpose of chunking just as well.
                            (forward-line 4)
                            (point))))
                ;; At this point progress should be ensured or there
                ;; might be a logic error causing the function to run
                ;; forever.  A simple assertion like
                ;; (assert
                ;;   (> beg (marker-position folio-vocabulary-marker)))
                ;; however won't do since errors from a timer function
                ;; apparently effectively make the timer dysfunctional.
                ;; The timer function also can be intercepted by user
                ;; activity between invocations such that the assertion
                ;; would fail because the marker has not moved.
                (folio-vocabulary-process-chunk buffer beg end)))))
        (setq chunk (1+ chunk))
        ;; Report progress.
        (let* ((pos (marker-position folio-vocabulary-marker))
               (progress (if (= pos (point-max))
                             (progn
                               ;; Independent of any hook function that
                               ;; may or may not run.
                               (font-lock-fontify-buffer)
                               'done)
                           (list (point-min) pos (point-max)))))
          (run-hook-with-args
           'folio-vocabulary-build-progress-hooks buffer progress))))
    (eq (marker-position folio-vocabulary-marker) (point-max))))

(defvar folio-vocabulary-progress-indicator nil
  "")
(make-variable-buffer-local 'folio-vocabulary-progress-indicator)

(defvar folio-vocabulary-progress-last-update (float-time)
  "")
(make-variable-buffer-local 'folio-vocabulary-progress-last-update)

(defun folio-vocabulary-build-dictionary ()
  "Construct a dictionary data structure.
The dictionary is constructed from the hash-table
`folio-next-vocabulary' with the keys sorted by the Unicode
Collation Algorithm."
  (let* ((lessp (lambda (x y)
                 (folio-uca-lessp
                  (folio-vocabulary-entry-sort-key (cdr x))
                  (folio-vocabulary-entry-sort-key (cdr y)))))
         (table-data (sort (folio-hash-table-to-alist
                            folio-next-vocabulary) lessp)))
    ;; Slot 0 caches characters from the vocabulary with diacritical
    ;; marks; slot 1 maintains currently selected words from the
    ;; vocabulary for the various views.
    (setq folio-vocabulary
          (folio-make-dictionary table-data :extra-slots 2))))

(defun folio-vocabulary-build-progress (buffer progress)
  "Provide visual feedback for spell-checking and/or vocabulary build."
  (with-current-buffer buffer
    (when (or (eq progress 'done)
              (null folio-vocabulary-progress-indicator)
              (or (null folio-vocabulary-progress-last-update)
                  (< folio-vocabulary-progress-last-update
                      (+ (float-time) 1.0))))
      (if (or (null progress)
              (eq progress 'done))
          (progn
            (folio-load-good-words 'noerror)
            (with-temp-message "Building dictionary"
              (folio-vocabulary-build-dictionary))
            (folio-vocabulary-progress-update 'done)
            (message (concat
                      (format "%s: %d words, "
                              (buffer-name buffer)
                              (count-words (point-min) (point-max)))
                      (format "%d distinct words spell-checked, "
                              (folio-vocabulary-word-count))
                      (format "%d misses, "
                              (folio-vocabulary-miss-count))
                      (format "%s elapsed"
                              (folio-format-diff-time
                               (folio-spellcheck-get buffer :start))))))
        (let ((windows (get-buffer-window-list
                        buffer nil 'visible-frames))
              (marker (marker-position
                       folio-vocabulary-marker)))
          (dolist (window windows)
            (let* ((ra1 (elt progress 0))
                   (ra2 (elt progress 2))
                   (rb1 (window-start window))
                   (rb2 (window-end window))
                   (pos (truncate (folio-map-range
                                   ra1 ra2 rb1 rb2 (elt progress 1)))))
              (folio-vocabulary-progress-update pos)
              (let ((beg (window-start))
                    (end (window-end)))
                (when (and (>= marker beg) (<= marker end))
                  ;; Font-Lock sometimes does move point (sic!).
                  (save-excursion
                    (folio-font-lock-fontify-region beg end))
                  (force-mode-line-update))))))))))

;;;###autoload
(defun folio-next-misspelling (&optional type skip-doublon)
  "Move point forward to the next misspelling."
  (interactive (list 'folio-spellcheck current-prefix-arg))
  (setq type (or type 'folio-spellcheck))
  ;; XXX TODO add support for skipping doublons, primary or secondary
  (let ((pos (point))
        current next)
    ;; Skip current.
    (setq current (or (text-property-not-all
                       pos (point-max) type t) pos))
    ;; Seek next.
    (setq next (or (text-property-any
                    current (point-max) type t) current))
    (if (> next current)
        (setq pos (goto-char next))
      ;; Wrap around and seek next.
      (setq next (and (> pos (point-min))
                      (text-property-any
                       (point-min) (1- pos) type t)))
      (if next
          (progn
            (when (called-interactively-p 'any)
              (message "Wrapped around"))
            (setq pos (goto-char next)))
        (when (called-interactively-p 'any)
          (message "No more misspellings"))
        pos))))

;;;###autoload
(defun folio-next-doublon ()
  (interactive)
  (let ((current-prefix-arg 'folio-doublon))
    (call-interactively 'folio-next-misspelling)))

;;;###autoload
(defun folio-correct-word ()
  "Interactively correct word at or before point."
  (interactive)
  (save-excursion
    (let ((word (progn
                  (when (folio-within-word-p)
                    (folio-beginning-of-word))
                  (when (and (looking-at "\\<\\(\\sw+\\)\\>")
                             (get-text-property
                              0 'folio-misspelled (match-string 0)))
                    (match-string 0)))))
      ;; XXX TODO
      (message "folio-correct-word %s" word))))

;; XXX TODO on-the-fly spell-checking
;; (defun folio-vocabulary-after-change (start end old-len)
;;   "Spell-check after a text change.
;; START, END, and OLD-LEN have the usual meanings, see `XXX'.")

(defun folio-spellcheck-font-lock-matcher (bound prop)
  "Return a FontLock matcher suitable for spell-checking.
Perform buffer scanning within the boundaries of point and BOUND.
PROP is the text property to check."
  (let ((opoint (point))
        match)
    (while (and (null match) (< (point) bound))
      (let ((props (text-properties-at (point)))
            (next-change (or (next-property-change
                              (point) (current-buffer) bound)
                             bound)))
        (if (and (plist-get props prop)
                 (looking-at "\\<\\(\\sw+\\)\\>"))
            (progn
              (goto-char (match-beginning 0))
              (setq match t))
          (goto-char next-change))))
    match))

(defsubst folio-spellcheck-font-lock-misspelled (bound)
  "Return a FontLock matcher for misspelled words."
  (folio-spellcheck-font-lock-matcher bound 'folio-misspelled))

(defsubst folio-spellcheck-font-lock-doublon (bound)
  "Return a FontLock matcher for doublons."
  (folio-spellcheck-font-lock-matcher bound 'folio-doublon))

(defvar folio-spellcheck-font-lock-misspelled-keywords
  (list (list #'folio-spellcheck-font-lock-misspelled
              0 (quote 'folio-misspelled) 'prepend))
  "FontLock keywords for misspelled words using matcher.")

(defvar folio-spellcheck-font-lock-doublon-keywords
  (list (list #'folio-spellcheck-font-lock-doublon
              0 (quote 'folio-doublon) 'prepend))
  "FontLock keywords for doublons using matcher.")

(defun folio-spellcheck-font-lock (&optional disable)
  "Enable or disable FontLock for spell-checking.
If DISABLE is nil register FontLock keywords using a suitable matcher,
otherwise remove the keywords."
  (if disable
      (progn
        (font-lock-remove-keywords
         nil folio-spellcheck-font-lock-misspelled-keywords)
        (font-lock-remove-keywords
         nil folio-spellcheck-font-lock-doublon-keywords))
    (font-lock-add-keywords
     nil folio-spellcheck-font-lock-misspelled-keywords)
    (font-lock-add-keywords
     nil folio-spellcheck-font-lock-doublon-keywords))
  (font-lock-fontify-buffer))


;;; Hooks

(defun folio-spellcheck-kill-buffer-hook ()
  "Normal hook for the `kill-buffer' operation."
  (folio-spellcheck-set-buffer (current-buffer) nil))

(defun folio-spellcheck-change-major-mode-hook ()
  "Normal hook for changing the major mode change."
  (folio-spellcheck-set-buffer (current-buffer) nil))

;; TODO after-change, outline-view-change-hook?

(defun folio-spellcheck-add-local-hooks (buffer &optional remove)
  "Register or deregister local mode hooks."
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook
                     . folio-spellcheck-kill-buffer-hook)
                    (change-major-mode-hook
                     . folio-spellcheck-change-major-mode-hook)
                    ;; Progress indicator, fringe marker or message.
                    (folio-vocabulary-build-progress-hooks
                     . folio-vocabulary-build-progress)
                    (folio-word-occurrence-functions
                     . folio-locate-word)
                    (folio-dict-maintainance-functions
                     . folio-maintain-dictionary)
                    (folio-word-substitution-functions
                     . folio-replace-word)))
      (if remove
          (remove-hook (car hook) (cdr hook) t)
        (add-hook (car hook) (cdr hook) nil t)))))

(defun folio-spellcheck-remove-local-hooks (buffer)
  "Deregister local mode hooks."
  (folio-spellcheck-add-local-hooks buffer 'remove))


;;;; Mode setup and teardown

;;;###autoload
(defun folio-spellcheck-init-dictionaries ()
  "Determine current buffer language and setup dictionaries."
  ;; XXX TODO dictionaries from project save file
  (let ((lang (or (folio-primary-dictionary)
                  (folio-guess-language))))
    (when (and lang (member lang (folio-dictionary-list)))
      (folio-change-dictionary lang)
      lang)))

;;;###autoload
(defun folio-spellcheck-mode-enable ()
  "Turn on Folio spell-checking."
  (folio-spellcheck-font-lock)
  (folio-spellcheck-add-local-hooks (current-buffer))
  (if (folio-spellcheck-init-dictionaries)
      (progn
        (folio-yield)
        (folio-build-vocabulary (current-buffer) 'force))
    ;; Only notify, don't cancel mode activation.
    (message "No dictionary language set"))
  (force-mode-line-update)
  (folio-yield))

;;;###autoload
(defun folio-spellcheck-mode-disable ()
  "Turn off Folio spell-checking."
  (folio-vocabulary-build-interrupt (current-buffer))
  (folio-spellcheck-remove-local-hooks (current-buffer))
  (save-excursion
    (folio-spellcheck-unpropertize))
  (folio-spellcheck-font-lock 'disable)
  (force-mode-line-update)
  (folio-yield))

;;;###autoload
(defvar folio-spellcheck-mode-map) ;; Forward declaration.

;;;###autoload
(defcustom folio-spellcheck-use-meta-tab t
  "Non-nil means that Folio uses M-TAB to correct a word."
  :group 'folio-spellcheck
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (define-key folio-spellcheck-mode-map (kbd "M-<tab>")
           (if (set sym val)
               'folio-spellcheck-correct-word))))

;;;###autoload
(defun folio-spellcheck-make-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-,") 'folio-next-misspelling)
    (define-key map (kbd "M-,") 'folio-next-doublon)
    (define-key map (kbd "C-.") 'folio-correct-word)
    (when folio-spellcheck-use-meta-tab
      (define-key map (kbd "M-<tab>") 'folio-correct-word))
    map))

;;;###autoload
(defvar folio-spellcheck-mode-map (folio-spellcheck-make-mode-map)
  "Minor mode key-map for Folio spell-check mode.")

;;;###autoload
(add-to-list 'minor-mode-map-alist
             `(folio-spellcheck-mode . ,folio-spellcheck-mode-map) t)

;;;###autoload
(defun folio-spellcheck-mode-lighter ()
  "Render the lighter for Folio spellcheck mode.
Return a string appropriately propertized for display in the mode-line."
  (let ((lang (folio-primary-dictionary))
        (pos (or (and folio-vocabulary-marker
                      (marker-position folio-vocabulary-marker)) 0))
        label)
    (if lang
        (setq label (propertize
                     lang 'help-echo (folio-language-info
                                      lang 'name)))
      (setq label (propertize
                   "--" 'help-echo "No dictionary set")))
    (propertize (concat "[" label "]")
                'face (cond
                       ((null lang)
                        'folio-spellcheck-inactive)
                       ((< (point) pos)
                        'folio-spellcheck-done)
                       ((folio-vocabulary-build-active-p)
                        'folio-spellcheck-active)
                       (t
                        'folio-spellcheck-inactive)))))

;;;###autoload
(define-minor-mode folio-spellcheck-mode
  "Toggle Folio spell-checking.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Folio spell-check mode is a buffer-local minor mode.  It is most
useful in combination with a Folio major mode but not generally
restricted to it."
  :lighter (:eval (folio-spellcheck-mode-lighter))
  :keymap folio-spellcheck-mode-map
  :group 'folio-spellcheck
  (if folio-spellcheck-mode
      (condition-case err
          (progn
            (when (fboundp 'turn-off-flyspell)
              (turn-off-flyspell))
            (folio-spellcheck-mode-enable))
        (error (message "Error enabling Folio spell-check mode:\n%s"
                        (cdr err)))
        (folio-spellcheck-mode -1))
    (folio-spellcheck-mode-disable)
    (setq folio-spellcheck-mode nil)))

;;;###autoload
(custom-add-option 'folio-text-mode-hook 'folio-spellcheck-mode-enable)

;;;###autoload
(defun turn-on-folio-spellchecking ()
  "Unconditionally turn on Folio spell-check mode."
  (interactive)
  (folio-spellcheck-mode 1))

;;;###autoload
(defun turn-off-folio-spellchecking ()
  "Unconditionally turn off Folio spell-check mode."
  (interactive)
  (folio-spellcheck-mode -1))

;;;###autoload
(defun folio-spellcheck-now ()
  "Spell-check the current buffer."
  (interactive)
  (turn-off-folio-spellchecking)
  (turn-on-folio-spellchecking))

;;;###autoload
(defun folio-spellcheck-abort ()
  "Abort spell-checking the current buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (folio-vocabulary-discard buffer)
    (message "Aborted spell-checking of `%s'"
             (buffer-name buffer))))

(defun folio-spellcheck (&optional buffer-or-name arg no-msg)
  (interactive (list (folio-completing-read-buffer)
                     current-prefix-arg
                     nil))
  (setq folio-vocabulary-marker nil
        folio-next-vocabulary nil) ;; XXX
  (let ((buffer (get-buffer buffer-or-name)))
    (when (if (folio-vocabulary-build-active-p buffer)
              (if arg  ; cancel
                  (progn
                    (let ((folio-vocabulary-type 'spellcheck))
                      (folio-vocabulary-discard buffer))
                    (unless no-msg
                      (message "Spell-check for `%s' canceled"
                               (buffer-name buffer)))
                    nil)
                (unless no-msg
                  (message "Spell-check for `%s' in progress..."
                           (buffer-name buffer)))
                nil)
            (not arg))
      (let ((folio-vocabulary-type 'spellcheck))
        (folio-build-vocabulary buffer)
        (unless no-msg
          (message "Spell-checking `%s'..." (buffer-name buffer)))))))

(defun folio-vocabulary-progress-update (pos)
  (if folio-vocabulary-progress-indicator
      (if (eq pos 'done)
          (progn
            (folio-fringe-unmark
             folio-vocabulary-progress-indicator)
            (setq folio-vocabulary-progress-indicator nil))
        (folio-fringe-move-mark
         folio-vocabulary-progress-indicator pos)
        (setq folio-vocabulary-progress-last-update
              (float-time)))
    (unless (eq pos 'done)
      (setq folio-vocabulary-progress-indicator
            (folio-fringe-mark 'left-triangle pos)
            folio-vocabulary-progress-last-update (float-time)))))

(defun folio-vocabulary-build-interrupt (&optional target)
  "Interrupt a vocabulary build or active spellchecker.
TARGET should be a buffer or a buffer name or the symbol
cancel-all in which case the process is stopped for all buffers
currently being processed."
  (let ((buffers (if (eq target 'all)
                     (folio-spellcheck-get-all :buffer)
                   (list (get-buffer
                          (or target (current-buffer)))))))
    (dolist (buffer buffers)
      (folio-spellcheck-put buffer :done t)
      (folio-spellcheck-set-buffer buffer 'release)
      (with-current-buffer buffer
        (folio-vocabulary-progress-update 'done)
        (force-mode-line-update))))
  (unless (folio-spellcheck-next-buffer-p :done)
    (folio-cancel-timer 'vocabulary)
    (setq folio-vocabulary-build-interrupted t)))

(defun folio-vocabulary-process-buffer ()
  "Select a buffer and process some text chunks.
When done with a buffer run the hook `folio-vocabulary-build-done-hooks'.
Deactivate the word frequency collector when all registered buffers
have been processed."
  (setq folio-vocabulary-build-interrupted nil)
  ;; Find a buffer to process in this time slot.  The buffer of the
  ;; selected window is preferred over buffers from the active window
  ;; list.
  (let ((buffer (folio-spellcheck-next-buffer :done)))
    (cond
     ((null buffer)
      ;; No buffers to process.
      (folio-vocabulary-build-interrupt)
      nil)
     ((folio-vocabulary-build-interrupted-p)
      ;; Reschedule to resume after handling pending input.
      t)
     (t
      (let (minibuffer-auto-raise)
        (with-current-buffer buffer
          ;; Process some chunks until interrupted by pending input.
          (when (folio-vocabulary-process-chunks)
            (folio-spellcheck-put buffer :done t)
            ;; Report buffer as done.
            (run-hook-with-args
             'folio-vocabulary-build-done-hooks buffer))))
      (if (folio-spellcheck-next-buffer-p :done)
          (progn
            (unless (folio-timer-running-p 'vocabulary)
              (folio-schedule-timer
               'vocabulary folio-vocabulary-build-delay))
            t)
        (folio-vocabulary-build-interrupt buffer)
        nil)))))

(defvar folio-vocabulary-type nil
  "")

(defun folio-build-vocabulary (buffer-or-name &optional force sync)
  "Asynchroneously start collecting the vocabulary for BUFFER-OR-NAME.
This normally includes word frequencies as well as the results
from a spellchecker.  BUFFER-OR-NAME must be a either buffer or
the name of an existing buffer.  `folio-vocabulary-cancel-process'
cancels an active process; `folio-vocabulary-build-active-p'
can be used for querying, see which."
  (let ((buffer (get-buffer buffer-or-name)))
    (folio-vocabulary-discard buffer)
    (folio-spellcheck-set-buffer buffer)
    (folio-spellcheck-put buffer :start (current-time))
    (with-current-buffer buffer
      (save-excursion
        (folio-spellcheck-skip
         (concat folio-spellcheck-skip-keywords-default "\\|"
                 folio-spellcheck-skip-regexp-default)))
      (unless (and folio-next-vocabulary folio-vocabulary-marker)
        (setq folio-next-vocabulary (folio-make-vocabulary)
              folio-vocabulary-marker nil))
      ;; `vocabulary' is an idle timer called each time Emacs has been
      ;; idle for `folio-vocabulary-build-delay' seconds with
      ;; `folio-vocabulary-process-buffers' for the worker function.
      ;; While idle processing continues but is interrupted every
      ;; `folio-vocabulary-build-pause' seconds.
      (unless (folio-timer-running-p 'vocabulary)
        (if sync
            (folio-vocabulary-process-buffer)
          (folio-schedule-timer
           'vocabulary folio-vocabulary-build-delay))))))

(defun folio-build-vocabulary-synchroneous (buffer-or-name force)
  "Asynchroneously start assembling a vocabulary for BUFFER-OR-NAME.
This normally includes word frequencies as well as the results
from a spellchecker.  BUFFER-OR-NAME must be a either buffer or
the name of an existing buffer.  `folio-vocabulary-cancel-process'
cancels an active process; `folio-vocabulary-build-active-p'
can be used for querying, see which."
;;  (interactive (list (folio-completing-read-buffer) 'force))
  (folio-build-vocabulary buffer-or-name force 'sync))

(defun folio-vocabulary-discard (&optional buffer-or-name)
  "Discard any buffer local vacabulary lists collected so far.
BUFFER-OR-NAME is a buffer object or name.  If nil apply this
command to the current buffer."
  (interactive (list (folio-completing-read-buffer)))
  (let ((buffer (get-buffer (or buffer-or-name
                                (current-buffer)))))
    (folio-vocabulary-build-interrupt buffer)
    (with-current-buffer buffer
      (when folio-next-vocabulary
        (setq folio-next-vocabulary
              (clrhash folio-next-vocabulary)))
      (setq folio-vocabulary-marker nil)
      (save-excursion
        (folio-spellcheck-unpropertize))
      (folio-spellcheck-unset-buffer buffer))))


(provide 'folio-etaoin-shrdlu)

;;; folio-etaoin-shrdlu.el ends here
