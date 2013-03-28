;;; folio-spellcheck.el --- Spell-checking support for Folio mode

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

;; This package provides basic spell-checking functionality for Folio
;; mode.  Supported spell-checking engines are aspell, hunspell, and
;; with Aquamacs also NSSpellChecker which interfaces the Mac OS X
;; spell-checker based on hunspell.  Spell-checker engines can be used
;; concurrently in that an engine can be customized to handle a
;; particular language.  If no preference is assigned, the first
;; engine supporting the chosen language is used.  Dictionaries are
;; selected by assigning a unified BCP 47 language tag for the
;; spell-checker language, independent of any spell-checker dependent
;; dictionary names.  This language tag can also be used for
;; attributing XHTML output formats with xml:lang.

;; Spell-checking basically is setup and applied by
;; `folio-with-spellcheck-language', `folio-spellcheck-send-data', and
;; `folio-spellcheck-receive-data':

;;  (folio-with-spellcheck-language "en-US"
;;    (folio-spellcheck-send-data (list "check" "my" "bad" "spellig")))
;;    (folio-spellcheck-receive-data)))
;;
;; `folio-spellcheck-receive-data' is implemented to return a list of
;; lists with the misspelled word in the car of a member, and any
;; suggestions reported by the spell-checker in the member's cdr.

;; The input encoding unconditionally is assumed to be UTF-8.

;; The high-level layer is in folio-etaoin-shrdlu.el which also
;; includes the actual text scanner extracting individual words and
;; the spell-checker drivers.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'folio-atoms)
(require 'folio-base)

(defvar folio-spellcheck-buffer-data nil
  "*List of property lists maintaining spell-checker data.")

(defconst folio-dictionary-alist-aspell
  (purecopy
   '(("ca" . "catalan")
     ("da" . "dansk")
     ("de-CH" . "swiss")
     ("de-DE" . "deutsch") ;; i.e. de-1996
     ("de-1901" . "deutsch-alt")
     ("en" . "english")
     ("en-US" . "american")
     ("en-GB" . "british")
     ("es" "castellano")
     ;; ("XXX" . "czech")
     ("fi" . "finnish")
     ("fr" . "francais")
     ("fr-CH" . "suisse")
     ("it" . "italiano")
     ("grc" . "grc")
     ("he" . "hebrew")
     ;; ("XXX" . "polish")
     ("nl" . "dutch")
     ("nno" . "nynorsk")
     ("nob" . "bokmal")
     ("pt-PT" . "portugues") ;; XXX pt-BR
     ;; ("XXX" . "russian") ; koi8-r
     ;; ("XXX" . "slovak")
     ;; ("XXX" . "slovenian")
      ("sv" . "svenska")))
  "Alist mapping language to dictionary name and vice versa.
This alist is used for Aspell.")

(defconst folio-dictionary-alist-hunspell
  (purecopy
   '(("ca" . "ca_ES")
     ("cy-GB" . "cy_GB")
     ("da" . "da")
     ("de" . "de_DE")
     ("de-1901" . "de_DE-old")
     ("el" . "el_GR")
     ("en-GB" . "en_GB")
     ("en-GB-oed" . "en_GB-oed")
     ;; ("en_AU" . "en_AU")
     ;; ("en_CA" . "en_CA")
     ("en-US" . "en_US")
     ("es" . "es_ES")
     ("fr" . "fr")
     ("it" . "it")
     ("nob" . "nb_NO")
     ("nl" . "nl_NL")
     ("nno" . "nn_NO")
     ;; ("pt_BR" . "pt_BR")
     ("pt-PT" . "pt_PT")
     ;; ("ru" . "ru")
     ;; ("sv" . "sv")
     ("he" . "he_IL")))
  "Alist mapping language to dictionary name and vice versa.
This alist is used for hunspell.")

(defconst folio-dictionary-alist-ns-spellchecker
  (purecopy
   '(("da" . "da")
     ("de" . "de")
     ("el" . "el")
     ("en" . "en")
     ;; ("en_AU" . "en_AU")
     ;; ("en_CA" . "en_CA")
     ("en-GB" . "en_GB")
     ("es" . "es")
     ("fr" . "fr")
     ("it" . "it")
     ;; ("nl" . "nl")
     ;; ("pt_BR" . "pt_BR")
     ("pt-PT" . "pt_PT")
     ;; ("ru" . "ru")
     ;; ("sv" . "sv")
     ("he" . "he")))
  "Alist mapping language to dictionary name and vice versa.
This alist is used for NSSpellChecker.")

(defconst folio-spellcheck-engine-alist
  '(("Default" . nil)
    ("Aspell" . aspell)
    ("Hunspell" . hunspell)
    ("NSSpellChecker" . ns-spellchecker))
  "Alist of supported spell-checker engines.")

(defcustom folio-aspell-program
  (locate-file "aspell" exec-path exec-suffixes 'file-executable-p)
  "File name of the Aspell program."
  :type '(choice
          (const :tag "Unavailable" nil)
          (file :tag "File"))
  :group 'folio-spellcheck
  :group 'folio-external)

(defcustom folio-hunspell-program
  (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p)
  "File name of the Hunspell program."
  :type '(choice
          (const :tag "Unavailable" nil)
          (file :tag "File"))
  :group 'folio-spellcheck
  :group 'folio-external)

(defvar folio-dictionaries nil
  "*List of dictionaries to use with the current project.
The primary dictionary is stored in the car; secondary
dictionaries are in the cdr.")
(make-variable-buffer-local 'folio-dictionaries)

(defvar folio-dictionaries-history nil
  "History of entered dictionary names.
This variable is maintained with `folio-dictionaries' but
globally.")

(defvar folio-dictionary-change-hook nil
  "Hook run when the primary or secondary dictionaries have changed.")

(defsubst folio-spellcheck-get-specific
  (key value &optional target-key)
  "Retrieve spell-checker buffer data.
KEY, VALUE and TARGET-KEY have the same meanings like in
`folio-buffer-data-get-specific', which see."
(folio-buffer-data-get-specific
 'folio-spellcheck-buffer-data key value target-key))

(defsubst folio-spellcheck-get (buffer &optional target-key)
  "Retrieve spell-checker buffer data.
BUFFER and TARGET-KEY have the same meanings like in
`folio-buffer-data-get', which see."
  (folio-buffer-data-get
   'folio-spellcheck-buffer-data buffer target-key))

(defsubst folio-spellcheck-put (buffer key value)
  "Store spell-checker buffer data.
BUFFER, KEY and VALUE have the same meanings like in
`folio-buffer-data-put', see which."
  (when (not (bufferp buffer))
    (error "Invalid spell-check buffer"))
  (folio-buffer-data-put
   'folio-spellcheck-buffer-data buffer key value))

(defsubst folio-spellcheck-get-all (&optional key)
  "Retrieve all spell-checker buffer data.
KEY has the same meanings like in `folio-buffer-data-get-all',
see which."
  (folio-buffer-data-get-all
   'folio-spellcheck-buffer-data key))

(defsubst folio-spellcheck-get-all-but (key except)
  "Retrieve all spell-checker buffer data filtered by a secondary key.
KEY and EXCEPT have the same meanings like in
`folio-buffer-data-get-all-but', which see."
  (folio-buffer-data-get-all-but
   'folio-spellcheck-buffer-data key except))

(defun folio-spellcheck-delete (buffer)
  "Delete all data associated with BUFFER."
  (let ((process (folio-spellcheck-get buffer :process)))
    ;; This package itself does not use process buffers but another
    ;; one might.
    (when process
      (when (process-buffer process)
        (kill-buffer (process-buffer process)))
      (delete-process process))
    (folio-buffer-data-delete
     'folio-spellcheck-buffer-data buffer)))

(defun folio-spellcheck-delete-all ()
  "Delete all buffer data used for spell-checking."
  (let ((buffers (remq nil (folio-spellcheck-get-all :buffer))))
    (mapc (lambda (x)
            (folio-spellcheck-delete x)) buffers))
  (setq folio-spellcheck-buffer-data nil))

(defun folio-spellcheck-next-buffer (&optional except)
  "Return the next buffer suitable for spell-checking."
  (if except
      (folio-preferred-process-buffer
       (folio-spellcheck-get-all-but :buffer except))
    (folio-preferred-process-buffer
     (folio-spellcheck-get-all :buffer))))

(defalias 'folio-spellcheck-next-buffer-p 'folio-spellcheck-next-buffer
  "Return non-nil if there is at least one buffer suitable for spell-checking.")

(defsubst folio-spellcheck-engine-list ()
  "Return a list of supported spell-checker engines."
  (delq nil (mapcar #'cdr folio-spellcheck-engine-alist)))

(defsubst folio-spellcheck-preferred-engine (lang)
  "Return the preferred spell-checker for LANG.
If none is defined return nil."
  (assoc lang folio-spellcheck-language-engine-alist))

(defun folio-dictionary-choices-list ()
  "Return a list of dictionaries not in `folio-dictionaries'."
  (let ((dicts (sort (folio-dictionary-list) 'string-lessp)))
    (folio-filter-list
     dicts (lambda (x)
             (not (member x folio-dictionaries))))))

(defsubst folio-primary-dictionary ()
  "Return the primary dictionary.
The return value is nil if none is set; there is no default."
  (car folio-dictionaries))

(defsubst folio-secondary-dictionaries ()
  "Return the list of secondary dictionaries which may be nil."
  (cdr folio-dictionaries))

(defun folio-spellcheck-current-dictionary-language (&optional buffer)
  "Return the current spell-checker language.
This is equivalent to the first parameter to
`folio-with-spellcheck-language'."
  (folio-spellcheck-get (or buffer (current-buffer)) :language))

;;;###autoload
(defun folio-change-dictionary (primary &optional secondary)
  "Change PRIMARY or SECONDARY dictionaries for spellchecking.
The dictionary always is only set \"locally\", just for the
current buffer.

If called interactively read the new primary dictionary name from
the minibuffer prompt.  Display the current primary dictionary if
the prompt is answered with just RET.

If called from Lisp set primary and secondary dictionaries to the
values of PRIMARY and SECONDARY, respectively.  Setting only the
primary dictionary resets all secondary dictionaries.

Run the normal hook `folio-dictionary-change-hook' if any
dictionary has changed."
  (interactive
   (let ((completion-ignore-case t))
     (list (completing-read
            "Use new dictionary (RET for current, SPC to complete): "
            (folio-dictionary-list)
            nil t nil 'folio-dictionaries-history) nil)))
  (assert (not (null primary)))
  (if (string= primary "") ; from completing read: query current
      (progn
        (setq primary (car folio-dictionaries))
        (if (called-interactively-p 'any)
            (if primary
                (message "Using dictionary `%s' (%s)"
                         primary (folio-language-info primary 'alt-name))
              (message "No dictionary set"))
          primary))
    (let ((old (car folio-dictionaries))
          changed)
      (if (string-equal old primary) ; non-nil per the assert above
          (when (called-interactively-p 'any)
            (message "Dictionary unchanged from `%s' (%s)"
                     primary (folio-language-info primary 'alt-name)))
        (setq changed (if folio-dictionaries
                          (setcar folio-dictionaries primary)
                        (setq folio-dictionaries (list primary))))
        (when (called-interactively-p 'any)
          (message "Using dictionary `%s' (%s)"
                   primary (folio-language-info primary 'alt-name))))
      (setq changed (or changed
                        (equal (cdr folio-dictionaries) secondary)))
      (setcdr folio-dictionaries secondary)
      (when changed
        (run-hooks 'folio-dictionary-change-hook))))
    folio-dictionaries)

(defun folio-language-dictionary (lang engine)
  "Return the dictionary name for the language name LANG."
  (cond
   ((eq engine 'ns-spellchecker)
    (cdr (assoc lang folio-dictionary-alist-ns-spellchecker)))
   ((eq engine 'aspell)
    (cdr (assoc lang folio-dictionary-alist-aspell)))
   ((eq engine 'hunspell)
    (cdr (assoc lang folio-dictionary-alist-hunspell)))))

(defun folio-dictionary-language (dict engine)
  "Return the language name for the dictionary name DICT."
  (cond
   ((eq engine 'ns-spellchecker)
    (car (rassoc dict folio-dictionary-alist-ns-spellchecker)))
   ((eq engine 'aspell)
    (car (rassoc dict folio-dictionary-alist-aspell)))
   ((eq engine 'hunspell)
    (car (rassoc dict folio-dictionary-alist-hunspell)))))

(defsubst folio-aspell-available-p ()
  "Return non-nil if aspell is available.
For a non-nil return value `folio-aspell-program' is a valid
argument to `call-process'."
  (and (stringp folio-aspell-program)
       (file-executable-p folio-aspell-program)))

(defun folio--aspell-config-value (key)
  "Return value of Aspell configuration option KEY.
Assumes that Aspell is installed and that the configuration value
contains no whitespace."
  ;; Adapted from ispell.el.
  (with-temp-buffer
    (car (delq "" (split-string
                   (with-temp-buffer
                     (call-process
                      folio-aspell-program nil t nil "config" key)
                     (buffer-substring-no-properties
                      (point-min) (point-max))))))))

(defun folio--aspell-dictionary-directory ()
  "Return Aspell's directory for dictionaries.
The Aspell program must be installed."
  (folio--aspell-config-value "dict-dir"))

(defun folio--aspell-dictionary-alias-names ()
  "Return an alist with the alias names for Aspell dictionaries."
  (let ((alias-files (file-expand-wildcards
                      (concat (folio--aspell-dictionary-directory) "/*.alias")))
        aliases)
    (mapc (lambda (x)
            (with-temp-buffer
              (insert-file-contents-literally x)
              ;; From "add NAME.multi" extract NAME.
              (when (re-search-forward "^add \\([^.]+\\)\\.multi" nil t)
                (let* ((alias (file-name-sans-extension
                               (file-name-nondirectory x)))
                       (name (match-string-no-properties 1))
                       (entry (assoc name aliases)))
                  (if entry
                      (setcdr entry (cons alias (cdr entry)))
                    (setq aliases (cons (cons name (list alias)) aliases)))))))
          alias-files)
    aliases))

(defun folio--aspell-dictionary-names ()
  "Return a list of Aspell dictionary names."
  (delq "" (split-string
            (with-temp-buffer
              (call-process
               folio-aspell-program nil t nil "dicts")
              (buffer-substring-no-properties (point-min) (point-max))))))

(defun folio-aspell-dictionary-names ()
  "Return a list of Aspell dictionaries names with aliases.
The car of a list member is the dictionary name, any alias
names are in the cdr."
  (let ((aliases (folio--aspell-dictionary-alias-names))
        (dicts (folio--aspell-dictionary-names))
        names)
    (mapc (lambda (x)
            (setq names
                  (cons (append (list x) (cdr (assoc x aliases)))
                        names))) dicts)
    names))

(defun folio-aspell-dictionaries ()
  "Return a list of dictionaries for Aspell.
The dictionary names are specific to Aspell.  A reverse mapping to
canonical BCP 47 language codes is performed by `folio-dictionary-language'."
  (when (folio-aspell-available-p)
    (let ((dicts (folio-flatten (folio-aspell-dictionary-names))))
      (delq nil
            (mapcar (lambda (x)
                      (and (rassoc x folio-dictionary-alist-aspell)
                           x)) dicts)))))

(defsubst folio-hunspell-available-p ()
  "Return non-nil if hunspell is available.
For a non-nil return value `folio-hunspell-program' is a valid
argument to `call-process'."
  (and (stringp folio-hunspell-program)
       (file-executable-p folio-hunspell-program)))

(defun folio-hunspell-dictionaries ()
  "Return a list of dictionaries for hunspell.
The dictionary names are specific to hunspell.  A reverse mapping to
canonical BCP 47 language codes is performed by `folio-dictionary-language'."
  (let (dicts)
    (with-temp-buffer
      (call-process
       folio-hunspell-program nil t nil "-D")
      (goto-char (point-min))
      (when (re-search-forward "^AVAILABLE DICTIONARIES")
        (forward-line)
        ;; Dealing with broken hunspell query "API" here.
        (while (not (or (eobp)
                        (looking-at-p "^Can't")))
          (setq dicts
                (cons (file-name-nondirectory
                       (buffer-substring-no-properties
                        (point) (line-end-position))) dicts))
          (forward-line))))
    dicts))

(defsubst folio-ns-spellchecker-available-p ()
  "Return non-nil if NSSpellChecker is available."
  (and (fboundp 'ns-spellchecker-check-spelling)
       (fboundp 'ns-spellchecker-get-suggestions)))

(defun folio-ns-spellchecker-dictionaries ()
  "Return a list of dictionaries for NSSpellChecker.
The dictionary names are specific to NSSpellChecker.  A reverse mapping to
canonical BCP 47 language codes is performed by `folio-dictionary-language'."
  (when (and (folio-ns-spellchecker-available-p)
             (fboundp 'ns-spellchecker-list-languages))
    (delq nil
          (mapcar (lambda (x)
                    (and (rassoc x folio-dictionary-alist-ns-spellchecker)
                         x))
                  (ns-spellchecker-list-languages)))))

(defun folio-spellchecker-available-p (engine)
  "Return non-nil if ENGINE is a valid spell-checker.
ENGINE should be a symbol from `folio-spellcheck-engine-list'."
  (let ((check (and engine (intern-soft
                            (format "folio-%s-available-p"
                                    (symbol-name engine))))))
    (and check (funcall check))))

(defun folio-spellchecker-language-available-p (lang engine)
  "Return non-nil if the spell-checker ENGINE supports LANG.
ENGINE should be a member of `folio-spellchecker-alist'.  The return
value is the dictionary name of ENGINE for language LANG."
  (let ((dicts (and engine (intern-soft
                            (format "folio-%s-dictionaries"
                                    (symbol-name engine)))))
        dict)
    (when dicts
      (car (member (folio-language-dictionary lang engine)
                   (funcall dicts))))))

(defun folio-spellchecker (lang)
  "Return a cons of spell-checker engine and dictionary for language LANG.
This value is determined by selecting the preferred engine from
`folio-spellcheck-language-engine-alist' falling back to the
first spell-checker from `folio-spellcheck-engine-list' supporting
LANG."
  (let* ((preferred (folio-spellcheck-preferred-engine lang))
         (engines (delq nil
                        (cons preferred
                              (delq preferred
                                    (copy-list
                                     (folio-spellcheck-engine-list))))))
         engine dict)
    (while (setq preferred (pop engines))
      (when (setq dict (folio-spellchecker-language-available-p
                        lang preferred))
        (setq engine preferred engines nil)))
    (when engine
      (cons engine dict))))

(defun folio-dictionary-list ()
  "Return a list of valid dictionaries.
Members are canonical BCP 47 language tags, independent of any
particular spell-checker engine.  The first two letters of a
member constitute the ISO-639-1 language code."
  (let ((engines (folio-spellcheck-engine-list))
        engine dicts)
    (while (setq engine (pop engines))
      (let ((dict-list (funcall
                        (intern-soft
                         (format "folio-%s-dictionaries"
                                 (symbol-name engine))))))
        (setq dicts (append (mapcar (lambda (x)
                                      (folio-dictionary-language
                                       x engine)) dict-list) dicts))))
    (sort (delete-duplicates dicts :test #'string-equal) #'string-lessp)))

(defcustom folio-spellcheck-language-engine-alist nil
  "*List associating a language code to a spell-checker engine.
`Default' means no particular spell-checker engine is preferred
which is equivalent to not assigning an engine at all.  If a
spell-checker engine is found to not support the assigned
dictionary language at run-time Folio mode uses the first
suitable engine instead.  The list of choices for the dictionary
should not be empty."
  :tag "Preferred spell-check engines"
  :type `(alist
          :key-type
          (choice :tag "Dictionary"
                  :format " %[%t%]: %v"
                  ,@(mapcar (lambda (x)
                              (list 'const :tag (format "%-8s" x)
                                    :format "%t" x))
                            (sort (folio-dictionary-list) #'string-lessp)))
          :value-type
          (choice :tag "Engine"
                  :format "   %[%t%]: %v"
                  ,@(mapcar (lambda (x)
                              (list 'const :tag (car x) (cdr x)))
                            folio-spellcheck-engine-alist)))
  :group 'folio-spellcheck)

;; XXX language options: max number of run-together words; apostrophe
;; as word constituent

;; XXX from language options assemble regexp for scanning for 'words'

(defun folio-spellcheck-set-buffer (buffer &optional undo)
  "Prepare BUFFER for spell-checking.
This function should be called before any spell-checking activity
starts or, with UNDO set to non-nil, after it has stopped for any
length of time, for instance by calling it from a mode hook."
  (when (not (bufferp buffer))
    (error "Invalid buffer"))
  (let ((old (remq nil (folio-spellcheck-get-all :process))))
    ;; Remove dead buffers.
    (mapc (lambda (x)
            (when (or (and undo (eq x buffer))
                      (not (buffer-live-p x)))
              (folio-spellcheck-delete x)))
          (folio-spellcheck-get-all :buffer))
    ;; Delete obsolete processes, i.e. processes in old but not in
    ;; new.
    (let ((new (remq nil (folio-spellcheck-get-all :process))))
      (mapc (lambda (x)
              (unless (memq x new)
                (when (process-buffer x)
                  (kill-buffer (process-buffer x)))
                (delete-process x))) old))))

(defmacro folio-with-spellcheck-language (language &rest body)
  "Switch the spell-checker to LANGUAGE and eval BODY.
The return value is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((the-buffer (make-symbol "the-buffer"))
        (the-engine (make-symbol "the-engine"))
        (the-dict (make-symbol "the-dict")))
    `(destructuring-bind (,the-buffer ,the-engine . ,the-dict)
         (cons (current-buffer) (or (folio-spellchecker ,language)
                                    (cons nil nil)))
       (cond
        ((eq ,the-engine 'aspell)
         (let ((process (folio-spellcheck-get
                         ,the-buffer :process))
               (process-language (folio-spellcheck-get
                                  ,the-buffer :language)))
         (when (or (null process)
                   (not (folio-process-running-p process))
                   (not (string= ,language process-language)))
           (let ((program folio-aspell-program)
                 (args (list "-a"
                             "-d"
                             ,the-dict
                             "--encoding=utf-8")))
             (when (null program)
               (error "Aspell spell-checker not available"))
             ;; Spawn process with pipe connection and mark it
             ;; internal to prevent Emacs from querying the user
             ;; when process or process buffer is about to get
             ;; killed.
             (setq process
                   (apply #'start-process
                          "folio-spellcheck" nil program args))
             (set-process-query-on-exit-flag process nil)
             ;; Put Aspell in terse mode.
             (process-send-string process "!\n")
             ;; Set the output handler and associated buffer.
             (set-process-filter
              process #'folio-spellcheck-process-filter)

             ;; Register actual engine, process and process language.
             (folio-spellcheck-put ,the-buffer :engine ,the-engine)
             (folio-spellcheck-put ,the-buffer :process process)
             (folio-spellcheck-put ,the-buffer :language ,language)))

         ;; Splice in the BODY forms.
         (with-current-buffer ,the-buffer
           ,@body)))
        ;; XXX hunspell
      ((eq ,the-engine 'ns-spellchecker)) ;; XXX
      ((null ,the-engine)
       (error "Undefined spell-checker engine"))
      (t
       (error "Unsupported spell-checker engine"))))))

(defun folio-spellcheck-process-filter (process output)
  "Process filter function for asynchronously receiving responses."
  (let* ((buffer (folio-spellcheck-get-specific
                  :process process :buffer))
         (pending (folio-spellcheck-get buffer :received)))
    (folio-spellcheck-put
     buffer :received (concat pending output))))

(defun folio-spellcheck-send-data (strings)
  (let* ((buffer (current-buffer))
         (engine (folio-spellcheck-get buffer :engine)))
    (cond
     ((eq engine 'aspell)
      (let ((process (folio-spellcheck-get buffer :process))
            (data (concat (mapconcat (lambda (x)
                                       (concat "^" x))
                                     strings "\n") "\n")))
        (process-send-string process data)))
     ((eq engine 'ns-spellchecker))
     (t
      (error "Unsupported spell-checker engine")))))

(defun folio-parse-aspell-suggestions (string)
  "Parse spelling suggestions for Aspell-compatible programs."
  (let (result)
    (when string
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (let ((search-spaces-regexp nil)) ;; Treat spaces literally.
          (save-match-data
            (while (re-search-forward
                    "^& \\([^ ]+\\) \\([0-9]+\\) [0-9]+: \\(.+\\)$" nil t)
              (let ((misspelled (match-string-no-properties 1))
                    (count (string-to-number
                            (match-string-no-properties 2)))
                    (guess-list (split-string
                                 (match-string-no-properties 3) ", " t)))
                (push (cons misspelled
                            (folio-sublist
                             guess-list 0 count)) result)))))))
    result))

(defun folio-spellcheck-receive-data ()
  (let* ((buffer (current-buffer))
         (engine (folio-spellcheck-get buffer :engine))
         result)
    (condition-case err
        (cond
         ((eq engine 'aspell)
          (let ((process (folio-spellcheck-get buffer :process)))
            (when (folio-process-running-p process)
              ;; Intercept and wait for output to arrive in the
              ;; filter.  This call shouldn't hang in select (2), or
              ;; poll (2), or whatever is behind it; if it does
              ;; however, quitting should abort it and the response
              ;; data might get read with the next call to this defun
              ;; which should be okay in most cases.
              (with-local-quit
                (accept-process-output process)) ;; XXX 0 0 t))
              ;; Parse the response here rather than from within the
              ;; filter itself; this makes errors easier to observe.
              ;; Read-write access to the :received variable should be
              ;; naturally synchronized.
              (setq result
                    (folio-parse-aspell-suggestions
                     (folio-spellcheck-get buffer :received)))
              (folio-spellcheck-put buffer :received nil))))
         ((eq engine 'ns-spellchecker)
          (folio-spellcheck-put buffer :received nil))
         (t
          (error "Unsupported spell-checker engine")))
      (error
       (folio-spellcheck-delete buffer)
       (message "Failure parsing spell-checker data: %s" (cdr err))))
    result))

(provide 'folio-spellcheck)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-spellcheck.el ends here
