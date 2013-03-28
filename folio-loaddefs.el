;;

;;;### (autoloads (folio-guess-language folio-language-info folio-pop-directional-formatting
;;;;;;  folio-right-to-left-override folio-left-to-right-override
;;;;;;  folio-right-to-left-embedding folio-left-to-right-embedding
;;;;;;  folio-right-to-left-mark folio-left-to-right-mark folio-pluralize)
;;;;;;  "folio-babel" "folio-babel.el" (20815 34157 0 0))
;;; Generated autoloads from folio-babel.el

(autoload 'folio-pluralize "folio-babel" "\
Ultra trivial pluralization of English nouns.

\(fn WORD NUMBER)" nil nil)

(autoload 'folio-left-to-right-mark "folio-babel" "\
Insert an implicit left-to-right marker.

\(fn)" t nil)

(autoload 'folio-right-to-left-mark "folio-babel" "\
Insert an implicit right-to-left marker.

\(fn)" t nil)

(autoload 'folio-left-to-right-embedding "folio-babel" "\
Insert an explicit left-to-right marker U+202A.
The marker should be paired by U+202C as with
`folio-pop-directional-formatting'.

\(fn)" t nil)

(autoload 'folio-right-to-left-embedding "folio-babel" "\
Insert an implicit right-to-left embedding marker U+202B. The
marker should be paired by U+202C as with
`folio-pop-directional-formatting'.

\(fn)" t nil)

(autoload 'folio-left-to-right-override "folio-babel" "\
Insert an explicit left-to-right override marker U+202D.
The marker should be paired by U+202C as with
`folio-pop-directional-formatting'.

\(fn)" t nil)

(autoload 'folio-right-to-left-override "folio-babel" "\
Insert an explicit right-to-left override marker U+202E. The
marker should be paired by U+202C as with
`folio-pop-directional-formatting'.

\(fn)" t nil)

(autoload 'folio-pop-directional-formatting "folio-babel" "\
Pop the directional formatting stack.

\(fn)" t nil)

(defvar folio-directional-formatting-menu (let ((map (make-sparse-keymap "Insert BiDi control characters"))) (define-key map [folio-left-to-right-mark] '(menu-item "Insert `LRM'" folio-left-to-right-mark :keys "C-x a l")) (define-key map [folio-right-to-left-mark] '(menu-item "Insert `RLM'" folio-right-to-left-mark :keys "C-x a r")) map))

(define-key menu-bar-edit-menu [folio-directional-formatting-menu] `(menu-item "Insert BiDi control characters" ,folio-directional-formatting-menu))

(autoload 'folio-language-info "folio-babel" "\
Return the language info entry for the tag LANG.
KEY identifies the entry, SUB-KEY an optional sub-key.  For
possible values of KEY and SUB-KEY see
`folio-language-info-alist'.

\(fn LANG KEY &optional SUB-KEY)" nil nil)

(autoload 'folio-guess-language "folio-babel" "\
Guess language in region or buffer.

\(fn &optional BEG END)" t nil)

;;;***

;;;### (autoloads (folio-called-interactively-p folio-project folio-restore-hook
;;;;;;  folio-save-file-name folio-cycle-state with-folio-parent-buffer
;;;;;;  folio-set-parent-buffer folio-parent-buffer-track-kill) "folio-base"
;;;;;;  "folio-base.el" (20818 54731 0 0))
;;; Generated autoloads from folio-base.el

(defvar folio-parent-buffer nil "\
References the project text buffer actions apply to.")

(defvar folio-child-buffers nil "\
*List of child buffers to the project text buffer.")

(defsubst folio-mode-buffer-p nil "\
Return non-nil if the current buffer is a Folio mode buffer." (memq major-mode (quote (folio-mode folio-form-mode))))

(autoload 'folio-parent-buffer-track-kill "folio-base" "\
Hook function run just before actually killing the parent buffer.
In Folio mode, run through the list of child buffers and kill any
child still left alive.

\(fn)" nil nil)

(autoload 'folio-set-parent-buffer "folio-base" "\
Register the current buffer as a child buffer of BUFFER.
Child buffers are killed when the parent is killed.

\(fn BUFFER)" nil nil)

(autoload 'with-folio-parent-buffer "folio-base" "\
Execute the forms in BODY with `folio-parent-buffer' temporarily current.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'.

\(fn &rest BODY)" nil t)

(put 'with-folio-parent-buffer 'lisp-indent-function '0)

(autoload 'folio-cycle-state "folio-base" "\
Advance the cyclic state variable STATE by one.
STATE should be the symbol of a non-empty sequence.  If item is
non-nil assume its position in STATE for the current state.  KEYS
are additional keyword parameters.  Return a cons of the state
index positions of the old and the new state.

Keywords supported:  :test :scope

\(fn STATE [ITEM [KEYWORD VALUE]...])" nil nil)

(autoload 'folio-save-file-name "folio-base" "\
Find the buffer's save file name.
BUFFER is a valid buffer name or buffer object.

\(fn &optional BUFFER)" nil nil)

(autoload 'folio-restore-hook "folio-base" "\
Restore project state from .aux file.
Return t if a state file exists and was loaded successfully, or
nil otherwise.  This function should be called from the mode
hook.

\(fn)" nil nil)

(autoload 'folio-project "folio-base" "\


\(fn FORMAT-VERSION &optional ARGS)" nil nil)

(autoload 'folio-called-interactively-p "folio-base" "\


\(fn &optional KIND)" nil t)

;;;***

;;;### (autoloads (folio-project-auto-magic-p) "folio-core" "folio-core.el"
;;;;;;  (20818 54812 0 0))
;;; Generated autoloads from folio-core.el

(autoload 'folio-project-auto-magic-p "folio-core" "\
Auto mode magic predicate for folio-mode project files.
The predicate condition is satisfied if at the beginning of the
buffer file certain syntactic elements can be identifed.  The
look-ahead is restricted by the variable
`magic-mode-regexp-match-limit'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (folio-show-project-buffer folio-project-buffer-name)
;;;;;;  "folio-dialog" "folio-dialog.el" (20796 39829 0 0))
;;; Generated autoloads from folio-dialog.el

(autoload 'folio-project-buffer-name "folio-dialog" "\
Return the name of the project buffer.
PARENT is the text buffer.

\(fn PARENT)" nil nil)

(autoload 'folio-show-project-buffer "folio-dialog" "\
Display project buffer.

\(fn &optional PARENT)" t nil)

;;;***

;;;### (autoloads (folio-widget-integer-notify) "folio-dialog-pages"
;;;;;;  "folio-dialog-pages.el" (20796 39829 0 0))
;;; Generated autoloads from folio-dialog-pages.el

(autoload 'folio-widget-integer-notify "folio-dialog-pages" "\
XXX

\(fn WIDGET CHILD &optional EVENT)" nil nil)

;;;***

;;;### (autoloads (turn-off-folio-spellchecking turn-on-folio-spellchecking
;;;;;;  folio-spellcheck-mode folio-spellcheck-mode-lighter folio-spellcheck-make-mode-map
;;;;;;  folio-next-doublon folio-next-misspelling folio-correct-word
;;;;;;  folio-load-good-words folio-cycle-ellipsis folio-cycle-dash)
;;;;;;  "folio-etaoin-shrdlu" "folio-etaoin-shrdlu.el" (20819 12547
;;;;;;  0 0))
;;; Generated autoloads from folio-etaoin-shrdlu.el

(autoload 'folio-cycle-dash "folio-etaoin-shrdlu" "\
Replace a dash-like glyph at point with an alternative.
Alternatives include canonical dash glyphs as well as
equivalents made up from HYPHEN-MINUS.  Repeated
execution of this command at point switches to the next
alternative.  If called from Lisp return non-nil if a
replacement has happened.

\(fn)" t nil)

(autoload 'folio-cycle-ellipsis "folio-etaoin-shrdlu" "\
Replace an ellipsis-like glyph at point with an alternative.
Alternatives include canonical ellipsis glyphs as well as
equivalents made up from full stop characters.  Repeated
execution of this command at point switches to the next
alternative.  Retain leading and trailing whitespace but remove
inter-dot space.  If called from Lisp return non-nil if a
replacement has happened.

\(fn)" t nil)

(autoload 'folio-load-good-words "folio-etaoin-shrdlu" "\
Load the project's `good words' dictionary.

\(fn &optional NO-ERROR)" t nil)

(autoload 'folio-correct-word "folio-etaoin-shrdlu" "\
Interactively correct word at or before point.

\(fn)" t nil)

(autoload 'folio-next-misspelling "folio-etaoin-shrdlu" "\


\(fn &optional TYPE)" t nil)

(autoload 'folio-next-doublon "folio-etaoin-shrdlu" "\


\(fn)" t nil)

(defvar folio-spellcheck-mode-map)

(autoload 'folio-spellcheck-make-mode-map "folio-etaoin-shrdlu" "\


\(fn)" nil nil)

(defvar folio-spellcheck-mode-map (folio-spellcheck-make-mode-map) "\
Minor mode key-map for Folio spellcheck mode.")

(add-to-list 'minor-mode-map-alist `(folio-spellcheck-mode \, folio-spellcheck-mode-map) t)

(autoload 'folio-spellcheck-mode-lighter "folio-etaoin-shrdlu" "\
Render the lighter for Folio spellcheck mode.
Return a string appropriately propertized for display in the mode-line.

\(fn)" nil nil)

(autoload 'folio-spellcheck-mode "folio-etaoin-shrdlu" "\
Toggle Folio spellchecking.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Folio spellcheck mode is a buffer-local minor mode.  It is most
useful in combination with a Folio major mode but not generally
restricted to it.

\(fn &optional ARG)" t nil)

(custom-add-option 'folio-mode-hook 'folio-spellcheck-mode-enable)

(autoload 'turn-on-folio-spellchecking "folio-etaoin-shrdlu" "\
Unconditionally turn on Folio spellcheck mode.

\(fn)" t nil)

(autoload 'turn-off-folio-spellchecking "folio-etaoin-shrdlu" "\
Unconditionally turn off Folio spellcheck mode.

\(fn)" t nil)

;;;***

;;;### (autoloads (folio-form-previous-page folio-form-next-page
;;;;;;  folio-form-refresh folio-form-quit folio-form-goto) "folio-forms"
;;;;;;  "folio-forms.el" (20796 39829 0 0))
;;; Generated autoloads from folio-forms.el

(autoload 'folio-form-goto "folio-forms" "\


\(fn LINK)" t nil)

(autoload 'folio-form-quit "folio-forms" "\
Exit current project buffer.

\(fn)" t nil)

(autoload 'folio-form-refresh "folio-forms" "\


\(fn)" t nil)

(autoload 'folio-form-next-page "folio-forms" "\
Go to next page.

\(fn)" t nil)

(autoload 'folio-form-previous-page "folio-forms" "\
Go to previous page.

\(fn)" t nil)

;;;***

;;;### (autoloads (xxxfolio-guiguts-parse-pages folio-guiguts-update-sexp
;;;;;;  folio-guiguts-parse-sexp folio-guiguts-import-magic-p folio-guiguts-bin-file-name)
;;;;;;  "folio-guiguts" "folio-guiguts.el" (20818 55502 0 0))
;;; Generated autoloads from folio-guiguts.el

(autoload 'folio-guiguts-bin-file-name "folio-guiguts" "\
Find the name of a GuiGuts .bin file.
BUFFER is the project buffer or its name.

\(fn &optional BUFFER)" nil nil)

(autoload 'folio-guiguts-import-magic-p "folio-guiguts" "\
Test whether or not a GuiGuts .bin file should be imported.
BUFFER is the project buffer or its name; if nil use the current
buffer.  If IGNORE-MTIMES is non-nil ignore file modification
times.

Unless the customization variable `folio-guiguts-support' says
otherwise also register GuiGuts import and export
handlers.

Return the GuiGuts .bin file name on success, or nil otherwise.

\(fn &optional BUFFER IGNORE-MTIMES)" nil nil)

(autoload 'folio-guiguts-parse-sexp "folio-guiguts" "\
XXX docs

\(fn KEY)" nil nil)

(autoload 'folio-guiguts-update-sexp "folio-guiguts" "\
XXX docs

\(fn KEY STR &rest ARGS)" nil nil)

(autoload 'xxxfolio-guiguts-parse-pages "folio-guiguts" "\
Parse auxillary page information from a GuiGuts .bin file,
in particular the 'pagenumbers' hash map.

The result is a list of per page cons cells of the form

   (PAGE . (OFFSET LABEL STYLE ACTION BASE)),

starting with PAGE number 1.

This function is a detail of `folio-guiguts-import' (see which).

\(fn)" nil nil)

;;;***

;;;### (autoloads (folio-show-page-scan folio-image-mode folio-image-mwheel-scroll)
;;;;;;  "folio-image" "folio-image.el" (20796 39829 0 0))
;;; Generated autoloads from folio-image.el

(autoload 'folio-image-mwheel-scroll "folio-image" "\
Handle mouse wheel events according to EVENT.

\(fn EVENT)" t nil)

(autoload 'folio-image-mode "folio-image" "\
A major mode for the display of page scan images.

\(fn)" t nil)

(autoload 'folio-show-page-scan "folio-image" "\


\(fn FILE-NAME)" nil nil)

;;;***

;;;### (autoloads (folio-log-buffer-name) "folio-log" "folio-log.el"
;;;;;;  (20796 39829 0 0))
;;; Generated autoloads from folio-log.el

(autoload 'folio-log-buffer-name "folio-log" "\
Return the name of the log buffer.
PARENT is the text buffer.

\(fn PARENT)" nil nil)

;;;***

;;;### (autoloads (folio-mode folio-mode-auto-detected-p turn-off-folio-auto-fill-mode
;;;;;;  folio-flyspell-mode-disable folio-get-version) "folio-mode"
;;;;;;  "folio-mode.el" (20818 63227 0 0))
;;; Generated autoloads from folio-mode.el

(autoload 'folio-get-version "folio-mode" "\
Return the Folio mode version.
If called interactively echo the current version in the
minibuffer.  Otherwise return the version as a string.

\(fn)" t nil)

(autoload 'folio-flyspell-mode-disable "folio-mode" "\
Turn off Flyspell mode.
This function is meant to be run from `folio-mode-hook'.  Folio
mode maintains a spellchecking subsystem of its own.

\(fn)" nil nil)

(autoload 'turn-off-folio-auto-fill-mode "folio-mode" "\
Turn off Auto Fill mode.
This function is meant to be run from `folio-mode-hook'.
Auto-filling by default is disabled when Folio mode is enabled
since auto-filling (rewrapping) is likely to produce unwanted
mismatches between book text and scan.  Filling instead should be
done explicitly.

\(fn)" nil nil)

(custom-add-option 'folio-mode-hook 'folio-auto-fill-mode-disable)

(autoload 'folio-mode-auto-detected-p "folio-mode" "\
Return t if the current buffer contains an auto-detectable text file.
This function is intended to be used from `magic-mode-alist' like
with (add-to-list magic-mode-alist (cons folio-mode-auto-detected-p folio-mode))
to enable auto-detection of folio-mode for all supported file formats.

\(fn)" nil nil)

(add-to-list 'magic-mode-alist '(folio-mode-auto-detected-p . folio-mode))

(autoload 'folio-mode "folio-mode" "\
XXX

The following commands are available:

\\{folio-mode-map}

\(fn)" t nil)

;;;***

;;;### (autoloads (folio-smarten-quotes) "folio-smartquotes" "folio-smartquotes.el"
;;;;;;  (20813 59402 0 0))
;;; Generated autoloads from folio-smartquotes.el

(autoload 'folio-smarten-quotes "folio-smartquotes" "\
Turn straight quotation marks into smart quotes in region or
buffer.

\(fn &optional STYLE)" t nil)

;;;***

;;;### (autoloads (folio-speech-festival-start) "folio-speech" "folio-speech.el"
;;;;;;  (20796 39829 0 0))
;;; Generated autoloads from folio-speech.el

(autoload 'folio-speech-festival-start "folio-speech" "\
Start a TTS process.
If a process is already running, this is a no-op.

\(fn)" t nil)

;;;***

;;;### (autoloads (folio-change-dictionary) "folio-spellcheck" "folio-spellcheck.el"
;;;;;;  (20819 11941 0 0))
;;; Generated autoloads from folio-spellcheck.el

(autoload 'folio-change-dictionary "folio-spellcheck" "\
Change PRIMARY or SECONDARY dictionaries for spellchecking.
The dictionary always is only set \"locally\", just for the
current buffer.

If called interactively read the new primary dictionary name from
the minibuffer prompt.  Display the current primary dictionary if
the prompt is answered with just RET.

If called from Lisp set primary and secondary dictionaries to the
values of PRIMARY and SECONDARY, respectively.  Setting only the
primary dictionary resets all secondary dictionaries.

Run the normal hook `folio-dictionary-change-hook' if any
dictionary has changed.

\(fn PRIMARY &optional SECONDARY)" t nil)

;;;***

;;;### (autoloads (folio-text-auto-magic-p) "folio-text" "folio-text.el"
;;;;;;  (20813 59965 0 0))
;;; Generated autoloads from folio-text.el

(autoload 'folio-text-auto-magic-p "folio-text" "\
Auto mode magic predicate for text files.
The predicate condition is satisfied if at the beginning of the
file certain syntactic elements can be identifed.  The look-ahead
is restricted by the variable `magic-mode-regexp-match-lime'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (folio-find-things folio-style-thing-at-point folio-kill-thing-at-point
;;;;;;  folio-transpose-thing-at-point folio-transform-thing-at-point
;;;;;;  folio-canonically-space-thing-at-point folio-unfill-thing-at-point
;;;;;;  folio-fill-thing-at-point folio-indent-thing-at-point folio-pad-thing-at-point
;;;;;;  folio-unfold-thing-at-point folio-mark-thing-at-point folio-region-or-thing
;;;;;;  folio-point-inside-pairs-p folio-check-parens folio-looking-at-thing-p
;;;;;;  folio-thing-at-point-p) "folio-things" "folio-things.el"
;;;;;;  (20819 7215 0 0))
;;; Generated autoloads from folio-things.el

(autoload 'folio-thing-at-point-p "folio-things" "\
Predicate returning t if THING is defined as a `thing-at-point'
type, or nil otherwise.

\(fn THING)" nil nil)

(autoload 'folio-looking-at-thing-p "folio-things" "\
Return THING if text after point matches a `thing-at-point' of
type THING.  Otherwise return nil.  THING normally is a symbol
defining a suitable forward movement function.  It also can be a
list of THING symbols in which case tests are performed in order.
Point must be positioned at the beginning of THING for the test
to succeed.  With non-nil prefix argument ARG look backward
instead.

\(fn THING &optional ARG)" nil nil)

(defsubst folio-looking-back-at-thing-p (thing &optional arg) "\
Return THING if text before point matches a `thing-at-point'
of type THING, or nil otherwise.  THING normally is a symbol
defining a suitable forward movement function.  It also can be a
list of THING symbols in which case tests are performed in order.
Point must be positioned at the end of THING for the test to
succeed.  With non-nil prefix argument ARG look forward instead." (folio-looking-at-thing-p thing (not arg)))

(autoload 'folio-check-parens "folio-things" "\


\(fn &optional ARG)" t nil)

(autoload 'folio-point-inside-pairs-p "folio-things" "\
Return t if point is inside any pairs of the parenthesis syntax
class, or nil otherwise.  This function does not consider unbalanced
parentheses.

\(fn &optional PPSS)" nil nil)

(autoload 'folio-region-or-thing "folio-things" "\
Return THING or region at point.
The result is a consp of the form (TEXT (START . END)) containing
the region and its bounds if there is a region active and it's
appropriate to act on it, or THING in the same form.  TEXT is
returned with properties.

\(fn THING)" nil nil)

(autoload 'folio-mark-thing-at-point "folio-things" "\
Set mark ARG things away from point.
The place mark goes is the same place `forward-thing' would
move to with the same argument.  THING is a symbol identifying a
`thing-at-point' including 'paragraph, 'line, '`folio-footnote',
'`folio-sidenote', etc.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it extends the selection to include
the next ARG THINGs.

\(fn THING &optional ARG ALLOW-EXTEND)" nil nil)

(autoload 'folio-unfold-thing-at-point "folio-things" "\
Unfold a thing by deleting new-line characters.
THING either is a symbol of a `thing-at-point' or a cons of the
left and right boundaries of THING.  The text stretch of THING is
assumed to be normalized to contain LF characters only (not CRs).
The replacement character is SP.  Successive occurences of LF
characters will be replaced by single SP character.

The return value is a cons describing the boundaries of THING.

\(fn THING)" t nil)

(autoload 'folio-pad-thing-at-point "folio-things" "\
Apply left or right padding to THING.
The first argument THING either is a symbol of a `thing-at-point'
or a cons cell describing the boundaries of THING.  For left
padding the second argument LPAD must be a string or a character;
correspondingly the optional third argument RPAD is used for
right padding.  Multiple occurrences of the first character of
LPAD before THING automatically are collapsed into a single
instance of this character; with right padding the last character
of RPAD is collapsed.

Note that the paddings do not inherit any properties from THING,
i.e. padding text is not considered part of the thing.

The return value is a cons of start and end positions of THING
\(excluding any padding).

\(fn THING LPAD &optional RPAD)" t nil)

(autoload 'folio-indent-thing-at-point "folio-things" "\
XXX

\(fn THING INDENT &optional HANG)" t nil)

(autoload 'folio-fill-thing-at-point "folio-things" "\


\(fn THING &optional JUSTIFY HANG)" t nil)

(autoload 'folio-unfill-thing-at-point "folio-things" "\


\(fn THING)" t nil)

(autoload 'folio-canonically-space-thing-at-point "folio-things" "\
Remove extra spaces between words.
In the region occupied by THING leave one space between words,
two at end of sentences or after colons (depending on values of
`sentence-end-double-space', `colon-double-space', and
`sentence-end-without-period').  Remove indentation from each
line.

\(fn THING)" t nil)

(autoload 'folio-transform-thing-at-point "folio-things" "\
Transform THING using REGEXP search and replace with TO-STRING.
THING either is a symbol identifying a `thing-at-point' or a cons
cell with the buffer start and end positions (inclusive) of
THING.  If the optional third argument REPEAT is non-nil search
and replace all occurences within the boundaries of THING, or
only the first occurence.  Search case-sensitivity is determined
by the value of the variable `case-fold-search', which see.  If
the fourth optional argument FIXED-CASE is non-nil, retain case
of replacement text.  Otherwise maybe capitalize the whole text,
or maybe just word initials, based on the replaced text.  If the
fifth optional argument is non-nil TO-STRING is inserted
literally.  Otherwise treat `' as special:
  `&' in NEWTEXT means substitute original matched text.
  `N' means substitute what matched the Nth `(...)'.
       If Nth parens didn't match, substitute nothing.
  `\\' means insert one `'.
Case conversion does not apply to these substitutions.

\(fn THING REGEXP TO-STRING &optional REPEAT FIXED-CASE LITERAL)" t nil)

(autoload 'folio-transpose-thing-at-point "folio-things" "\
Transpose two things.
With the `thing-at-point' THING and OTHER move THING to follow OTHER
and return the boundaries of THING in the new location on success, or
nil if OTHER is not found.  OTHER can also be a marker or a consp
defining a buffer region, or a buffer position such as `mark'.  By
repeatedly calling this function with the return value of a previous
call, a sequence of THINGs can easily be reordered or moved to a new
page location further up or down.

Except for point, marker or consp types of OTHER, the optional third
argument NTH can be used to select the nth occurence of OTHER instead
of the next following point.  For NTH > 0 the search for OTHER is
forward, or backward (NTH < 0) otherwise.

If the optional fourth argument PRECEDE is non-nil THING is moved
to directly precede (the NTH occurence of) OTHER.  This is a
legal operation even if THING already precedes OTHER (only
further up), i.e. effectively a double transposition.

THING may be completely contained by OTHER, any other intersection of
the two things including a shared boundary will be rejected by raising
an error, though.

If THING is moved its text properties are preserved.

Note that THING if moved will always directly precede or follow
OTHER; if any kind of padding is required to separate the two things,
`folio-pad-thing-at-point' can be used, or a simple `insert' (see which)
at either of the new boundaries.

\(fn THING OTHER &optional NTH PRECEDE)" nil nil)

(autoload 'folio-kill-thing-at-point "folio-things" "\
Kill THING at point adding it to the kill-ring.
THING must be a symbol defining a `thing-at-point' such as
`folio-proofer-note'.

\(fn THING &optional VERB)" nil nil)

(autoload 'folio-style-thing-at-point "folio-things" "\
Apply transformations to a `thing-at-point' as defined by a
given style.

\(fn &optional THING STYLE)" t nil)

(autoload 'folio-find-things "folio-things" "\
Return a list of `thing-at-point' THINGs.
The symbol THING identifies the type of things to search.  Search
starts at point and always is forward.

The result is a list of conses of the buffer start and end
positions of a THING sorted ascending.

\(fn THING)" nil nil)

;;;***

;;;### (autoloads (update-autoloads-for-file-in-package-area update-autoloads-in-package-area)
;;;;;;  "update-auto-loads" "update-auto-loads.el" (20818 62434 0
;;;;;;  0))
;;; Generated autoloads from update-auto-loads.el

(when load-file-name (setq load-path (cons (file-name-directory load-file-name) load-path)))

(autoload 'update-autoloads-in-package-area "update-auto-loads" "\
Update autoloads for files in the directory containing this
file or FILE.

\(fn &optional FILE)" t nil)

(autoload 'update-autoloads-for-file-in-package-area "update-auto-loads" "\


\(fn FILE)" t nil)

;;;***

;;;### (autoloads nil nil ("folio-dialog-scanno.el" "folio-dialog-setup.el"
;;;;;;  "folio-dialog-word-frequency.el" "folio-faces.el" "folio-font-lock.el"
;;;;;;  "folio-frame.el" "folio-menu.el" "folio-more-like-this.el"
;;;;;;  "folio-outline.el" "folio-roman.el" "folio-scanno.el" "folio-splash.el"
;;;;;;  "folio-time.el" "folio-tip-of-the-day.el" "folio-widget.el"
;;;;;;  "future-folio-ruby.el" "future-folio.el") (20819 12595 942757
;;;;;;  0))

;;;***

;;;### (autoloads (folio-with-muted-message folio-copy-other-window
;;;;;;  folio-lower-case-char-at-p folio-chomp folio-binary-search-internal
;;;;;;  folio-binary-search folio-upper-bound folio-current-line
;;;;;;  folio-map-range folio-flatten folio-truncate-list folio-assq-pop
;;;;;;  folio-assoc-pop folio-sublist folio-shuffle-list) "folio-atoms"
;;;;;;  "folio-atoms.el" (20815 32916 0 0))
;;; Generated autoloads from folio-atoms.el

(defsubst folio-filter-list (list pred) "\
Filter the elements of LIST according to the predicate PRED." (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) list)))

(autoload 'folio-shuffle-list "folio-atoms" "\
Randomly permute the elements of LIST.
All permutations are equally likely.

\(fn LIST)" nil nil)

(autoload 'folio-sublist "folio-atoms" "\
Return a sublist of LIST.
Index START counts from 0, END is exclusive.  If END is omitted,
it defaults to the length of LIST.  Otherwise END should be
greater than or equal to START.

This function can operate destructively; write
`(setq l (folio-sublist from))' to be sure of correctly changing the
value of a list `l'.

\(fn LIST START &optional END)" nil nil)

(autoload 'folio-assoc-pop "folio-atoms" "\
From ALIST pop element with key KEY.
Return the first element of ALIST whose car is `equal' to KEY, and
delete it from the list.  For a non-existant KEY nil is returned.

\(fn KEY ALIST)" nil t)

(autoload 'folio-assq-pop "folio-atoms" "\
From ALIST pop element with key KEY.
Return the first element of ALIST whose car is `eq' to KEY, and
delete it from the list.  For a non-existant KEY nil is returned.

\(fn KEY ALIST)" nil t)

(autoload 'folio-truncate-list "folio-atoms" "\
Truncate LIST to at most N elements destructively.
The argument is modified directly; the return value is undefined.

\(fn LIST N)" nil nil)

(autoload 'folio-flatten "folio-atoms" "\
Recursively flatten ARG.
Return a list with the atoms in ARG at any level.  ARG need not
be a list, but also can be a cons cell with non-nil cdr, or a
primitive type, both then also coerced into a flat list.

\(fn ARG)" nil nil)

(autoload 'folio-map-range "folio-atoms" "\
Linearly map the number F from one into another range.
The interval [RA1, RA2] is the source range, [RB1, RB2] the
target range.  RA1, RA2, RB1, RB2 and F should be floating point
numbers.

\(fn RA1 RA2 RB1 RB2 F)" nil nil)

(autoload 'folio-current-line "folio-atoms" "\
Return current line number of point.
For absolute line numbers, narrowing must not be in effect.

\(fn)" nil nil)

(defsubst folio-goto-line (line) "\
Goto line LINE moving point.
Return the number of lines left to move." (goto-char (point-min)) (forward-line (1- line)))

(autoload 'folio-upper-bound "folio-atoms" "\


\(fn VALUE ORDERED-SEQ &key (value-extract (function identity)))" nil nil)

(autoload 'folio-binary-search "folio-atoms" "\
Perform a binary search in a sequence.

\(fn VALUE ORDERED-SEQ &key (value-extract (function identity)) (lower-test (function <=)) (upper-test (function <=)))" nil nil)

(autoload 'folio-binary-search-internal "folio-atoms" "\


\(fn VALUE ORDERED-SEQ START END VALUE-EXTRACT LOWER-TEST UPPER-TEST)" nil nil)

(defsubst folio-starts-with-p (s beginning) "\
Return t if string S starts with BEGINNING.
S or BEGINNING also can be symbols in which case their print
names are compared.  Text properties are ignored.  See also
`folio-ends-with-p'." (cond ((>= (length s) (length beginning)) (string-equal (substring s 0 (length beginning)) beginning)) (t nil)))

(defsubst folio-ends-with-p (s ending) "\
Return t if string S ends with ENDING.
S or ENDING also can be symbols in which case their print names
are compared.  Text properties are ignored.  See also
`folio-starts-with-p'" (string-equal (substring s (- (length ending))) ending))

(autoload 'folio-chomp "folio-atoms" "\
Chomp leading and tailing whitespace from STR.
Match data is modified.

\(fn STR)" nil nil)

(defsubst folio-case-fold-string-equal (str1 str2) "\
Compare the contents of two strings STR1 and STR2 ignoring case.
Convert to multibyte if needed.  Return t if strings are equal,
or nil otherwise." (eq t (compare-strings str1 nil nil str2 nil nil (quote ignore-case))))

(defalias 'folio-case-fold-string= 'folio-case-fold-string-equal "\
Alternative function name for `folio-case-fold-string-equal'.")

(defsubst folio-case-fold-string-hash (str) "\
Return a hash code for case folded string STR." (sxhash (upcase str)))

(define-hash-table-test 'folio-case-fold-hash-table-test 'folio-case-fold-string-equal 'folio-case-fold-string-hash)

(autoload 'folio-lower-case-char-at-p "folio-atoms" "\
Return t if the character after POS is in lowercase.
If POS is nil check the character at point instead.

\(fn &optional POS)" nil nil)

(defsubst folio-upper-case-char-at-p (&optional pos) "\
Return t if the character after POS is in uppercase.
If POS is nil check the character at point instead." (not (folio-lower-case-char-at-p (or pos (point)))))

(autoload 'folio-copy-other-window "folio-atoms" "\
Copy region text or word to buffer in other window.
Text properties are not retained.

\(fn BEG END)" t nil)

(autoload 'folio-with-muted-message "folio-atoms" "\
Redefine `message' to be silent.
Eval BODY forms sequentially and return value of last one.  Upon
return restore the normal behaviour of `message'.

\(fn &rest BODY)" nil t)

;;;***
