;;; folio-base.el --- Folio mode base functionality

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

;; This package provides base functionality for various Folio major
;; modes including saving and restoring auxiliary project data,
;; parent-child buffer management, maintenance of buffer data, and
;; asynchronous buffer activities using idle timers.

;;; Code:

(require 'folio-atoms)

(defgroup folio nil
  "Support for producing e-books at Distributed Proofreaders."
  :tag "Folio"
  :group 'wp
  :version "24.1"
  :link '(url-link "https://github.com/em-space/folio-mode"))

(defgroup folio-external nil
  "Settings for programs external to Emacs."
  :tag "Folio Programs"
  :group 'folio
  :version "24.1")

(defgroup folio-technical nil
  "Technical settings for tweaking stuff."
  :tag "Folio Hackers"
  :group 'folio
  :version "24.1")

(defcustom folio-pgdp-projects-url "http://www.pgdp.net/projects/"
  "HTTP base URL for PGDP projects.
This URL for instance is used to fetch the DC document for an
e-text from."
  :tag "Folio base URL for PGDP projects"
  :group 'folio-technical
  :type 'string)

(defcustom folio-inverted-scrolling nil
  "If non-nil revert the vertical scrolling direction such that
scrolling up makes contents scroll down and vice versa.  This
option might be convenient to Mac OS X users with a touch pad;
users of a mouse wheel might want to leave this option
off (nil)."
  :tag "Folio Whether to invert the scrolling direction"
  :type 'boolean
  :group 'folio)

(defconst folio-save-version "1.0"
  "Format version for the project save file.")

(defvar folio-save-file-mtime nil
  "Timestamp of last modification of the save file.")
(make-local-variable 'folio-save-file-mtime)

(defvar folio-save-file-checksum nil
  "Checksum used by `folio-save-hook' to avoid needless saves.")
(make-local-variable 'folio-save-file-checksum)

(defvar folio-save-list nil
  "List of project state variables to save.
The elements are symbols.

If the list is to be maintained ordered `add-to-ordered-list'
should be used instead of `add-to-list'.")
(make-local-variable 'folio-save-list)

(defvar folio-before-save-hook nil
  "Hook run before project state is saved.
This is useful for truncating history lists, for example.")

(defvar folio-after-restore-hook nil
  "Hook run after project state has been restored.")

(defvar folio-restore-fallback-functions nil
  "Hook run when no project data appears to be available.
This hook is meant for importing meta data like page locations or
project state from other data sources or in formats not directly
supported.  `folio-restore-fallback-functions' is an abnormal hook,
i.e. for this hook the hook function must return t on success, or
nil otherwise.  No arguments are passed.")

(defvar folio-project-id nil
  "PGDP identifier of the current project.
This normally is of the form projectID4f95b7aa16477.")
(make-variable-buffer-local 'folio-project-id)

(defvar folio-interrupted-activity-list nil
  "*List of interrupted activities.
An activity normally is a potentially long running background
process driven by an idle timer.  This state is switched by
`folio-activity-interrupted-p'.")

(defvar folio-activity-alist nil
  "Alist of activities.")

;;;###autoload
(defvar folio-parent-buffer nil
  "References the project text buffer actions apply to.")
(make-variable-buffer-local 'folio-parent-buffer)

;;;###autoload
(defvar folio-child-buffers nil
  "*List of child buffers to the project text buffer.")
(make-variable-buffer-local 'folio-child-buffers)

;;;###autoload
(defun folio-mode-p (mode)
  "Return non-nil if MODE is a Folio major mode.
Note that by the definition of the function only major modes like
folio-text-mode or folio-xhtml-mode are considered Folio modes,
not auxillary modes like folio-image-mode, or folio-form-mode."
  (let ((major-mode mode))
    (derived-mode-p 'folio-text-mode 'folio-xhtml-mode)))

;;;###autoload
(defun folio-mode-buffer-p (&optional buffer)
  "Return non-nil if the current buffer is a Folio major mode buffer.
If BUFFER is non-nil query that buffer instead.  For definition
of a Folio major mode, see `folio-mode-p'."
  (with-current-buffer (or buffer (current-buffer))
    (folio-mode-p major-mode)))

;;;###autoload
(defun folio-parent-buffer-track-kill ()
  "Hook function run just before actually killing the parent buffer.
In Folio mode, run through the list of child buffers and kill any
child still left alive."
  (when (folio-mode-buffer-p)
    (mapc (lambda (x)
            (when (buffer-live-p x)
              (kill-buffer x))) folio-child-buffers)
    (setq folio-child-buffers nil)))

;;;###autoload
(defun folio-set-parent-buffer (buffer)
  "Register the current buffer as a child buffer of BUFFER.
Child buffers are killed when the parent is killed."
  (let ((child (current-buffer)))
    (assert (not (eq buffer child)))
    (setq folio-parent-buffer buffer)
    (with-current-buffer buffer
      (when (= (length (setq folio-child-buffers
                             (cons child folio-child-buffers))) 1)
        (add-hook 'kill-buffer-hook 'folio-parent-buffer-track-kill t)))))

;;;###autoload
(defmacro folio-with-parent-buffer (&rest body)
  "Execute the forms in BODY with `folio-parent-buffer' temporarily current.
The value returned is the value of the last form in BODY.  See
also `with-current-buffer'."
  (declare (indent 0) (debug t))
  `(save-current-buffer
     (set-buffer folio-parent-buffer)
     ,@body))

(defun folio-buffer-list (&optional include-text-mode-variants)
  "Return a list of buffers in Folio mode.
If INCLUDE-TEXT-MODE-VARIANTS is non-nil also include buffers
where the major mode is a variant of text-mode."
  (let (buffers)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (or (folio-mode-buffer-p)
                  (and include-text-mode-variants text-mode-variant))
          (push buffer buffers))))
    buffers))

(defun folio-preferred-process-buffer (buffer-list)
  "Find a buffer for asynchroneous batch processing.
BUFFER-LIST specifies possible candidates like from
`buffer-list', `folio-buffer-list', etc.  The buffer is selected
such that the buffer from the selected window is preferred over
buffers from the active window list which in turn are preferred
over buffers associated with internal windows.  With multiple
candidates for each class a random permutation is applied."
  (let ((buffers (folio-filter-list buffer-list #'buffer-live-p)))
    (or (car (memq (window-buffer (selected-window)) buffers))
        (car (folio-filter-list
              (folio-shuffle-list (folio-filter-list
                                   (window-list) #'window-live-p))
              (lambda (x)
                (memq x buffers))))
        (car (folio-shuffle-list buffers)))))

(defun folio-completing-read-buffer (&optional buffer-list)
  "Read a buffer name from mini-buffer prompt.
BUFFER-LIST should be a list of valid candidates.  If nil the
buffer list produced by `folio-buffer-list' is used."
  (let ((completion-ignore-case t)
        (candidates (or buffer-list (folio-buffer-list))))
    (completing-read
     "Buffer: " (mapcar 'buffer-name candidates) nil t
     (buffer-name (unless (member (buffer-name) candidates)
                    (car candidates))))))


(defun folio-buffer-data-put (list buffer key value)
  "In LIST for BUFFER set KEY's VALUE.
The key :buffer is reserved for internal purposes."
  (when (eq key :buffer)
    (error "Invalid key"))
  (if (null (symbol-value list))
      (set list `((:buffer ,buffer ,key ,value)))
    (catch 'done
      (mapc (lambda (x)
              (when (equal (plist-get x :buffer) buffer)
                (plist-put x key value)
                (throw 'done t))) (symbol-value list))
      (set list (cons `(:buffer ,buffer ,key ,value)
                      (symbol-value list)) )))
  (symbol-value list))

(defun folio-buffer-data-delete (list buffer)
  "In LIST delete all data associated to BUFFER."
  (set list
       (delq nil (mapcar (lambda (x)
                           (unless (eq (plist-get x :buffer)
                                       buffer)
                             x)) (symbol-value list)))))

(defun folio-buffer-data-get-specific
  (list key value &optional target-key)
  "In LIST query the first matching KEY VALUE pair.
If TARGET-KEY is nil return all associated data.  Otherwise
return only the element's value which key is TARGET-KEY.  LIST,
KEY and TARGET-KEY should be symbols."
  (catch 'result
    (mapc (lambda (x)
            (when (equal (plist-get x key) value)
              (throw 'result (if target-key
                                 (plist-get x target-key)
                               x)))) (symbol-value list))
    nil))

(defsubst folio-buffer-data-get
  (list buffer &optional target-key)
  "From LIST retrieve all data associated to BUFFER.
If TARGET-KEY is non-nil return only the element's value which
key is TARGET-KEY.  LIST and TARGET-KEY should be symbols."
  (folio-buffer-data-get-specific
   list :buffer buffer target-key))

(defun folio-buffer-data-get-all (list &optional key)
  "From LIST return every buffer's value for KEY.
If KEY is nil return every buffer's data."
  (if key
      (delq nil (mapcar (lambda (x)
                          (plist-get x key))
                        (symbol-value list)))
    (symbol-value list)))

(defun folio-buffer-data-get-all-but (list key except)
  "Return buffer data from LIST for KEY but filter by EXCEPT.
This function is like `folio-buffer-data-get-all' except that an
item is excluded from the result if its value for the secondary
key EXCEPT is non-nil."
  (delq nil (mapcar (lambda (x)
                      (let ((v (plist-get x key)))
                        (if (plist-get x except) nil v)))
                    (symbol-value list))))


(defun folio-activity-interrupted-p (activity &optional reset)
  "Return non-nil when input has arrived while processing ACTIVITY.
ACTIVITY should be a unique symbol identifying a particular
activity.  If RESET is non-nil reset the interrupted state
unconditionally.  Otherwise update the activity status if input
has arrived.  This function is not idempotent since the activity
state is reset when read."
  (or (when (or reset
                (memq activity folio-interrupted-activity-list))
        (setq folio-interrupted-activity-list
              (delq activity folio-interrupted-activity-list)) t)
      (when (or (input-pending-p)
                (active-minibuffer-window)
                executing-kbd-macro
                defining-kbd-macro)
        (setq folio-interrupted-activity-list
              (cons activity folio-interrupted-activity-list))
        t)))

(defsubst folio-activity-buffer-list (activity)
  (assq activity folio-activity-alist))

(defun folio-activity-p (activity &optional buffer-or-name)
  "Return non-nil if there is ACTIVITY running or scheduled.
BUFFER-OR-NAME is the buffer object or buffer name to check.  If
omitted only the existence of an active process is checked."
  (and (folio-timer-running-p activity)
       (or (null buffer-or-name)
           (memq (get-buffer buffer-or-name)
                 (folio-activity-buffer-list activity)))))

(defsubst folio-next-activity-buffer (activity)
  (folio-preferred-process-buffer
   (folio-activity-buffer-list activity)))

(defun folio-schedule-activity (activity buffer &optional secs)
  (cond
   ((or (null folio-activity-alist)
        (null (assq activity folio-activity-alist)))
    (push (cons activity buffer) folio-activity-alist))
   ((null (memq buffer (assq activity folio-activity-alist)))
    (push buffer (cdr (assq activity folio-activity-alist)))))
  (unless (folio-timer-running-p activity)
    (folio-schedule-timer activity secs)))

(defun folio-cancel-activity (activity &optional buffer)
  "Cancel the activity ACTIVITY."
  (let ((buffers (delq buffer
                       (assq activity folio-activity-alist))))
    (if buffers
        (progn
          (setcdr (assq activity folio-activity-alist) buffers)
          (assq activity folio-activity-alist))

      (setq folio-activity-alist
            (delq activity folio-activity-alist))
      (setq folio-interrupted-activity-list
            (delq activity folio-interrupted-activity-list))
      (folio-cancel-timer activity)
      nil)))

(defun folio-cancel-activities ()
  (mapc (lambda (x)
          (folio-cancel-timer (car x))) folio-activity-alist)
  (setq folio-activity-alist nil
        folio-interrupted-activity-list nil))


(defun folio-find-project-id ()
  "Determine a PGDP project identifier.
This function is a detail of `folio-assign-project-id', see
which."
  (let (cand)
    (mapc (lambda (x)
            (let* ((case-fold-search nil)
                   (id (when (string-match
                              "\\(projectID[a-f0-9]+\\)" x)
                         (match-string 1 x))))
              (unless (member id cand)
                (push id cand))))
          (directory-files default-directory nil
                           "projectID[a-f0-9]+"))
    cand))

(defun folio-assign-project-id (&optional id)
  "Assign a unique PGDP project identifier.
If the optional argument ID is omitted when called interactively,
prompt the user to enter an ID in the minibuffer, or assign nil
otherwise.  For the interactive usage, completion is supported,
and a selection of identifiers to choose from as determined from
the current directory contents.  Project identifiers are of the
form \"projectID\" followed by a combination of lower-case
letters and digits, a hexadecimal number."
  (interactive
   (let ((hist (folio-find-project-id)))
     (list (completing-read "Project ID: " hist nil 'confirm (car hist)))))
  (let ((id (car-safe (or (and id (list id)) (folio-find-project-id))))
        (old-id folio-project-id))
    (unless (string-equal id old-id)
      (setq folio-project-id id)
      (set-buffer-modified-p 'modified))
    (when (called-interactively-p 'any)
      (message (if (string-equal id old-id)
                   "Project ID unchanged from %s"
                 "Project ID assigned to %s") (or folio-project-id "<none>")))
    folio-project-id))

(defun folio-default-save-list (buffer)
  (with-current-buffer buffer
    (list folio-save-version
          (list
           folio-project-id))))

;; XXX TODO
(defun folio-default-xsave-list (buffer)
  (with-current-buffer buffer
    (list folio-save-version
          (list
           folio-project-id
           (let (minor-modes)
             (mapc
              #'(lambda (minor-mode)
                  (and (boundp minor-mode)
                       (symbol-value minor-mode)
                       (let ((value (when (functionp minor-mode) minor-mode)))
                         (when value (add-to-list 'minor-modes value)))))
              (mapcar #'car minor-mode-alist))
             minor-modes)
           (point)
           (list (mark t) mark-active)
           buffer-read-only))))

(defmacro folio-restore-variable (var value)
  "Set a variable's value when loading project data.
VAR must be a symbol, and VALUE its value appropriately quoted.
VAR is automatically registered with the save list
`folio-save-list'."
  `(prog1
     (setq ,var ,value)
     (add-to-list 'folio-save-list ',var)))

(defmacro folio-restore-deferred (setter value)
  "Restore the variable value VALUE deferred.
SETTER should be a function symbol for setting the variable's
value."
  `(add-hook 'folio-restore-deferred-hook (lambda ()
                                            (,setter ,value)) nil t))

;;;###autoload
(defun folio-save-file-name (&optional buffer)
  "Find the buffer's save file name.
BUFFER is a valid buffer name or buffer object."
  (let ((file-name (buffer-file-name
                    (get-buffer (or buffer (current-buffer))))))
    (when file-name
      (concat file-name ".aux"))))

(defun folio-save-variable (varspec)
  "Output a statement for variable VAR to the project file.
The argument VARSPEC may be the variable name VAR (a symbol), or
a cons cell of the form (VAR . MAX-SIZE), which means to truncate
VAR's value to at most MAX-SIZE elements if the value is a
list before saving the value.

If VAR has the special function property `folio-save-value'
assigned, this function's return value is saved; correspondingly,
if the function property `folio-restore-value' is assigned to a
function, the value of `folio-restore-value' is called upon restore
with the saved value for the sole input parameter."
  (let (var
        size)
    (if (consp varspec)
        (setq var (car varspec)
              size (cdr varspec))
      (setq var varspec))
    (when (boundp var)
      (let* ((getter (get var 'folio-save-value))
            (setter (get var 'folio-restore-value))
            (value (if (and getter (symbolp getter))
                       (funcall getter)
                     (symbol-value var))))
        (when (and (integerp size)
                   (> size 0)
                   (listp value))
          (folio-truncate-list value size))
        (concat (if setter
                    (prin1-to-string
                     `(folio-restore-deferred ,setter ',value))
                  (prin1-to-string
                   `(folio-restore-variable
                     ,var ',value))) "\n")))))

(defvar folio-restore-deferred-hook nil
 "Hook functions that are run deferred after all buffers are loaded.
This variable is intended for internal use.")

(defun folio-save-hook ()
  "Save auxiliary project data."
  (let* ((buffer (current-buffer))
         (save-file (folio-save-file-name buffer))
         (last-checksum folio-save-file-checksum))
    ;; XXX check mtime
    ;; XXX save hash document hash
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp; coding: emacs-mule; -*-\n"
       ";;\n;; Emacs folio-mode Project File\n"
       ";;\n;; =======================================================\n"
       ";; NOT FOR EDITING. YOU'LL RISK LOOSING ALL PROJECT STATE.\n"
       ";; =======================================================\n"
       ";;\n"
       ";; Created (current-time-string)\n"
       ";; File format version " folio-save-version "\n"
       ";; Emacs version " emacs-version "\n"
       ";;\n"
       ";;; Save hooks:\n\n")
      (with-current-buffer buffer ;; XXX let bind before save hook; remove save-excursion?
        (save-excursion (run-hooks 'folio-before-save-hook)))

      ;; Core project variable state.
      (goto-char (point-max))
      (insert
       ";;; General:\n\n"
       "(folio-project")
      (mapc (lambda (var)
              (insert "\n   ")
              (when (and var (listp var))
                  (insert ?'))
              (insert (prin1-to-string var)))
              (folio-default-save-list buffer))
      (insert ")\n\n"
              ";;; State:\n\n")

      ;; Save state variables.
      (mapc (lambda (var)
              (insert (with-current-buffer buffer
                        (folio-save-variable var))))
            (with-current-buffer buffer folio-save-list))

      ;; Checksum machinery and output.
      (let ((checksum (folio-hash-md5 (current-buffer))))
        (unless (and checksum (string= checksum last-checksum))
          (setq folio-save-file-checksum checksum)
          (goto-char (point-min))
          (when (search-forward "(current-time-string)" nil t)
            (replace-match (current-time-string)))

          (let ((file-precious-flag t)
                (coding-system-for-write 'emacs-mule))
            (write-region (point-min) (point-max) save-file nil
                          (unless (called-interactively-p 'any) 'quiet))))))))

;;;###autoload
(defun folio-restore-hook ()
  "Restore project state from .aux file.
Return t if a state file exists and was loaded successfully, or
nil otherwise.  This function should be called from the mode
hook."
  (let* ((buffer (current-buffer))
         (save-file (concat (buffer-file-name buffer) ".aux"))
         (progress (make-progress-reporter
                    "Restoring project... " 0 100)))
    (if (load save-file t t t nil)
        (progn
          (run-hooks 'folio-restore-deferred-hook)
          (setq folio-save-file-mtime
                (nth 5 (file-attributes save-file)))
          (setq folio-restore-deferred-hook nil)
          (progress-reporter-done progress)
          (run-hooks 'folio-after-restore-hook)
          t)
      (if (run-hook-with-args-until-success
           'folio-restore-fallback-functions)
          (progn
            (set-buffer-modified-p t)
            (progress-reporter-done progress)
            (run-hooks 'folio-after-restore-hook)
            t)
        (and (message "Restoring project...no data") nil)))))

;;;###autoload
(defun folio-project (format-version &optional args)
;;  "XXX"
  (let* ((version (split-string format-version "\\." t))
         (version-major (string-to-number (car version)))
         (version-minor (if (cadr version)
                            (string-to-number (cadr version))
                          0)))
    ;; XXX checksum check.
    (cond
     ((>= version-major 1)
      (setq folio-project-id (car args)))
     (t
      (error "failure restoring project data.")))))


(defun folio-delete-processes ()
  "Delete all inferior Folio mode processes.
Retain processes having the `query-on-exit' flag set."
  (dolist (p (process-list))
    (cond ((or (memq (process-status p) '(exit signal closed))
               (and (not (process-query-on-exit-flag p))
                    (folio-string-prefix-p
                     "folio-" (process-name p)))
           (delete-process p))))))


(provide 'folio-base)

;;; folio-base.el ends here
