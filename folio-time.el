;;; folio-time.el --- Folio mode time and timers

;; Copyright (C) 2012, 2013  Christoph W. Kluge

;; Author: Christoph W. Kluge <shift.in.emphasis@gmail.com>
;; Keywords: wp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines auxillary functions for operating on time
;; stamps and the definintion and management of single-shot and
;; periodical timers.  Different from Emacs standard idle timers a
;; timer defined by `folio-define-timer' executes its worker function
;; while Emacs is idle, i.e. the worker function is repeatedly
;; executed until user input intercepts.  For fairness to other timers
;; and in order to reduce latencies in the handling of keyboard and
;; mouse events a non-trivial worker function itself should frequently
;; yield or return after having processed a chunk of data.

;;; Code:

(eval-when-compile
  (require 'cl))

(defsubst folio--timer-symbol-interned (name)
  (intern (format "folio-%s-timer" (symbol-name name))))

(defmacro folio-define-timer (name docstring &rest args)
  "Define a timer object with name NAME.
NAME should be a (quoted) symbol.  DOCSTRING describes the timer.

The timer is set up to perform an action by calling FUNCTION
after a delay of SECS seconds, optionally repeatedly whenever
Emacs is idle.  SECS may by any expression that evaluates into an
integer or a floating point number.  ARGS are addtional keyword
arguments and arguments to FUNCTION.

Useful keyword arguments are:

  :repeat        If non-nil the action is repeated in which case the
                 value should be an integer or floating point
                 number or any other expression that evaluates to
                 any of those types.  The unit is seconds.

  :pause         Repeat the action every REPEAT seconds, if REPEAT is
                 non-nil.  SECS and REPEAT may be integers or
                 floating point numbers.  The action is to call
                 FUNCTION with arguments from :args."
  ;; XXX TODO :buffer-local
  (declare (doc-string 2)
           (indent 2))
  (let ((timer-symbol (folio--timer-symbol-interned (eval name)))
        (timer-args args)
        forms)
    (push `(defvar ,timer-symbol nil ,docstring) forms)
    (push `(put ',timer-symbol :class 'folio-timer) forms)
    ;; XXX   (push `(put ',timer-symbol :secs ,secs) forms)
    ;;    (push `(put ',timer-symbol :function ,function) forms)
    ;; Additional keyword args and arguments to :function.
    (while timer-args
      (let ((next (nth 0 timer-args)))
        (if (keywordp next)
            (progn
              (if (eq next :buffer-local)
                  (when (nth 1 timer-args)
                    (push `(make-variable-buffer-local ',timer-symbol) forms)
                    (when (eq (nth 1 timer-args) 'permanent)
                      (push `(put ',timer-symbol 'permanent-local t) forms)))
                (push `(put ',timer-symbol ,next ,(nth 1 timer-args)) forms))
              (setq timer-args (nthcdr 2 timer-args)))
          (push `(put ',timer-symbol :args ',timer-args) forms)
          (setq timer-args nil))))
    `(progn ,@(nreverse (cons `',timer-symbol forms)))))

(defsubst folio-timer-get (timer propname)
  "Return the value of TIMER's PROPNAME property.
TIMER should be a symbol used with `folio-define-timer'."
  (let ((timer-symbol (folio--timer-symbol-interned timer)))
    (get timer-symbol propname)))

(defsubst folio-timer-put (timer propname value)
  "Store TIMER's PROPNAME property with value VALUE."
  (let ((timer-symbol (folio--timer-symbol-interned timer)))
    (put timer-symbol propname value)))

(defun folio-timer-p (timer)
  "Return t if TIMER is a timer object created by `folio-define-timer'."
  (eq (folio-timer-get timer :class) 'folio-timer))

(defsubst folio-idle-time-seconds ()
  "Return current idle time in seconds.
The returned value is a floating point number."
  (let ((time (or (current-idle-time)
                  '(0 0 0))))
    ;; The format is (HIGH LOW MICROS).
    (+ (* (car time) (expt 2 16))
       (cadr time)
       (/ (cl-caddr time) 1000000.0))))

(defsubst folio-time-diff (lhs rhs)
  "Return the time difference between LHS and RHS in seconds.
LHS is the more recent point in time.  Both LHS and RHS are
expected in the format used by `current-time'.
The result is a floating point number."
  (+ (* (- (car lhs) (car rhs)) (expt 2 16))
     (- (cadr lhs) (cadr rhs))))

(defun folio-format-diff-time (since &optional now format)
  "Format the time difference between NOW and SINCE.
NOW defaults to the current point in time.  FORMAT is a string to
format the result different from the default, using
`format-seconds'."
  (format-seconds (or format "%Y, %D, %H, %M, %z%S")
                  (float-time
                   (time-subtract (or now (current-time))
                                  since))))

(defun folio-yield (&optional count seconds nth)
  "Yield every 10th call and check for pending input.
COUNT is the current call number or an equivalent thereof.
SECONDS if non-nil is the maximal time to wait for input.  The
default is not to wait at all.  NTH if non-nil means to perform
this check every NTH call instead.  Return t if input is pending
or nil otherwise.  This function may also trigger a redisplay."
  (when (or (null count) (zerop (% count (or 10 nth))))
    (save-current-buffer (sit-for (or seconds 0)))))

(defsubst folio-time-elapsed (since)
  "Return the number of seconds elapsed since SINCE."
  (folio-time-diff (current-time) since))

(defmacro folio-timed (&rest body)
  "Execute BODY, returning a cons of result and the time elapsed.
The format of the time value is that of `current-time'."
  (let ((now (gensym))
        (result (gensym)))
    `(let ((,now (current-time))
           (,result (progn
                      ,@body)))
       (cons ,result (folio-time-elasped (current-time) ,now)))))

(defsubst folio-timer-eval (timer keyword)
  (let ((value (folio-timer-get timer keyword)))
    (when value
      (if (functionp value)
          (funcall value)
        ;; form or symbol value
        (eval value)))))

(defun folio-timer-run-while-idle (timer)
  "Internal timer function used by `folio-run-timer'.
TIMER should be the timer name used with `folio-define-timer'."
  (condition-case err
      (let ((debug-on-error t)
            (function (folio-timer-get timer :function))
            (args (folio-timer-get timer :args))
            (notify (folio-timer-get timer :notify)))
        (if (folio-timer-get timer :include-runtime)
            (let ((now (current-time)))
              (apply function args)
              (let ((break-time (folio-timer-eval timer :pause)))
                (when break-time
                  (while (sit-for (- break-time
                                     (folio-time-elapsed now)))
                    (setq now (current-time))
                    (apply function args)
                    (setq break-time (folio-timer-eval timer :pause))))))
          (apply function args)
          (let ((break-time (folio-timer-eval timer :pause)))
            (while (and break-time
                        (sit-for break-time))
              (apply function args)
              (setq break-time (folio-timer-eval timer :pause)))))
        (when (folio-timer-get timer :repeat)
          (folio-schedule-timer timer))
        (when notify
          (funcall notify timer)))
    (error
     (folio-cancel-timer timer)
     (message "Error in asynchronous execution of timer `%S': %S"
              timer err))))

(defun folio-timer-running-p (timer)
  "Return non-nil if TIMER is running.
TIMER should be the symbol of a timer object originally set up
with `folio-define-timer'."
  (let ((the-timer (folio--timer-symbol-interned timer)))
    (and (symbol-value the-timer)
         (member (symbol-value the-timer) timer-idle-list) t)))

(defun folio-cancel-timer (timer)
  "Cancel the timer TIMER.
TIMER should be the symbol of a timer object originally set up
with `folio-define-timer'."
  (let ((the-timer (folio--timer-symbol-interned timer)))
    (when (symbol-value the-timer)
      (cancel-timer (symbol-value the-timer)))))

(defun folio-schedule-timer (timer &optional secs)
  "Schedule or re-schedule the timer TIMER.
SEC if non-nil overrides the scheduling time in seconds from now.
TIMER should be the symbol of a timer object originally set up
with `folio-define-timer'."
  (when secs
    (folio-timer-put timer :secs secs))
  (folio-cancel-timer timer)
  (let ((the-timer (folio--timer-symbol-interned timer)))
    (set the-timer (apply 'run-with-idle-timer
                          (folio-timer-eval timer :secs)
                          (folio-timer-get timer :repeat)
                          'folio-timer-run-while-idle (list timer)))))


(provide 'folio-time)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-time.el ends here
