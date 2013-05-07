;;; folio-nfa.el --- NFA and DFA building blocks for Folio mode

;; Copyright (C) 2013  Christoph W. Kluge

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

;; Support for NFA-εs, non-deterministic finite automata capable of
;; doing spontaneous ε-transitions, i.e. transitions that use up no
;; input, like the empty string ε.  A modified power set construction
;; is used to create a DFA accepting the same language.[1]
;;
;; An ε-move is defined by the symbol 'epsilon; 'any can be used for
;; moves where any input symbol is accepted--the NFA currently does
;; not accept symbol ranges at a given state.
;;
;; An NFA is created from `folio-make-nfa'.  Moves and final states
;; are defined using `folio-add-nfa-transition' and
;; `folio-add-final-nfa-state', respectively.  NFA resources are the
;; initial state (a scalar parametric state), the transition table,
;; and the set of final states.  Notably the NFA's current state is
;; maintained outside the NFA and therefore is input parameter.
;;
;; XXX TODO DFA
;; XXX TODO Levenshtein automata
;; XXX TODO precomputed Levenshtein automata for given k and a set of
;; word lengths
;;
;; [1] Hopcroft, Motwani, Ullman, Introduction to Automata Theory,
;; Languages, and Computation.

;;; Code:

(require 'cl)

(defun folio-make-nfa (start)
  "Return an NFA with the initial state START."
  (let ((nfa (make-vector 3 nil)))
    (aset nfa 0 start)
    nfa))

(defun folio-add-nfa-transition (nfa from-state input to-state)
  "Add a state transition to the NFA.
FROM-STATE is the source parametric state, INPUT the input
symbol, TO-STATE the destination state.  Using the symbol
'epsilon for INPUT makes this transition an epsilon move; 'any
makes the NFA accept any input symbol at FROM-STATE (other than
'any which is a reserved symbol for this purpose.)"
  (let ((transitions (aref nfa 1))
        update
        cell)
    (cond
     ((null transitions)
      (aset nfa 1 (list (cons from-state
                              (list (cons input
                                          (list to-state)))))))
     (t
      (mapc (lambda (x)
              (when (equal (car x) from-state)
                (mapc (lambda (y)
                        (when (eq (car y) input)
                          (push to-state (cdr y))
                          (setq update t))) (cdr x))
                (unless update
                  (push (cons input
                              (list to-state)) (cdr x))
                  (setq update t))))
            transitions)
      (unless update
        (push (cons from-state
                    (list (cons input
                                (list to-state)))) (aref nfa 1)))))))

(defun x-folio-add-nfa-transition (nfa from-state input to-state)
  "Add a state transition to the NFA.
FROM-STATE is the source parametric state, INPUT the input symbol,
TO-STATE the destination state."
  (let ((transition (aref nfa 1))
        cell)
    (cond
     ((null transition)
      (aset nfa 1 (list (cons from-state
                              (list (list (cons input
                                          to-state)))))))
     ((setq cell (assoc from-state transition))
      (nconc (cadr cell) (list (cons input to-state))))
     (t
      (nconc transition
             (list (cons from-state
                         (list (list (cons input
                                           to-state))))))))))

(defun folio-add-final-nfa-state (nfa state)
  "Make STATE a final (or accepting) state of the NFA."
  (let ((final-states (aref nfa 2)))
    (if final-states
        (nconc final-states (list state))
      (aset nfa 2 (list state)))))

(defun folio-final-nfa-state-p (nfa states)
  "Return non-nil if STATES contains at least one final state of
the NFA."
  (let ((final-states (aref nfa 2))
        final)
    (while (if (member (car states) final-states)
               (progn
                 (setq final t)
                 nil)
             (pop states)))
    final))

(defsubst folio-get-nfa-transitions (nfa state)
  "Retrieve the NFA moves possible in state STATE.
Return an alist mapping input symbols to destination states."
  (cdr (assoc state (aref nfa 1))))

(defun folio-accepted-nfa-inputs (nfa states)
  "Return the set of input symbols the NFA accepts at states
STATES."
  (let (inputs)
    (unless (listp states)
      (setq states (list states)))
    (mapc (lambda (x)
            (mapc (lambda (y)
                    (setq inputs (cons y inputs)))
                  (mapcar #'car (folio-get-nfa-transitions
                                 nfa x)))) states)
    (delete-duplicates inputs)))

(defun folio-nfa-move (nfa states input)
  "Apply the input symbol INPUT to the NFA at state STATES.
INPUT also can be a list of symbols.  Return the new states."
  (let (transitions
        new-states)
    (unless (listp input)
      (setq input (list input)))
    (mapc (lambda (state)
            (setq transitions
                  (folio-get-nfa-transitions nfa state))
            (mapc (lambda (i)
                    ;; not consing here, value is list of lists
                    (setq new-states
                          (append (cdr (assq i transitions))
                                  new-states))) input)) states)
    new-states))

(defun folio-nfa-epsilon-closure (nfa states)
  "Find the epsilon-closure for the NFA at state STATES.
The e-closure of a state is the set of all states, including
STATES itself, that you can get to via e-transitions.  This
function is used internally."
  (let (new-states)
    ;; From any state in STATES apply epsilon.  If there are
    ;; e-transition add the destination states to the set of STATES
    ;; and apply epsilon recursively to any new state.
    (mapc (lambda (state)
            (mapc (lambda (new-state)
                    (unless (member new-state states)
                      (push new-state new-states)))
                  (folio-nfa-move
                   nfa (list state) 'epsilon)))
          states)
    (when new-states
      (setq states
            (append states
                    new-states
                    (folio-nfa-epsilon-closure nfa new-states)))))
  (remove-duplicates states :test 'equal))

(defun folio-nfa-start-state (nfa)
  "Return the initial state of the NFA.
This only is a singleton state if there is no e-transition to
other states than the initial."
  (folio-nfa-epsilon-closure nfa (list (aref nfa 0))))

(defun folio-evolve-nfa (nfa states input)
  "Evolve the NFA by applying the input symbol INPUT to each
state in STATES.  Return the new NFA states including those
reachable by e-moves."
  (let (new-states)
    (unless (listp states)
      (setq states (list states)))
    (unless (listp input)
      (setq input (list input)))
    (unless (memq 'any input)
      (setq input (cons 'any input)))
    (mapc (lambda (state)
            (mapc (lambda (new-state)
                    (push new-state new-states))
                  (folio-nfa-move
                   nfa (list state) input))) states)
    (remove-duplicates
     (folio-nfa-epsilon-closure nfa new-states) :test 'equal)))

(defun folio-make-dfa (start)
  "Return a DFA with the initial state START."
  (let ((dfa (make-vector 3 nil)))
    (aset dfa 0 start)
    (aset dfa 1 (make-hash-table :test 'equal))
    (aset dfa 2 (make-hash-table :test 'equal))
    dfa))

(defun folio-add-dfa-transition (dfa from-state input to-state)
  "Add a state transition to the DFA.
FROM-STATE is the source parametric state, INPUT the input
symbol, TO-STATE the destination states.  INPUT may be 'any, but
not 'epsilon, obviously."
  (message "DFA transition from-state %s input %s to-state %s"
  from-state (if (symbolp input) (format "%s" input) (format "%c" input))
           to-state)
  (when (null (gethash from-state (aref dfa 1)))
    (puthash from-state (make-hash-table :test 'equal)
             (aref dfa 1)))
  (puthash input to-state (gethash from-state (aref dfa 1))))

(defun folio-add-final-dfa-state (dfa state)
  "Make STATE a final state of the DFA."
  (puthash state t (aref dfa 2)))

(defun folio-final-dfa-state-p (dfa state)
  "Return non-nil if STATE is final state of the DFA."
  (gethash state (aref dfa 2)))

(defsubst folio-get-dfa-transitions (dfa state)
  (gethash state (aref dfa 1)))

(defun folio-evolve-dfa (dfa state input)
  "Evolve the DFA according to the current state STATE.
Test with INPUT as the input symbol.  Return the new DFA state or
nil if INPUT is rejected."
  (let ((transitions (gethash state (aref dfa 1)))
        new-state)
    (when transitions
      (setq new-state (gethash input transitions))
      (unless new-state
        (setq new-state (gethash 'any transitions))))
    new-state))


(defvar nfa nil)
(defvar dfa nil)

(defun folio-nfa-test ()
  (setq nfa (folio-make-nfa '(0 . 0)))
  (when t
  (folio-add-nfa-transition nfa '(1 . 2) ?b '(3 . 4))
  (folio-add-nfa-transition nfa '(1 . 2) ?c '(7 . 8))
  (folio-add-nfa-transition nfa '(2 . 3) ?d '(0 . 0))
  (folio-add-nfa-transition nfa '(2 . 3) 'epsilon '(0 . 0)))
  (folio-add-final-nfa-state nfa '(7 . 7))
  (folio-add-final-nfa-state nfa '(9 . 9))
  (message "%S" nfa)

  (setq dfa (folio-make-dfa '(0 . 0)))
  (folio-add-dfa-transition dfa '(1 . 2) 'any '(3 . 4))

  (folio-add-dfa-transition dfa '(1 . 3) ?a '(3 . 4))
  )



(provide 'folio-nfa)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-nfa.el ends here
