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
;; initial state, the transition table, and the set of final states.
;; Notably the NFA's current state is maintained outside the NFA and
;; therefore is input parameter.
;;
;; XXX TODO DFA
;; XXX TODO Levenshtein automata
;; XXX TODO precomputed Levenshtein automata for given k and a set of
;; word lengths
;;
;; [1] Hopcroft, Motwani, Ullman, Introduction to Automata Theory,
;; Languages, and Computation

;;; Code:

(require 'cl)

(defun folio-make-nfa (start)
  "Return an NFA with the initial state START."
  (let ((nfa (make-vector 3 nil)))
    (aset nfa 0 start)
    nfa))

(defun folio-add-nfa-transition (nfa from-state input to-state)
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
  (let ((final-states (aref nfa 2)))
    (if final-states
        (nconc final-states (list state))
      (aset nfa 2 (list state)))))

(defun folio-final-nfa-states-p (nfa states)
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
  "Retrieve the NFA moves possible in state STATE.  Return an
alist mapping input symbols to destination states."
  (cadr (assoc state (aref nfa 1))))

(defun folio-accepted-nfa-inputs (nfa states)
  (let (inputs)
    (mapc (lambda (x)
            (nconc inputs
                   (mapcar #'car (folio-get-nfa-transitions
                                  nfa x)))) states)
    (delete-duplicates inputs)))

(defun folio-nfa-accept-input (nfa input state)
  (let ((transitions (folio-get-nfa-transitions nfa state))
        new-states)
    (unless (listp input)
      (setq input (list input)))
    (mapc (lambda (x)
            (setq new-states
                  (cons (cdr (assq x transitions))
                        new-states)))
          input)
    (delq nil new-states)))

(defun folio-expand-nfa-frontier (nfa states)
  "Expand the NFA frontier to the set of STATES.
STATES is a set \(list) of parametric destination states.  Return
STATES updated to reflect the current frontier of states.  This
function is used internally."
  (let ((next states)
        transitions)
    (while next
      (mapc (lambda (x)
              (unless (member x states)
                (push x states)
                (push x next)))
            (folio-nfa-accept-input nfa 'epsilon (car next)))
      (pop next))
    states))

(defun folio-nfa-start-state (nfa)
  "Return the start state of the NFA."
  (folio-expand-nfa-frontier nfa (list (aref nfa 0))))

(defun folio-nfa-evolve (nfa states input)
  "Evolve the NFA according to the current NFA states STATES, the
NFA's transition table, and input symbol INPUT.  Return the new
NFA state."
  (let (new-states)
    (mapc (lambda (x)
            (nconc new-states
                   (folio-nfa-accept-input nfa input x)))
          states)
    (folio-expand-nfa-frontier nfa (delq nil new-states))))

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
  )



(provide 'folio-nfa)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-nfa.el ends here
