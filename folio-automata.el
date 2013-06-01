;;; folio-automata.el --- Finite State Automata for Folio mode

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

;; This package provides support for NFAs, DFAs, and DAFSAs (MAFSA).

;; NFAs are NFA-εs, non-deterministic finite automata capable of doing
;; spontaneous ε-transitions, i.e. transitions that use up no input,
;; like the empty string ε.
;;
;; An ε-move is defined by the symbol 'epsilon; 'any can be used for
;; moves where any input symbol is accepted--the NFA currently does
;; not accept symbol ranges at a given state.
;;
;; An NFA is created from `folio-make-nfa'.  Moves and final states
;; are defined using `folio-nfa-add-transition' and
;; `folio-nfa-add-final-state', respectively.  NFA resources are the
;; initial state (a scalar parametric state), the transition table,
;; and the set of final states.  Notably the NFA's current state is
;; maintained outside the NFA and therefore is input parameter.

;; Using a modified power set construction `folio-nfa-to-dfa' converts
;; a given NFA into a deterministic finite state automaton (DFA)
;; recognizing the same language as the NFA, but executing more
;; efficiently.[1]

;; `folio-make-mafsa' creates a minimal acyclic finite state automaton
;; for use with static dictionaries where fast access times,
;; lexicographic ordering, and low memory footprint through prefix and
;; infix compression is required.[2]

;; `folio-make-levenshtein-nfa' creates a Levenshtein automaton for a
;; given word and edit distance.  The intersection of the equivalent
;; DFA with a dictionary FSA allows fuzzy or similarity searches (or
;; the implementation of suggesters and auto-correctors).

;; XXX TODO pre-computed universal Levenshtein automata for given k
;; and a set of word lengths.

;; [1] Hopcroft, Motwani, Ullman, Introduction to Automata Theory,
;;       Languages, and Computation.
;; [2] Daciuk,􏰏Watson, and Watson, Incremental Construction of Minimal
;;       Acyclic Finite State Automata and Transducers.

;;; Code:

(require 'cl)

(defun folio-make-nfa (start)
  "Return an NFA with the initial state START."
  (let ((nfa (make-vector 3 nil)))
    (aset nfa 0 start)
    nfa))

(defun folio-nfa-add-transition (nfa from-state input to-state)
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

(defun folio-nfa-add-final-state (nfa state)
  "Make STATE a final (or accepting) state of the NFA."
  (let ((final-states (aref nfa 2)))
    (if final-states
        (nconc final-states (list state))
      (aset nfa 2 (list state)))))

(defun folio-nfa-final-state-p (nfa states)
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

(defsubst folio-nfa-get-transitions (nfa state)
  "Retrieve the NFA moves possible in state STATE.
Return an alist mapping input symbols to destination states."
  (cdr (assoc state (aref nfa 1))))

(defun folio-nfa-accepted-inputs (nfa states)
  "Return the set of input symbols the NFA accepts at states
STATES."
  (let (inputs)
    (unless (listp states)
      (setq states (list states)))
    (mapc (lambda (x)
            (mapc (lambda (y)
                    (setq inputs (cons y inputs)))
                  (mapcar #'car (folio-nfa-get-transitions
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
                  (folio-nfa-get-transitions nfa state))
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
states other than the initial."
  (folio-nfa-epsilon-closure nfa (list (aref nfa 0))))

(defun folio-nfa-evolve (nfa states input)
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

(defun folio-dfa-add-transition (dfa from-state input to-state)
  "Add a state transition to the DFA.
FROM-STATE is the source parametric state, INPUT the input
symbol, TO-STATE the destination state.  INPUT may be 'any, but
not 'epsilon, obviously."
  (when (null (gethash from-state (aref dfa 1)))
    (puthash from-state (make-hash-table :test 'equal)
             (aref dfa 1)))
  (puthash input to-state (gethash from-state (aref dfa 1))))

(defun folio-dfa-add-final-state (dfa state)
  "Make STATE a final state of the DFA."
  (puthash state t (aref dfa 2)))

(defun folio-dfa-final-state-p (dfa state)
  "Return non-nil if STATE is a final (accepting) state of the
DFA."
  (gethash state (aref dfa 2)))

(defsubst folio-dfa-start-state (dfa)
  "Return the initial state of the DFA."
  (aref dfa 0))

(defsubst folio-dfa-transitions (dfa state)
  "Return the transitions for the DFA at state STATE.
This is a mapping of input symbols to destination state."
  (gethash state (aref dfa 1)))

(defun folio-dfa-transition-labels (dfa state)
  "Return the transition labels for the DFA at state STATE.
The return value is an alist mapping input symbols to destination
states."
  (let ((transitions (folio-dfa-transitions dfa state))
        labels)
    (when transitions
      (maphash (lambda (k v)
                 (setq labels (cons k labels)))
               transitions))
    labels))

(defun folio-dfa-evolve (dfa state input)
  "Evolve the DFA from the current state STATE by applying INPUT
as the input symbol.  Return the new DFA state or nil if INPUT is
rejected."
  (let ((transitions (folio-dfa-transitions dfa state))
        new-state)
    (when transitions
      (setq new-state (gethash input transitions))
      (unless new-state
        (setq new-state (gethash 'any transitions))))
    new-state))

(defun folio-nfa-to-dfa (nfa)
  "Return a DFA corresponding to the NFA by powerset construction.
The new DFA is able to recognize the same language as the NFA,
but executes more efficiently.

The NFA is the non-deterministic finite automaton created and
defined by `folio-make-nfa', `folio-nfa-add-transition',
`folio-nfa-add-final-state', respectively.  The set of all states
of the DFA is the powerset of Q, the set of all possible subsets
of Q, with Q being the set of all possible states of the NFA."
  ;; The initial state T of the DFA constructed from this NFA(-ε) is
  ;; the set of all NFA states that are reachable from the NFA's
  ;; initial state by ε-moves.
  (let* ((current-state (folio-nfa-start-state nfa))
         (dfa (folio-make-dfa current-state))
         (marked-states (make-hash-table :test 'equal))
         (states (list current-state))
         inputs
         new-state)
    ;; The transition function of the DFA maps a state S (representing
    ;; a subset of Q) and an input symbol x to the set T(S,x) =
    ;; ∪{T(q,x) | q ∈ S}, the set of all states that can be reached by
    ;; an x-transition from a state in S [Wikipedia].
    (while states
      (setq current-state (pop states)
            inputs (folio-nfa-accepted-inputs
                    nfa current-state))
      ;; Subset construction: Cycle the NFA by applying inputs
      ;; sequentially ...
      (mapc (lambda (input)
              (unless (eq input 'epsilon)
                ;; current-state is the new-state is the e-closure of
                ;; the move (T,x)
                (setq new-state (folio-nfa-evolve
                                 nfa current-state input))
                ;; visit any unmarked state only once ...
                (unless (gethash new-state marked-states)
                  (puthash new-state t marked-states)
                  (push new-state states)
                  ;; The final states of the DFA are those sets that
                  ;; contain a final state of the NFA.
                  (when (folio-nfa-final-state-p nfa new-state)
                    (folio-dfa-add-final-state dfa new-state)))
                (folio-dfa-add-transition
                 dfa current-state input new-state))) inputs))
    dfa))

(defun folio-dfa-string-match-p (dfa str)
  (let ((state (folio-dfa-start-state dfa))
        (i 0)
        (len (length str)))
    (while (and state (< i len))
      (setq state (folio-dfa-evolve dfa state (aref str i)))
      (setq i (1+ i)))
    (and (folio-dfa-final-state-p dfa state) t)))

(defun folio-make-levenshtein-nfa (word max-distance)
  "Return a Levenshtein automaton for word WORD.
The NFA accepts any input up to a maximal Levenshtein edit
distance MAX-DISTANCE.  `folio-nfa-to-dfa' should be used to
transform the NFA into an equivalent DFA accepting the same
input."
  (let ((nfa (folio-make-nfa '(0 . 0)))
        (len (length word))
        (i 0))
    ;; Setup edges of the form '(i . d), starting from the initial
    ;; state '(0 . 0) with state counter, that is character position i
    ;; ranging from 0 to (1- |word|), and edit distance d.  d becomes
    ;; state parameter in the form of an error count in the range (0,
    ;; max-distance).
    (while (< i len)
      (dotimes (d (1+ max-distance))
        ;; correct character, and correct path for d = 0
        (folio-nfa-add-transition
         nfa (cons i d) (aref word i) (cons (1+ i) d))
        (when (< d max-distance)
          ;; deletion
          (folio-nfa-add-transition
           nfa (cons i d) 'any (cons i (1+ d)))
          ;; insertion
          (folio-nfa-add-transition
           nfa (cons i d) 'any (cons (1+ i) (1+ d)))
          ;; substitution
          (folio-nfa-add-transition
           nfa (cons i d) 'epsilon (cons (1+ i) (1+ d)))))
      (setq i (1+ i)))
    ;; Setup final states at (length word).
    (dotimes (d (1+ max-distance))
      (when (< d max-distance)
        (folio-nfa-add-transition
         nfa (cons len d) 'any (cons len (1+ d))))
      (folio-nfa-add-final-state nfa (cons len d)))
    nfa))


;;;; MAFSAs

(defun folio-make-mafsa-state (fsa)
  (let ((state (make-vector 3 nil)))
    (aset state 0 (incf (aref fsa 0)))
    (aset state 2 (make-char-table 'mafsa))
    state))

(defun folio-mafsa-add-transition (state char next-state)
  (let ((edges (aref state 2)))
    (aset edges char next-state)))

(defsubst folio-mafsa-move (state char)
  (aref (aref state 2) char))

(defun folio-mafsa-transition-labels (_fsa state)
  (let (inputs)
    (map-char-table (lambda (k v)
                      (setq inputs (cons k inputs)))
                    (aref state 2))
    inputs))

(defsubst folio-mafsa-mark-final (state)
  (aset state 1 t))

(defsubst folio-mafsa-final-state-p (_fsa state)
  (aref state 1))

(defun folio-make-mafsa ()
  (let ((fsa (make-vector 5 nil)))
    ;; running sequence to draw node ids from
    (aset fsa 0 -1)
    ;; common prefix
    (aset fsa 1 "")
    ;; start state
    (aset fsa 2 (folio-make-mafsa-state fsa))
    ;; table mapping source state to destination state in the
    ;; minimized part of the FSA
    (aset fsa 3 (make-hash-table :test #'equal))
    ;; slot 4 maintains unchecked states of the yet unminimized part
    ;; of the FSA
    fsa))

(defsubst folio-mafsa-common-prefix (fsa &optional prefix)
  "When PREFIX is non-nil set otherwise get the common prefix.
This function is used internally when constructing the FSA."
  (if prefix
      (aset fsa 1 prefix)
    (aref fsa 1)))

(defsubst folio-mafsa-start-state (fsa)
  "Return the start state of the FSA."
  (aref fsa 2))

(defmacro folio-mafsa-states (fsa)
  "Return the FSA's source and target states.
This macro is used internally."
  `(aref ,fsa 3))

(defsubst folio-mafsa-next-state (fsa state)
  (gethash state (folio-mafsa-states fsa)))

(defsubst folio-mafsa-add-state (fsa from-state to-state)
  (puthash from-state to-state (folio-mafsa-states fsa)))

(defmacro folio-mafsa-unchecked-states (fsa)
  `(aref ,fsa 4))

(defun folio-mafsa-minimize (fsa prefix)
  (let* ((states (folio-mafsa-unchecked-states fsa))
         (len (length states))
         current char child)
    (while (< prefix len)
      (setq current (pop states)
            child (folio-mafsa-next-state
                   fsa (elt current 0)))
      (if child
          ;; replace child with previously encountered state
          (folio-mafsa-add-transition
           (elt current 2) (elt current 1) child)
        ;; keep state
        (folio-mafsa-add-state fsa child child))
      (setq prefix (1+ prefix)))
    (setf (folio-mafsa-unchecked-states fsa) states)))

(defun folio-mafsa-insert-word (fsa word)
  (let* ((prefix 0)
         (common-prefix (folio-mafsa-common-prefix fsa))
         (len (min (length common-prefix) (length word)))
         char state next-state)
    (catch 'prefix
      (while (< prefix len)
        (when (/= (aref word prefix)
                  (aref common-prefix prefix))
          (throw 'prefix prefix))
        (setq prefix (1+ prefix))))
    ;; update common prefix
    (folio-mafsa-common-prefix fsa word)
    (folio-mafsa-minimize fsa prefix)
    ;; add suffix
    (unless (setq len (length word)
                  state (caar (folio-mafsa-unchecked-states fsa)))
      (setq state (folio-mafsa-start-state fsa)))
    (while (< prefix len)
      (setq next-state (folio-make-mafsa-state fsa)
            char (aref word prefix))
      (folio-mafsa-add-transition state char next-state)
      (push (list next-state char state)
            (folio-mafsa-unchecked-states fsa))
      (setq state next-state
            prefix (1+ prefix)))
    (folio-mafsa-mark-final state)))

(defun folio-mafsa-finalize (fsa)
  (folio-mafsa-minimize fsa 0))

(defun folio-mafsa-evolve (_fsa state input)
  (folio-mafsa-move state input))

;; XXX TODO prefix-match
(defun folio-mafsa-string-accept-p (fsa word)
  (let ((state (folio-mafsa-start-state fsa))
        (i 0)
        (len (length word)))
    (while (and (< i len)
                (setq state (folio-mafsa-evolve
                             fsa state (aref word i))))
      (setq i (1+ i)))
    (when state
      (folio-mafsa-final-state-p fsa state))))

(defun folio-mafsa-intersect (fsa dfa)
  (let ((intersect (lambda (lhs rhs)
                     ;; Return the intersection of the two lists
                     ;; (sets) LHS and RHS using `eq'.  RHS may
                     ;; contain the input symbol 'any.
                     (let (elt interq)
                       (if (memq 'any rhs)
                           (setq interq lhs)
                         (while lhs
                           (setq elt (car lhs)
                                 lhs (cdr lhs))
                           (when (memq elt rhs)
                             (setq interq (cons elt interq)))))
                       interq)))
        (states `((""
                   ,(folio-mafsa-start-state fsa)
                   ,(folio-dfa-start-state dfa))))
        state fsa-state xfsa-state dfa-state xdfa-state
        path xpath edges output)
    (while states
      (setq state (pop states)
            path (elt state 0)
            fsa-state (elt state 1)
            dfa-state (elt state 2)
            edges (funcall intersect
                           (folio-mafsa-transition-labels
                            fsa fsa-state)
                           (folio-dfa-transition-labels
                            dfa dfa-state)))
      (mapc
       (lambda (x)
         (setq xdfa-state (folio-dfa-evolve
                           dfa dfa-state x)
               xfsa-state (folio-mafsa-evolve
                           fsa fsa-state x))
         (when (and xfsa-state xdfa-state)
           (setq xpath (concat path (list x)))
           (push `(,xpath ,xfsa-state ,xdfa-state) states)
           (when (and (folio-mafsa-final-state-p
                       fsa xfsa-state)
                      (folio-dfa-final-state-p
                       dfa xdfa-state))
             (push xpath output)))) edges))
    (nreverse output)))



(provide 'folio-automata)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; folio-automata.el ends here
