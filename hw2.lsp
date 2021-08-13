;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; TODO: comment code
(defun BFS (FRINGE) ; BFS searches a list of lists FRINGE layer by layer, returning a single list containing all the leaves in the order they were found
	(cond ((not FRINGE) nil) ; if FRINGE is empty, return nil
		((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE)))) ; if the car FRINGE is a leaf, cons leaves found on the rest of FRINGE
		(t (BFS (concatenate 'list (cdr FRINGE) (car FRINGE))) ; if car FRINGE is a subtree, run BFS on the subtree and the rest of FRINGE, and concatenate results
		)
	)
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond ((equal '(T T T T) S) T) ; if S is equal to goal state, return T
    	(t nil) ; otherwise, return NIL
    )
)
; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poison and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
	(let ((homer (car S)) (baby (cadr S)) (dog (caddr S)) (poison (cadddr S))) ; binded each action to local variables
		(cond ((equal A "h") ; if the action is homer only
			(cond ((eql baby dog) nil) ; if baby and dog on same side, invalid action
				((eql baby poison) nil) ; if baby and poison on same side, invalid action
				(t (cons (cons (not homer) (cdr S)) '()) ; otherwise, move homer across river
				)
			)
		)
		((equal A "b") ; if the action is homer and baby
			(cond ((not (eql homer baby)) nil) ; if homer and baby not on same side, invalid action
				(t (cons (cons (not homer) (cons (not baby) (cons dog (cons poison nil)))) '()) ; otherwise, move homer and baby across river
				)
			)
		)
		((equal A "d") ; if the action is homer and dog
			(cond ((eql baby poison) nil) ; if baby and poison on the same side, invalid action
				((not (eql homer dog)) nil) ; if homer and dog not on same side, invalid action
				(t (cons (cons (not homer) (cons baby (cons (not dog) (cons poison nil)))) '()) ; otherwise, move homer and dog across river
				)
			)
		)
		((equal A "p") ; if the action is homer and poison
			(cond ((eql baby dog) nil) ; if baby and dog on same side, invalid action
				((not (eql homer poison)) nil) ; if homer and poison not on same side, invalid action
				(t (cons (cons (not homer) (cons baby (cons dog (cons (not poison) nil)))) '()) ; otherwise, move homer and poison across river
				)
			)
		)
		)
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
	(concatenate 'list (NEXT-STATE S "h") (NEXT-STATE S "b") (NEXT-STATE S "d") (NEXT-STATE S "p") ; concatenate results of each action into a list of successor states
	)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond ((equal STATES nil) nil) ; if STATES is empty, return nil
		((equal S (car STATES)) T) ; if S is equal to the first element in STATES, return T
		(t (ON-PATH S (cdr STATES)) ; call ON-PATH on the rest of the unchecked states
		)
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	(cond ((not STATES) nil) ; if STATES is empty, return NIL
		((equal (DFS (car STATES) PATH) nil) (MULT-DFS (cdr STATES) PATH)) ; if DFS doesn't find a path to the goal state from car STATES, run MULT-DFS on the unexpanded frontier states
		(t (DFS (car STATES) PATH) ; otherwise, run DFS on that state to find goal state
		)
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
	(cond ((equal (ON-PATH S PATH) T) nil)  ; if S is on the path already, return NIL
		((equal (FINAL-STATE S) T) (concatenate 'list PATH (cons S '()))) ; if S is the goal state, concatenate onto path and return
		(t (MULT-DFS (SUCC-FN S) (concatenate 'list PATH (cons S '()))) ; otherwise, run MULT-DFS on children of S, with the path being PATH cons'd with S
		)		
	)
)
    
