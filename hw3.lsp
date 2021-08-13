;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v 0)
  )

(defun isWall (v)
  (= v 1)
  )

(defun isBox (v)
  (= v 2)
  )

(defun isKeeper (v)
  (= v 3)
  )

(defun isStar (v)
  (= v 4)
  )

(defun isBoxStar (v)
  (= v 5)
  )

(defun isKeeperStar (v)
  (= v 6)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s) 
  (cond ((null s) T) ; if s is empty, all squares have been checked with no boxes found, so goal state is achieved
  	((null (car s)) (goal-test(cdr s))) ; if row being checked is empty, run goal-test on remaining rows
  	((isBox(car (car s))) nil) ; if square being checked isBox, then return nil
  	(t (goal-test (cons (cdr (car s)) (cdr s))) ; else, run goal-test on squares minus the already checked square
  	)
  )
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s "UP" x y) (try-move s "DOWN" x y) (try-move s "LEFT" x y) (try-move s "RIGHT" x y))) ; try each possible move, and return as a list 
	 )
    (cleanUpList result);remove all invalid moves
   );end let
  );

;takes in current state, direction of move, and coordinates of keeper, returns the resulting state of the move if valid, NIL otherwise
(defun try-move (s dir x y)
	(if (eql (check-valid s dir x y) T) ;if move is valid
		(update-board s dir x y) ;return next state
		nil ;otherwise, move is invalid, return NIL
	)
)


;helper function of check-valid, returns how many rows there are
(defun bottom-edge (s)
	(cond ((equal (cdr s) nil) 0) ;if we reach the final row, return 0
		(t (+ (bottom-edge (cdr s)) 1) ;otherwise, add 1 and recursively call bottom-edge again on rest of rows
		)
	)
)

;helper function of check-valid, returns how many columns there are
(defun right-edge (s)
	(cond ((equal (cdr s) nil) 0) ;if we reach final column, return 0
		(t (+ 1 (right-edge (cdr s))) ;otherwise, add 1 and recursively call right-edge on rest of columns
		)
	)
)

;helper function for try-move, checks if the moved being tried is allowed, return T if valid, NIL otherwise
(defun check-valid (s dir x y)
	(cond ((equal dir "UP") ;if direction of move is up
		(cond ((or (eql y 0) (isWall (nth x (nth (- y 1) s)))) nil) ;if keeper at top row or the square above the keeper is a wall, return NIL
			((or (isBox (nth x (nth (- y 1) s))) (isBoxStar (nth x (nth (- y 1) s)))) ;otherwise, if the square above the keeper is a box or boxstar, then
				(if (or (eql y 1) (or (isWall (nth x (nth (- y 2) s))) (or (isBox (nth x (nth (- y 2) s))) (isBoxStar (nth x (nth (- y 2) s)))))) ;if the square two spaces above the keeper is a wall, box, or boxstar
					nil 
					T
				))
			(t T ;otherwise, valid
			)
		)
	)
	((equal dir "DOWN") ;if direction of move is down
		(cond ((or (eql y (bottom-edge s)) (isWall (nth x (nth (+ y 1) s)))) nil) ;if the keeper is at last row or the square below the keeper is a wall, return NIL
			((or (isBox (nth x (nth (+ y 1) s))) (isBoxStar (nth x (nth (+ y 1) s)))) ;if the square below the keeper is a box or boxstar
				(if (or (eql (+ y 1) (bottom-edge s)) (or (isWall (nth x (nth (+ y 2) s))) (or (isBox (nth x (nth (+ y 2) s))) (isBoxStar (nth x (nth (+ y 2) s)))))) ;check square two spaces below the keeper
					nil
					T
				))
			(t T
			)
		)
	)
	((equal dir "LEFT") ;if the direction of move is left
		(cond ((or (eql x 0) (isWall (nth (- x 1) (nth y s)))) nil) ;if the keeper is at col 0 or there is a wall to the left, return NIL
			((or (isBox (nth (- x 1) (nth y s))) (isBoxStar (nth (- x 1) (nth y s)))) ;if the square to the left is a box or boxstar
				(if (or (eql x 1) (or (isWall (nth (- x 2) (nth y s))) (or (isBox (nth (- x 2) (nth y s))) (isBoxStar (nth (- x 2) (nth y s)))))) ;if the keeper is at col 1, or if the square 2 spaces to the left is a wall, box, or boxstar
					nil
					T
				))
			(t T
			)
		)
	)
	((equal dir "RIGHT") ;if the direction of move is right
		(cond ((or (eql x (right-edge (car s))) (isWall (nth (+ x 1) (nth y s)))) nil) ;if the keeper is at last col or there is a wall to the right, return NIL
			((or (isBox (nth (+ x 1) (nth y s))) (isBoxStar (nth (+ x 1) (nth y s)))) ;if the square to the right is a box or boxstar
				(if (or (eql (+ x 1) (right-edge (car s))) (or (isWall (nth (+ x 2) (nth y s))) (or (isBox (nth (+ x 2) (nth y s))) (isBoxStar (nth (+ x 2) (nth y s)))))) ;if the keeper is at last col, or if the square 2 spaces to the right is a wall, box, or boxstar
					nil
					T
				))
			(t T
			)
		)
	)
	)
)

#|this function actually updates the board to the next state. It takes in the current state, direction, and coordinates of the keeper, and outputs the next state. All of the valid options are
hard-coded based on the keeper position and the states of the squares around the keeper based on the specified direction. These new states are passed to update-states.
|#
(defun update-board (s dir x y)
	(cond ((equal dir "UP")
		(cond ((isKeeperStar (nth x (nth y s)))
			(cond ((isBox (nth x (nth (- y 1) s)))
				(if (isStar (nth x (nth (- y 2) s)))
					(update-states s dir x (- y 2) '(5 3 4))
					(update-states s dir x (- y 2) '(2 3 4)) 
				)
			)
			((isBoxStar (nth x (nth (- y 1) s)))
				(if (isStar (nth x (nth (- y 2) s)))
					(update-states s dir x (- y 2) '(5 6 4))
					(update-states s dir x (- y 2) '(2 6 4))
				)
			)
			((isStar (nth x (nth (- y 1) s))) (update-states s dir x (- y 1) '(6 4)))
			(t (update-states s dir x (- y 1) '(3 4))
			)
			)
		)
		(t (cond ((isBox (nth x (nth (- y 1) s)))
			(if (isStar (nth x (nth (- y 2) s)))
				(update-states s dir x (- y 2) '(5 3 0))
				(update-states s dir x (- y 2) '(2 3 0))
			))
			((isBoxStar (nth x (nth (- y 1) s)))
				(if (isStar (nth x (nth (- y 2) s)))
					(update-states s dir x (- y 2) '(5 6 0))
					(update-states s dir x (- y 2) '(2 6 0))
			))
			((isStar (nth x (nth (- y 1) s))) (update-states s dir x (- y 1) '(6 0)))
			(t (update-states s dir x (- y 1) '(3 0))
			)
			)
		))	
	)
	((equal dir "DOWN")
			(cond ((isKeeperStar (nth x (nth y s)))
				(cond ((isBox (nth x (nth (+ y 1) s)))
					(if (isStar (nth x (nth (+ y 2) s)))
						(update-states s dir x y '(4 3 5))
						(update-states s dir x y '(4 3 2)) 
					)
				)	
				((isBoxStar (nth x (nth (+ y 1) s)))
					(if (isStar (nth x (nth (+ y 2) s)))
						(update-states s dir x y '(4 6 5))
						(update-states s dir x y '(4 6 2))
					)
				)
				((isStar (nth x (nth (+ y 1) s))) (update-states s dir x y '(4 6)))
				(t (update-states s dir x y '(4 3))
				)
				)
			)
			(t (cond ((isBox (nth x (nth (+ y 1) s)))
					(if (isStar (nth x (nth (+ y 2) s)))
						(update-states s dir x y '(0 3 5))
						(update-states s dir x y '(0 3 2))
					))
				((isBoxStar (nth x (nth (+ y 1) s)))
					(if (isStar(nth x (nth (+ y 2) s)))
						(update-states s dir x y '(0 6 5))
						(update-states s dir x y '(0 6 2))
						))
				((isStar (nth x (nth (+ y 1) s))) (update-states s dir x y '(0 6)))
				(t (update-states s dir x y '(0 3))
				)
			))
			)	
	)
	((equal dir "LEFT")
			(cond ((isKeeperStar (nth x (nth y s)))
				(cond ((isBox (nth (- x 1) (nth y s)))
					(if (isStar (nth (- x 2) (nth y s)))
						(update-states s dir (- x 2) y '(5 3 4))
						(update-states s dir (- x 2) y '(2 3 4)) 
					)
				)	
				((isBoxStar (nth (- x 1) (nth y s)))
					(if (isStar (nth (- x 2) (nth y s)))
						(update-states s dir (- x 2) y '(5 6 4))
						(update-states s dir (- x 2) y '(2 6 4))
					)
				)
				((isStar (nth (- x 1) (nth y s))) (update-states s dir (- x 1) y '(6 4)))
				(t (update-states s dir (- x 1) y '(3 4))
				)
				)
			)
			(t (cond ((isBox (nth (- x 1) (nth y s)))
					(if (isStar (nth (- x 2) (nth y s)))
						(update-states s dir (- x 2) y '(5 3 0))
						(update-states s dir (- x 2) y '(2 3 0))
					))
				((isBoxStar (nth (- x 1) (nth y s)))
					(if (isStar(nth (- x 2) (nth y s)))
						(update-states s dir (- x 2) y '(5 6 0))
						(update-states s dir (- x 2) y '(2 6 0))
						))
				((isStar (nth (- x 1) (nth y s))) (update-states s dir (- x 1) y '(6 0)))
				(t (update-states s dir (- x 1) y '(3 0))
				)
			))
			)	
	)		
	((equal dir "RIGHT")
			(cond ((isKeeperStar (nth x (nth y s)))
				(cond ((isBox (nth (+ x 1) (nth y s)))
					(if (isStar (nth (+ x 2) (nth y s)))
						(update-states s dir x y '(4 3 5))
						(update-states s dir x y '(4 3 2)) 
					)
				)	
				((isBoxStar (nth (+ x 1) (nth y s)))
					(if (isStar (nth (+ x 2) (nth y s)))
						(update-states s dir x y '(4 6 5))
						(update-states s dir x y '(4 6 2))
					)
				)
				((isStar (nth (+ x 1) (nth y s))) (update-states s dir x y '(4 6)))
				(t (update-states s dir x y '(4 3))
				)
				)
			)
			(t (cond ((isBox (nth (+ x 1) (nth y s)))
					(if (isStar (nth (+ x 2) (nth y s)))
						(update-states s dir x y '(0 3 5))
						(update-states s dir x y '(0 3 2))
					))
				((isBoxStar (nth (+ x 1) (nth y s)))
					(if (isStar(nth (+ x 2) (nth y s)))
						(update-states s dir x y '(0 6 5))
						(update-states s dir x y '(0 6 2))
					))
				((isStar (nth (+ x 1) (nth y s))) (update-states s dir x y '(0 6)))
				(t (update-states s dir x y '(0 3))
				)
			))
			)	
	)	
	)
)

;this function traverses the columns of the board to search for the squares to be updated. Once a state is found to be updated, update-point will be called on that row to actually update the point.
(defun update-states (s dir x y newstates)
	(cond ((equal dir "UP")
		(cond ((eql y 0)
			(if (equal (cdr newstates) nil)
				(cons (update-point (car s) x (car newstates)) (cdr s)) 
				(cons (update-point (car s) x (car newstates)) (update-states (cdr s) dir x y (cdr newstates)))
			))
		(t (cons (car s) (update-states (cdr s) dir x (- y 1) newstates))
		)
		)
	)	
	((equal dir "DOWN")
		(cond ((eql y 0)
			(if (equal (cdr newstates) nil)
				(cons (update-point (car s) x (car newstates)) (cdr s))
				(cons (update-point (car s) x (car newstates)) (update-states (cdr s) dir x y (cdr newstates)))
			))
		(t (cons (car s) (update-states (cdr s) dir x (- y 1) newstates))
		))
	)
	((equal dir "LEFT")
		(cond ((eql y 0) (cons (update-point (car s) x newstates) (cdr s)))
		(t (cons (car s) (update-states (cdr s) dir x (- y 1) newstates))
		)
	))
	((equal dir "RIGHT")
		(cond ((eql y 0) (cons (update-point (car s) x newstates) (cdr s)))
		(t (cons (car s) (update-states (cdr s) dir x (- y 1) newstates))
		)
	))
	)
)

; this function performs the actual change of states of a square(s) within a row. If the direction of the move is left or right, then all of the state updates are performed. If the direction is 
; up or down, then only the single state for the row being updated is done within a single call of update-states.
(defun update-point (row x newstate)
	(cond ((> x 0) (cons (car row) (update-point (cdr row) (- x 1) newstate)))
		((listp newstate)
			(if (equal (cdr newstate) nil)
				(cons (car newstate) (cdr row))
				(cons (car newstate) (update-point (cdr row) x (cdr newstate)))
			)
		)
		(t (cons newstate (cdr row))
		)
	)
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0 
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; h1 is admissible. This is because the minimum cost of reaching the goal is at least moving each misplaced box once. For example, if there are three misplaced boxes, all of which are one space away
; from the goal, then the minimum cost would be 3, since it would take at least a cost of 1 to have each box reach a star. h1 would make this cost 3, which is not an overestimate of the possible actual
; cost to reach the goal
(defun h1 (s)
	(cond ((equal (cdr s) nil) 0)
		((equal (cdr (car s)) nil) (h1 (cdr s)))
		((isBox (car (car s))) (+ 1 (h1 (cons (cdr (car s)) (cdr s)))))
		(t (h1 (cons (cdr (car s)) (cdr s)))
		)
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h604917650 (s)
	(h1 s)
  )

; I ran out of time to make my own heuristic function, but my idea would be to measure the Manhattan distance of each box to the closest star, and take the maximum between this heuristic and h1. If
; I wanted to go even further, I would ensure that each box measures its Manhattan distance in comparison to a unique star. This would definitely be admissible since this function dominates h1, which is
; also admissible.
(defun h_manhattan (s)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
