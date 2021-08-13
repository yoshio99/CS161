(defun PAD (N) ; takes in an integer argument N (N >= 0), and returns the Nth Padovan Number. PAD(N) = PAD(N-1) + PAD(N-2) + PAD(N-3)
	(cond ((= 0 N) 1) ; Base Case: if N == 0, then return 1
		((= 1 N) 1) ; Base Case: if N == 1, then return 1
		((= 2 N) 1) ; Base Case: if N == 2, then return 1
		(t (+ (PAD (- N 1)) (PAD (- N 2)) (PAD (- N 3))) ; Recursive Case: if N >= 3, add the sum of the results of three PAD functions with parameters N-1, N-2, and N-3
		)
	)		
)

(defun SUMS (N) ; takes in an integer argument N (N >= 0), and returns the num of the additions PAD would perform if its argument was N
	(cond ((= 0 N) 0) ; Base Case: if N == 0, then return 0 (no additions required)
		((= 1 N) 0) ; Base Case: if N == 1, then return 0
		((= 2 N) 0) ; Base Case: if N == 2, then return 0
		(t (+ 2 (SUMS (- N 1)) (SUMS (- N 2)) (SUMS (- N 3))) ; Recursive Case: if N >= 3, then this statement requires two additions, while recursively calling SUMS with the same three arguments that 
		)														;  PADS would call if its parameter was N
	)
)

(defun ANON (TREE) ; takes in a list TREE (which represents a tree), and returns an identical list, except that all of the elements (including in subtrees) are set to zero 
	(cond ((not TREE) NIL) ; Base Case: if TREE is the empty list, return NIL
		((atom TREE) '0) ; Base Case: if TREE is an atom, return '0
		(t (cons (ANON (car TREE)) (ANON (cdr TREE))) ; Recursive Case: recursively calls ANON twice, once with the head of TREE, and once with the tail of TREE. It then cons the two results together
		)
	)
)