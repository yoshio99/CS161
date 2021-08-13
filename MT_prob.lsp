(defun COMP-DUP (x)
   (cond ((equal x '()) nil)
      ((eql (car x) (cadr x)) (COMP-DUP (cdr x)))
      (t (cons (car x) (COMP-DUP (cdr x)))
      ) 
   )
)