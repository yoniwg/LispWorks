(defun rangelst (N &optional (lst NIL)) 
	(cond
		(
			(null lst) 
			(let
				( (first-N (floor (/ N 2))) )
				(rangelst first-N (list first-N))
			)
		)
		(
			(<= N 2)
			lst
		)
		(
			T
			(rangelst (- N 1) (cons (- N 1) lst))
		)
	)
)

(defun divisiblep (n m) 
	(zerop (mod n m))
)

(defun primep-rec (n list-n)
	(if (null list-n) 
		t
		(and 
			(not (divisiblep n (car list-n)))
			(primep-rec n (cdr list-n))
		)
	)
)

(defun primep (n) 
	(primep-rec n (rangelst n))
)
