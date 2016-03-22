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
