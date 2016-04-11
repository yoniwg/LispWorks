;;;;;;;;;;;;;;;;;;; Branch 1 ;;;;;;;;;;;;;;;;;;;
(defun rangelst-from-to(FROM TO) 
	(if (> FROM TO) nil (
			cons FROM (rangelst-from-to (+ FROM 1) TO)
		)
	)
)

(defun rangelst (N) 
	(rangelst-from-to 2 (/ N 2))
	
)

(defun divisiblep (N M) 
	(zerop (mod N M))
)

(defun primep-rec (N list-n)
	(if (null list-n) 
		t
		(and 
			(not (divisiblep N (car list-n)))
			(primep-rec n (cdr list-n))
		)
	)
)

(defun primep (N) 
	(if (and (integerp N) (> N 0))
		(primep-rec N (rangelst N))
		"ERROR: N should be positive integer"
	)
)

;;;;;;;;;;;;;;;;;;; Branch 2 ;;;;;;;;;;;;;;;;;;;
(defun primelst (N) 
	(if (and (integerp N) (> N 0))
		(remove-if-not 'primep  (rangelst-from-to 1 n))
		"ERROR: N should be positive integer"
	)
)

;;;;;;;;;;;;;;;;;;; Branch 3 ;;;;;;;;;;;;;;;;;;;

(defun ziplst (L1 L2) 
	(if (and (listp L1) (listp L2))
		(if (or (null L1) (null L2)) 
			nil 
			(cons (list (first L1) (first L2)) (ziplst (rest L1) (rest L2)))
		)
		"ERROR - No lists"
	)
)

;;;;;;;;;;;;;;;;;;; Branch 4 ;;;;;;;;;;;;;;;;;;;

(defun mymax (L &optional (N nil)) 
	(if (null L)
		N
		(if (or (null N) (> (first L) N))
			(mymax (rest L) (first L))
			(mymax (rest L) N)
		)
	)
)