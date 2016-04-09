(defvar func-assoc-list
	(list
		(cons 'pi1 'pi1-map-func)
		(cons 'pi2 'pi2-map-func)
		(cons 'pi3 'pi3-map-func)
	)
)
(defun num-to-k (k)
	(if (zerop k)
		(list 0)
		(append (num-to-k (- k 1)) (list k))
	)
)

(defun pi1-map-func (element)
	(* (expt -1 element) (/ 4 (+ 1 (* 2 element))))
)

(defun pi2-map-func (element)
	(/ 6 (expt (+ 1 element) 2))
)

(defun pi3-map-func (element)
	(* (expt (/ 1 16) element)
		(+
			(/ 4 (+ (* 8 element) 1))
			(/ -2 (+ (* 8 element) 4))
			(/ -1 (+ (* 8 element) 5))
			(/ -1 (+ (* 8 element) 6))
		)
	)
)

(defun pi-map-reduce (func-name num-list)
	(if (null num-list) 
		0
		(+ 
			(funcall func-name (car num-list)) 
			(pi-map-reduce func-name (cdr num-list))
		)
	)
)

(defun pi1 (k)
	(pi-map-reduce (cdr (assoc 'pi1 func-assoc-list))(num-to-k k))
)

(defun pi2 (k)
	(sqrt (pi-map-reduce (cdr (assoc 'pi2 func-assoc-list))(num-to-k k)))
)

(defun pi3 (k)
	(pi-map-reduce (cdr (assoc 'pi3 func-assoc-list))(num-to-k k))
)

(defun picalcp (pi-func k)
	(and
		(assoc pi-func func-assoc-list)
		(integerp k)
	)
)

(defun picalc (pi-func k)
	(if (picalcp pi-func k)
		(coerce (funcall pi-func k) 'double-float)
		"incorrect input"
	)
)