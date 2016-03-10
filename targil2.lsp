(defun numberlistp (chklist)
	(or 
		(null chklist)
		(and (numberp (car chklist)) (numberlistp (cdr chklist)) )
	)
)

(defun square (edge)
	(expt edge 2)
)

(defun rectangle (edge1 edge2) 
	(* edge1 edge2)
)

(defun circle (radius) 
	 (* 3.14 radius radius)
)

(defun triangle (edge alt)
	(/ (* edge alt) 2)
)

(defun ball (radius)
	(* 3.14 (/ 4 3) (expt radius 3))
)

(defun cube (edge) 
	(expt edge 3)
)

(defun shapecalc (sname prmlist)
	(cond 
		((not (listp prmlist)) nil)
		((not (numberlistp prmlist)) nil)
		((and (equal sname 'square) 	(= (length prmlist) 1)) (square (car prmlist)))
		((and (equal sname 'rectangle) 	(= (length prmlist) 2)) (rectangle (car prmlist) (cadr prmlist)))
		((and (equal sname 'circle) 	(= (length prmlist) 1)) (circle (car prmlist)))
		((and (equal sname 'triangle) 	(= (length prmlist) 2)) (triangle (car prmlist) (cadr prmlist)))
		((and (equal sname 'ball) 		(= (length prmlist) 1)) (ball (car prmlist)))
		((and (equal sname 'cube) 		(= (length prmlist) 1)) (cube (car prmlist)))
		(t nil)
	)
)

(defun ballcubediff (radius)
	(and
		(numberp radius)
		(let 
			( 
				(x (ball radius))
				(y (cube (/ (* 2 radius) (sqrt 3) )))
			)
			(list x y (- x y))
		)
	)
)

(defun cubeballdiff (edge)
	(and
		(numberp edge)
		(let
			(
				(x (cube edge))
				(y (ball (/ edge 2)))
			)
			(list x y (- x y))
		)
	)
)
