(defun numberlistp (chklist)
	(or 
		(null chklist)
		(and (numberp (car chklist)) (numberlistp (cdr chklist)) )
	)
)

(defun square (prms)
	(let ((edge (car prms))) (* edge edge))
)

(defun rectangle (prms) 
	(* (car prms) (car (cdr prms)))
)

(defun circle (prms) 
	(let ((radius (car prms))) (* 3.14 radius radius))
)

(defun triangle (prms)
	(/ (* (car prms) (car (cdr prms))) 2)
)

(defun ball (prms)
	(let ((radius (car prms))) (* 3.14 (/ 4 3) radius radius radius))
)

(defun cube (prms) 
	(let ((edge (car prms))) (* edge edge edge))
)

(defun shapecalc (sname prmlist)
	(cond 
		((not (listp prmlist)) nil)
		((not (numberlistp prmlist)) nil)
		((and (equal sname 'square) 	(= (length prmlist) 1)) (square prmlist))
		((and (equal sname 'rectangle) 	(= (length prmlist) 2)) (rectangle prmlist))
		((and (equal sname 'circle) 	(= (length prmlist) 1)) (circle prmlist))
		((and (equal sname 'triangle) 	(= (length prmlist) 1)) (triangle prmlist))
		((and (equal sname 'ball) 		(= (length prmlist) 1)) (ball prmlist))
		((and (equal sname 'cube) 		(= (length prmlist) 1)) (cube prmlist))
		(t nil)
	)
)

(defun ballcubediff (radius)
	(cond
		((not (numberp radius))  nil)
		(T	
			(let 
				( 
					(x (ball (list radius)))
					(y (cube (list (/ (* 2 radius) (sqrt 3) ) )))
				)
				(list x y (- x y))
			)
		)
	)
)

(defun cubeballdiff (edge)
	(cond
		((not (numberp edge)) nil)
		(T	(let(
					(x (cube (list edge)))
					(y (ball (list (/ edge 2))))
				)
				(list x y (- x y))
			)
		)
	)
)
