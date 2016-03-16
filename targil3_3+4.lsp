(defun middlenum-inputp (l)
	(and
		(listp l)
		(= (length l) 3)
		(let ((a (car l)) (b (cadr l)) (c (caddr l)))
			(and 
				(/= a b)
				(/= a c)
				(/= b c)
			)
		)
	)
)

(defun middlenum (l)
	(if (middlenum-inputp l)
		(let ((a (car l)) (b (cadr l)) (c (caddr l)))
			(cond
				((< b a) (middlenum (list b a c)))
				((< c b) (middlenum (list a c b)))
				(t b)
			)
		)
		"incorrect input"		
	)
)

(defun num2digit-inputp (num)
	(and
		(integerp num)
		(>= num 100)
		(< num 1000)
	)
)

(defun number-to-list (num &optional lst)
	(if (zerop num)
		lst
		(let ((dig (mod num 10))) 
			(number-to-list (/ (- num dig) 10) (cons dig lst))
		)
	)
)

(defun num2digit (num) 
	(if (num2digit-inputp num)
		(middlenum (number-to-list num))
		"incorrect input"
	)
)