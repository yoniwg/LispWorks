(defvar bms '(
	(7 . bum)
	(5 . bam)
	(3 . bim)
))

(defun checknum (number &optional bmslist (bmspairs bms))
	(if (not (integerp number)) "incorrect input"
		(if (null bmspairs)
			bmslist
			(let ((curNumber (car bmspairs)))
				(checknum 
					number 
					(if (= 0 (mod number (car curNumber)))
						(cons (cdr curNumber) bmslist)
						bmslist
					)
					(cdr bmspairs)
				)
			)		
		)
	)
)