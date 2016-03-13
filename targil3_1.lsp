(defvar 
	months '(
			(jan . 31) 
			(feb . (28 29))
			(mar . 31)
			(apr . 30)
			(may . 31)
			(jun . 30)
			(jul . 31)
			(aug . 31) 
			(sep . 30)
			(oct . 31)
			(nov . 30) 
			(dec . 31)
			)
)

(defun leapp (year)
	(or
		(and 
			(= (mod year 4) 0)
			(> (mod year 100) 0)
		)
		(= (mod year 400) 0)
	)
)

(defun daysofmonth (month is-leap)
	(let ((monthdays (cdr (assoc month months))))
		(if (eq month 'feb) 
			(if is-leap
				(cadr monthdays)
				(car monthdays)
			)
			monthdays
		)
	)
)
(defun datep (date) 
	(and
		(listp date)
		(= (length date) 3)
		(let ((day (car date)) (mon (cadr date)) (year (caddr date)))
			(and
				(integerp day)
				(> day 0)
				(symbolp mon)
				(not (null (assoc mon months)))
				(<= day (daysofmonth mon (leapp year)))
				(integerp year)
				(> year 0)
			)
		)
	)
)

(defun days-until-month (month is-leap &optional (monthslist months))
	(let ((monthname (caar monthslist)))
		(if (eq monthname month)
			0
			(+
				(daysofmonth monthname is-leap)
				(days-until-month month is-leap (cdr monthslist))
			)
		)
	)
)

(defun dayscount (taarich) 
	(if (datep taarich)
		(let* ((day (car taarich)) (mon (cadr taarich)) (year (caddr taarich)))
			(+ day (days-until-month mon (leapp year)))
		)
		"incorrect input"
	)
)
			
