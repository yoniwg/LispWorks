;;
;; Targil 6 - Functional Programming - semester B - 5776
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 1 - Input ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jctMarks '((12345 75 "English")
                   (23452 83 "Physics")
                   (23560 81 "Statistics")
                   (23415 61 "Computer")
                   (23459 90 "Physics")    
                   (12345 75 "Computer")
                   (23452 100 "Statistics")))

(defvar teacherName '(("Aharoni" "English")
                      ("Melamed" "Physics")
                      ("Kaner" "Computer")
                      ("Zloti" "Statistics")
                      ("Korman" "Philosophy")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 1 - Checks ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return true if lst and is list with equal size to lambdas-list, 
;; and for each i, lambdas-list[i] is T when it invokes on lst[i].
;; For example: lst=(12345 75 "English") returns true if lambdas-list=(#'numberp #'numberp #'stringp)
(defun lambdas-listp (lst lambdas-list)
	(or 
		(null lst)
		(and 
			(listp lst)
			(equal (list-length lst) (list-length lambdas-list))
			(funcall (first lambdas-list) (first lst))
			(lambdas-listp (rest lst) (rest lambdas-list))
		)
	)
)

(defun gradep (grade)
	(and
		(numberp grade)
		(>= grade 0)
		(<= grade 100)
	)
)
		
(defun teachersAvg-Check (list1 list2)
	(and
		(listp list1)
		(listp list2)
		(every #'(lambda (x) (lambdas-listp x (list #'numberp #'gradep #'stringp))) list1)
		(every #'(lambda (x) (lambdas-listp x (list #'stringp #'stringp))) list2)
	)
)
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 1 - Program ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gets list of numbers and return the average
(defun average (list) 
	(round
		(/ (reduce #'+ list) (list-length list))
	)
)

;; returns list of students and grades' average of specific course
(defun course-students-and-avg (course marks-list)
	(let ((course-mark-list
			(remove-if-not
				#'(lambda (rec) (equal (third rec) course)) 
				marks-list
			) 
		))
		(if (null course-mark-list) nil
			(append
				(mapcar #'first course-mark-list)
				(list (average (mapcar #'second course-mark-list)))
			)
		)
	)
)


(defun teachersAvg (list1 list2)
(if (teachersAvg-Check list1 list2) 
	(mapcar
		#'(lambda (record) 
			(cons 
				(first record) 
				(course-students-and-avg (second record) list1) 
			)
		)
		list2
	)
"ERROR INPUT")
)

(teachersAvg jctMarks teacherName)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 2 - Input ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar courses '(	("English" (
						("Aharoni" ((12345 75) (54689 50) (23452 95) (21437 70)))
						("Michaeli" ((12346 70) (89756 65)))
					))
					("Physics" (
						("Melamed" ((23452 83) (12345 90))) 
						("Levy" ((23454 85))) 
						("Cohen" ((23444 90)))
					))
					("Computer" (
						("Kaner" ((23415 61))) 
						("Katz" ((12345 75) (54689 80)))
					))
					("Statistics" (
						("Zloti" ((23560 81) (23452 85)))
					))
					("Philosophy" (
						("Korman" ((54689 90) (23452 85)))
					))
                 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 2 - Checks ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coursesAvg-check (list1)  ;; list1 = list of course with list of teachers (and students-grades)
	(or
		(null list1)
		(and
			(listp list1)
			(lambdas-listp (first list1) (list #'stringp #'listp))
			(coursesAvg-check-in (second (first list1)))
			(coursesAvg-check (rest list1))
		)
	)
)

(defun coursesAvg-check-in (in)  ;; in = list of teachers (and students-grades)
	(or
		(null in)
		(and
			(listp in)
			(lambdas-listp (first in) (list #'stringp #'listp))
			(every #'(lambda (x) (lambdas-listp x (list #'numberp #'gradep))) (second (first in)))
			(coursesAvg-check-in (rest in))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Branch 2 - Program ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun coursesAvg (list1)
(if (coursesAvg-Check list1) 
	(mapcar 
		#'(lambda (x) ;; x = record of list1. for example ("English" (list of student-grade pairs))
			(list
				(first x) ;; course name
				(let (( student-grade-list  (reduce #'append (mapcar #'second (second x)))  ))
					(append 
						(mapcar #'first student-grade-list)
						(list (average (mapcar #'second student-grade-list)))
					)
				)
			)
		)
		list1
	)
"ERROR INPUT")
)

(coursesAvg courses)


