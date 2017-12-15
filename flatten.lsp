;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  flatten function: 
;;		Outputs the list of integers without any nested sections.
;;  parameters: 
;;		lst - A nested list.
;;  assumptions:
;;		1.) The list doesn't always has to be composed of numbers.
(defun flatten (lst)
	(cond
		((not lst) ())
		((listp (car lst)) (append (car lst) (flatten (cdr lst))))
		(t (cons (car lst) (flatten (cdr lst))))
	)
)
;;  test plan for flatten:
;;  category / description		data		 		 		expected result
;;----------------------------------------------------------------------------------------------------
;;*A list with nested list
;;with integers only			(2 1 (1 2))						(2 1 1 2)
;;*A list with nested list
;;with symbols only 			((duck duck) goose)				(duck duck goose)
;;*A list composed of
;;words and numbers,
;;and some could be nested.		(its easy as (1 2 3)) 			(its easy as 1 2 3)
;;*A test case from HW.			()								() (The promp will return nil, but nil is equivalent as the empty list)
;;*An example from the HW.		'(day night)					(day night)
;;*An example from the HW.	'(a b (high low)()(e (f (deep) h))	(a b high low e f deep h)
(princ (flatten '(2 1(1 2))))
(terpri)
(princ (flatten '((duck duck) goose)))
(terpri) 
(princ (flatten '(its easy as (1 2 3))))
(terpri) (princ (flatten '()))
(terpri) (princ (flatten '(day night)))
(terpri) (princ (flatten '(a b (high low)()(e (f (deep) h)))))