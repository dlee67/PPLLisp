;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;	twin:	
;;		Twin receives a list and duplicates all the elements in the list.
;;  parameters:
;;       lst – a list of symbols and number, or composition of single categorizable elements.
;;  assumptions:
;;       1. the list is not nested.
(defun twin (lst)
	(cond 
		((not lst) ())
		(t (cons (car lst) (cons (car lst) (twin (cdr lst)))))
	)
)
;;  test plan for twin:
;;  category / description					data					  expected result
;;-----------------------------------------------------------------------------------------------------------------
;;1.)A list that is entirely
;;composed of numbers.					(1 2 3)						(1 1 2 2 3 3)
;;2.)A list that is entirely
;;composed of characters.			(stones arrows)					(stones stones arrows arrows)
;;3.)A list that is composed
;;some words and some integers.		(1 2 programmer manager)		(1 1 2 2 programmer programmer manager manager)
;;4.)Test case from the
;;HW assig example.					(dog 2 cat)						(dog dog 2 2 cat cat 7 7)
;;5.)Test case from the
;;HW assig example.					(3 3 4)							(3 3 3 3 4 4)
(princ "Starting the test cases for the twin function")
(terpri)
(princ "Test case one: ")(princ (twin '(1 2 3)))
(terpri)
(princ "Test case two: ")(princ (twin '(stones arrows)))
(terpri)
(princ "Test case three: ")(princ (twin '(1 2 programmer manager))) 
(terpri)
(princ "Test case four: ") (princ (twin '(dog 2 cat)))
(terpri)
(princ "Test case five: ") (princ (twin '(3 3 4)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;	untwin:	
;;		Untwin receives the list definable by the definition below, and gets rid of the duplicates.
;;  parameters:
;;       lst – a list of symbols and number, or composition of single categorizable elements.
;;  assumptions:
;;       1. the list is not nested.
;; 		 2. No more than two identical elements will be adjacent.
(defun untwin (lst)
	(cond 
		((not lst) ())
		((eq (car lst) (car (cdr lst))) (cons (car lst) (unTwin (cdr (cdr lst)))))
		(t (cons (car lst) (untwin (cdr lst))))
	)
)
;;  test plan for untwin:
;;  category / description					data					  				expected result
;;-----------------------------------------------------------------------------------------------------------------
;;1.)A list that is entirely
;;composed of numbers.						(1 1 2 2 3 3)			  				(1 2 3)
;;2.)A list that is entirely
;;composed of characters.			(stones stones arrows arrows)					(stones arrows)					
;;3.)A list that is composed
;;some words and some integers.		(1 1 2 2 programmer programmer manager manager)	(1 2 programmer manager)
;;4.)Test case from the
;;HW assignment.					(dog dog 2 cat 7 7)								(dog 2 cat 7)
;;5.)Test case from the
;;HW assignment.					(flower flower bee flower)						(flower bee flower)
(princ "Starting the test cases for the untwin function")
(terpri)
(princ "Test case one: ")(princ (untwin '(1 1 2 2 3 3)))
(terpri)
(princ "Test case two: ")(princ (untwin '(stones stones arrows arrows)))
(terpri)
(princ "Test case three: ")(princ (untwin '(1 1 2 2 programmer programmer manager manager))) 
(terpri)
(princ "Test case four: ")(princ (untwin '(dog dog 2 cat 7 7))) 
(terpri)
(princ "Test case five: ")(princ (untwin '(flower flower bee flower)))