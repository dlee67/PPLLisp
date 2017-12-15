;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  forNested:
;;		Returns the count of numbers those are same as the passed in number
;;  parameters:
;;      number - A number that will be used for counting duplicate. 
;;		nLst - A list of integers.
;;  assumptions:
;;		1.)The incoming lists will not contain words.
(defun forNested (number nLst)
	(cond
		((not (car nLst)) 0)
		((listp (car nLst)) (+ (forNested number (car nLst)) (forNested number (cdr nLst)))) 
		((eq number (car nLst)) (+ 1 (forNested number (cdr nLst))))
		(t (forNested number (cdr nLst)))
	)
)
;;  test plan for forNested:
;;  category / description			data							  expected result
;;-----------------------------------------------------------------------------------------------------------------
;;ALL TEST CASES BELOW ARE
;;ASSUMED TO HAVE COMPARABLE
;;OF 5.	
;;*A list that has no
;;integers that matches the			(1 2 3 4)							0
;;comparable.
;;*A list that has only one 		(5 1 2 3)							1
;;match, in the very beginning.
;;*A list that has only one match	(1 2 5 4)							1
;;in the middle of the list.				
;;*A list that has 3 matches.		(5 1 5 5)							3
;;*A list that has all the 
;;elements that matches.			(5 5 5 5)							4
(princ "Starting test cases for the forNested function.")
(terpri)
(princ "Test case one: ")(princ (forNested 5 '(1 2 3 4)))
(terpri)
(princ "Test case two: ")(princ (forNested 5 '(5 1 2 3)))
(terpri)
(princ "Test case three: ")(princ (forNested 5 '(1 2 5 4)))
(terpri)
(princ "Test case four: ")(princ (forNested 5 '(5 1 5 5)))
(terpri)
(princ "Test case Five: ")(princ (forNested 5 '(5 5 5 5)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  match:
;;		A function that returns the amount of specified duplicates in the given list.
;;  parameters:
;;		number - A number that will be used for counting duplicate.
;;		lst - A list of numbers.
;;  assumptions:
;; 		1.) The incoming lists will not contain any words.
(defun match (number lst)
	(cond
		((not lst) 0)
		((listp (car lst)) (+ (forNested number (car lst)) (match number (cdr lst))))
		((eq number (car lst)) (+ 1 (match number (cdr lst))))
		(t (match number (cdr lst)))
	)
)
;;  test plan for match:
;;  category / description			data							  expected result
;;-----------------------------------------------------------------------------------------------------------------
;;ALL TEST CASES BELOW ARE
;;ASSUMED TO HAVE COMPARABLE
;;OF 5, UNTIL CASE 6.
;;ALL TEST CASES BELOW HAVE
;;NESTED LIST
;;1.)A list that has no
;;integers that matches the			(1 2 (1 2) 3 4)							0
;;comparable.
;;2.)A list that has only one 		(5 1 (1 2) 2 3)							1
;;match, in the very beginning.
;;3.)A list that has only one match	(1 2 (1 2) 5 4)							1
;;in the middle of the list.				
;;4.)A list that has 3 matches.		(5 1 (5 5))								3
;;5.)A list that has all the 
;;elements that matches.			(5 5 5 (5 5 5) 5)						7
;;6.)This test case was
;;from the HW assig,
;;where 23 is the target			(11 20 5 84)							0
;;number.
;;7.)This test case was
;;from the HW assig,
;;where 101 is the target	(101 101 (88 123) 15 (101) ((17 82 101)(44)))	4(The HW assig states that output should be 3)
;;number.
;;8.)A list with
;;nested nested nested 	 (7 7 7 (((7 7) 7 7)))								7
;;list, with 7 as the
;;target number.	
(princ "Starting test cases for the match function.")
(terpri)
(princ "Test case one: ")(princ (match 5 '(1 2 (1 2) 3 4)))
(terpri)
(princ "Test case two: ")(princ (match 5 '(5 1 (1 2) 2 3)))
(terpri)
(princ "Test case three: ")(princ (match 5 '(1 2 (1 2) 5 4)))
(terpri)
(princ "Test case four: ")(princ (match 5 '(5 1 (5 5))))
(terpri)
(princ "Test case Five: ")(princ (match 5 '(5 5 5 (5 5 5) 5)))
(terpri)
(princ "Test case six: ") (princ (match 23 '(11 20 5 84)))
(terpri)
(princ "Test case seven: ") (princ (match 101 '(101 101 (88 123) 15 (101) ((17 82 101)(44)))))
(terpri)
(princ "Test case eight: ") (princ (match 7 '(7 7 7 (((7 7) 7 7)))))