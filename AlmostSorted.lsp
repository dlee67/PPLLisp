;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;	countInversion:
;;		The function will count the occurances of inversions in pair, in ascending order.
;;  parameters:
;; 		lst - A list of integers.
;;  assumptions:
;;		1.) There will be no duplicates in the list.
;;		2.) There will be no nested list.
(setf someList '())
(defun countInversion (lst)
	(cond 
		((eq (cdr lst) nil) 0)
		((> (car lst) (car (cdr lst))) (+ 1 (countInversion (cdr lst))))
		((listp (car lst)) (+ (counterInversion (car lst)) (counterInver (cdr lst))))
		(t (countInversion (cdr lst)))
	)
)

;;
;;  test plan for countInversion:
;;  category / description				data		  expected result
;;----------------------------------------------------------------------------------------------------------------------
;;1.)An empty list.						()				0	
;;2.)A list with no inverted
;;pairs								(1 2 3 4)			0
;;3.)A list with only one
;;inverted pair.					(2 1 3 4)			1
;;4.)A list that has no 
;;sorted pairs.                     (4 3 2 1)           3
;;5.)A list that has some		
;;negative numbers.					(1 -1 2 3)   		1
;;6.)Test case from the 
;;HW assignment.					'(4)				0
;;7.)Test case from the 
;;HW assignment.				'(7 8 9 10)				0
;;8.)Test case from the 
;;HW assignment.				'(1 2 3 5 4 6 7 8)		1
;;9.)Test case from the
;;HW assignment.				'(5 4 3 2 1)			4
(princ "Starting test cases for the countInversion")
(terpri)
(princ "Testing case one: ")(princ (countInversion '()))
(terpri)
(princ "Testin case two: ")(princ (countInversion '(1 2 3 4)))
(terpri)
(princ "Testing case three: ")(princ (countInversion '(2 1 3 4)))
(terpri)
(princ "Testing case four: ")(princ (countInversion '(4 3 2 1)))
(terpri)
(princ "Testing case five: ")(princ (countInversion '(1 -1 2 3)))
(terpri)
(princ "Testing case six: ")(princ (countInversion '(4)))
(terpri)
(princ "Testing case seven: ")(princ (countInversion '(7 8 9 10)))
(terpri)
(princ "Testing case eight: ")(princ (countInversion '(1 2 3 5 4 6 7 8)))
(terpri)
(princ "Testing case nine: ")(princ (countInversion '(5 4 3 2 1)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;	isSorted		
;;		Returns true if the list that was passed in was already sorted; also, returns false,
;;		if the list that was passed in was not sorted in ascending order.
;;  parameters:
;; 		lst - A list of integers.
;;  assumptions:
;;		1.) There will be no duplicates in the list.
;;      2.) There will be no nested list.
(defun isSorted (lst)
	(cond 
		((endp (cdr lst)) t)
		((> (car lst) (car (cdr lst))) nil)
		(t (isSorted (cdr lst)))
	)
)
;;  test plan for isSorted:
;;  category / description				data		  						expected result
;;----------------------------------------------------------------------------------------------------------------------
;;*A list that is not sorted 		(1 2 4 3)								nil
;;*A list that is already 
;;sorted.							(1 2 3 4)								T
;;*A list that has some		
;;negative numbers.					(-1 1 2 3)   							T
(princ "Starting test cases for the isSorted")
(terpri)
(princ "Testing case one: ")(princ (isSorted '(1 2 4 3)))
(terpri)
(princ "Testing case two: ")(princ (isSorted '(1 2 3 4)))
(terpri)
(princ "Testin case three: ")(princ (isSorted '(-1 1 2 3)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;	isAlmostSorted:
;; 		Let 0.2n, where n is the number of elements in the list, be the "invertededness" of the list.
;;		When the "invertededness" of the list is is < 0.2n, the list is almost sorted.
;;		The function is-almost-sorted returns the boolean status of the sortedness.
;;		This function will return t or nil, depending if the list is sorted or not.
;;  parameters:
;; 		lst - A list of integers.
;;  assumptions:
;;		1.) There will be no duplicates in the list.
;;		2.) There will be no nested list.
(defun is-almost-sorted (lst)
	(cond	
		((isSorted lst) nil)
		((< (countInversion lst) (* 0.2 (length lst))) t)
		((> (countInversion lst) (* 0.2 (length lst))) nil)
	)
)
(princ "Starting test cases for the is-almost-sorted")
(terpri)
(princ "Testing case one: ")(princ (is-almost-sorted '()))
(terpri)
(princ "Testing case two: ")(princ (is-almost-sorted '(1 2 3 4)))
(terpri)
(princ "Testing case three: ")(princ (is-almost-sorted '(-2 1 2 3 4)))
(terpri)
(princ "Testing case four: ")(princ (is-almost-sorted '(1 2 -3 4 5)))
(terpri)
(princ "Testing case five: ")(princ (is-almost-sorted '(11 22 33 44 55)))
(terpri)
(princ "Testing case six:")(princ (is-almost-sorted '(1 2 4 3)))
(terpri)
(princ "Testing case seven: ")(princ (is-almost-sorted '(1 2 4 3 5)))
(terpri)
(princ "Testing case eight: ")(princ (is-almost-sorted '(1 2 4 3 5 6)))
(terpri)
(princ "Testing case nine: ")(princ (is-almost-sorted '(1 2 7 4 9 1 13 15 19 20 21 22)))
(terpri)
(princ "Testin case ten: ")(princ (is-almost-sorted '(1 -7 3 4 5 17 16 0 22 33)))
(terpri)
;;  test plan for is-almost-sorted:
;;  category / description				data		  						expected result
;;----------------------------------------------------------------------------------------------------------------------
;;1.)An empty list.						()									nil (because it's already sorted)
;;2.)A list with no inverted
;;pairs								(1 2 3 4)								nil 
;;3.)A list with 
;;some negative numbers.			(-2 1 2 3 4)							t
;;4.)A list with some
;;negative numbers,					(1 2 -3 4 5)							nil
;;where the list
;;is not sorted.
;;5.)Test case from the 
;;HW assignment. 					(11 22 33 44 55)						nil
;;6.)Test case from the
;;HW assignment. 					(1 2 4 3)								nil
;;7.)Test case from the
;;HW assignment.					(1 2 4 3 5)								nil
;;8.)Test case from the
;;HW assignment.					(1 2 4 3 5 6)							t
;;9.)Test case from the
;;HW assignment.			(1 2 7 4 9 1 13 15 19 20 21 22)					t
;;10.)Test case from the
;;HW assignment. 			(1 -7 3 4 5 17 16 0 22 33)						nil