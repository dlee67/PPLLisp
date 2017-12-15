;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  checkThird function:
;;;		Third portion of the mutual exclusion chain beginning from the checker function.
;;  parameters:		
;;		lst - A list that is being passed in from the checkMiddle function.
;;  assumptions:
;;		1.) This function will always be called from the checkMiddle function.
(defun checkThird (lst)
	(cond
		((null lst) nil)
		((not (listp lst)) nil)
		((numberp (car lst)) t)
		((listp (car lst)) (checker (car lst)))
	)
)
;;  test plan for checkThird:
;;  category / description				data		  			expected result
;;----------------------------------------------------------------------------------------------------
;;*An integer 5.						5							nil (the function needs to take the list).
;;*A 5, as a list.						'(5)						t
;;*A word.								'dog 						nil	
;;*A list.								'()							nil
(princ "Strating test cases for the checkThird: ")
(terpri) (princ "Testing data 5: ") (princ (checkThird 5))
(terpri) (princ "Testing data '(5): ") (princ (checkThird '(5)))
(terpri) (princ "Testing data 'dog: ")(princ (checkThird 'dog))
(terpri) (princ "Testing data '(): ")(princ (checkThird '()))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  checkMiddle function:
;;;		A part of the mutual recursion being called from the checkFirst, which checks the operator.
;;  parameters:		
;;		lst - A list that is being passed in from the checkFirst function.
;;  assumptions:
;;		1.) This function will always be called from the checkFirst function.
(defun checkMiddle (ele lst)
	(cond 
		((not lst) nil)
		((not (listp ele)) nil)
		((equal (car ele) (car lst)) (checkThird (cdr ele)))
		((listp (car lst)) (checker (car lst)))
		(t (checkMiddle ele (cdr lst)))
	)
)
;;  test plan for checkMiddle:
;;  category / description				data		  			expected result
;;----------------------------------------------------------------------------------------------------
;;*A plus symbol.						'(plus)						nil(because the checkThird needs to evaluate it, and '(plus) is not a number.
;;*An empty list. 						'() 						nil
;;*A word.								'dog						nil
(terpri)
(princ "Starting test cases with checkMiddle: ")
(terpri) (princ "Testing data '(plus): ") (princ (checkMiddle '(plus) '(plus minus times dividedby)))
(terpri) (princ "Testing data '(): ") (princ (checkMiddle '() '(plus minus times dividedby)))
(terpri) (princ "Testing data 'dog: ") (princ (checkMiddle 'dog '(plus minus times dividedby)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  checkFirst function:
;;;		First portion of the mutual exclusion chain beginning from the checker function, deals handles the checking of the numeral operand.
;;  parameters:		
;;		lst - A list that is being passed in from the checker function, which is the beginning problem, and also the nested list.
;;  assumptions:
;;		1.) This function will always be called from the checker function.
(defun checkFirst (lst)
	(cond
		((not (listp lst)) nil)
		;The line below should not exist whatsoever, causes too much regression.
		;((not (numberp (car lst))) nil)
		((numberp (car lst)) (checkMiddle (cdr lst) '(plus minus times dividedby)))
		((null lst) nil)
		((listp (car lst)) (checker (car lst)))
	)
)
;;  test plan for checkFirst:
;;  category / description				data		  			expected result
;;----------------------------------------------------------------------------------------------------
;;*A list with number 5.				'(5) 					nil(because the checkMiddle will try to evaluate an empty list.
;;*An empty list. 						'()						nil
;;*A list with only the numbers   		'(1 1 1)				nil(because the checkMiddle will evaluated number, and return nil)
;;*A list with only the symbols. 		'(plus plus plus)		nil
;;*A valid representation of
;;binary operation.						'(1 plus 1)				t
(terpri)(princ "Starting test cases for the checkFirst: ")
(terpri)(princ "Test case with '(5): ") (princ (checkFirst '(5)))
(terpri)(princ "Test case with '(): ") (princ (checkFirst '()))
(terpri)(princ "Test case with '(1 1 1): ")(princ (checkFirst '(1 1 1)))
(terpri)(princ "Test case with '(plus plus plus): ")(princ (checkFirst '(plus plus plus)))
(terpri)(princ "Test case with '(1 plus 1): ")(princ (checkFirst '(1 plus 1)))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  checkFirst function:
;;;		First portion of the mutual exclusion chain beginning from the checker function, deals handles the checking of the numeral operand.
;;  parameters:		
;;		lst - A list that is being passed in from the checker function, which is the beginning problem, and also the nested list.
;;  assumptions:
;;		1.) This function will always be called from the checker function.
(defun checker (lst)
	(checkFirst lst)
)
;;  test plan for checker:
;;  category / description							data				  			expected result
;;-----------------------------------------------------------------------------------------------------------------------------------
;;*A valid binary operation with no nested list.	'(1 plus 1)								t
;;*A valid binary opreation with no nested list		'(2 plus 2)								t
;;*A valid binary operation with nested list in
;;the first elements.								'(((1 plus 1) plus 1) plus 1)			t
;;*A valid binary operation with the
;;nested list in the very end.						'(1 plus (1 plus (1 plus 1)))			t
;;*An empty list.									'()										nil
;;*A test case from the
;;HW assignment.									'(7 plus 11)							t
;;*A test case from the
;;HW assignment.									'(25 minus (17 times 12))				t
;;*A test case from the
;;HW assignment.									'((100 plus 200) minus (17 times 5))	t
;;*A test case from the
;;HW assignment.									'(-4 plus)								nil
;;*A test case from the
;;HW assignment.									'(-22 plus (car minus dog))				nil
;;*A test case from the
;;HW assignment.									'((7 + 3) minus 12)						nil
;;*A test case from the
;;HW assignment.									'(-4 plus (cat minus dog))				nil
(terpri)
(princ "Starting test cases for checker: ")
(terpri) (princ "case '(1 plus 1): ") (princ (checker '(1 plus 1)))
(terpri) (princ "case '(2 plus 2): ")(princ (checker '(2 plus 2)))
(terpri) (princ "case with '(((1 plus 1) plus 1) plus 1): ")(princ (checker '(((1 plus 1) plus 1) plus 1)))
(terpri) (princ "case with '(1 plus (1 plus (1 plus 1))): ") (princ (checker '(1 plus (1 plus (1 plus 1)))))
(terpri) (princ "case with '(): ") (princ (checker '()))
(terpri) (princ "case with '(7 plus 11): ") (princ (checker '(7 plus 11)))
(terpri) (princ "case with '(25 minus (17 times 12)): ") (princ (checker '(25 minus (17 times 12))))
(terpri) (princ "case with '((100 plus 200) minus (17 times 5)): ") (princ (checker '((100 plus 200) minues (17 times 5))))
(terpri) (princ "case with '(-4 plus): ") (princ (checker '(-4 plus)))
(terpri) (princ "case with '(-22 plus (car minus dog)): ") (princ (checker '(-22 plus (car minus dog))))
(terpri) (princ "case with '((7 + 3) minus 12): ") (princ (checker '((7 + 3) minus 12)))
(terpri) (princ "case with '(-4 plus (cat minus dog)): ") (princ (checker '(-4 plus (cat minus dog))))