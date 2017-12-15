;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  forNested function: Goes through the list, and returns the list
;;  with all the values less than the specified limit.
;;  This function is intended to be used only with the enforce-limit; thus,
;;  when called from outside of the enforce-limit, the function might return
;;  an unintended result.
;;  parameters:
;;      nLst – a list of limits with mix of words, and limits.
;;      limit -  A number that every element(when it's an integer value)
;;      must be less, or equal to.
;;  assumptions:
;;       1. no nested lists, the function is made to deal with the nested list.
(setf testOne '(1 2 1 2 3))
(setf testTwo '(1 6 2 4 7 8))
(setf testThree '(7 8 9 10 6))
(setf testFour '(6 2 num 7 1))
(defun forNested (limit nLst)
	(cond
		((not (car nLst)) ())
		((not (numberp (car nLst))) (cons (car nLst) (forNested limit (cdr nLst))))
		((<= limit (car nLst)) (cons limit (forNested limit (cdr nLst))))
		(t (cons (car nLst) (forNested limit (cdr nLst))))
	)
)
;;  test plan for forNested:
;;  category / description		  data		            expected result
;;----------------------------------------------------------------------------------------------------------------------------
;; ALL TEST CASES BELOW ASSUMES
;; THAT THE LIMIT IS 5.
;;1.)A list that is composed of
;;integers, those are 
;;all less than the limit.       (1 2 1 2 3)           (1 2 1 2 3)
;;2.)A list that is composed of
;;some integers those are
;;bigger than the limit.         (1 6 2 4 7 8)         (1 5 2 4 5 5)
;;3.)A list that has all the 
;;elements bigger than
;;the specified limit.           (7 8 9 10 6)          (5 5 5 5 5)
;;4.)A list that has some
;;words, and numbers those
;;are bigger than the limits     (6 2 num 7 1)         (5 2 num 5 1)
;;and sometimes smaller.
(princ "Testing case 1: ")(princ (forNested 5 testOne))
(terpri)
(princ "Testing case 2: ")(princ (forNested 5 testTwo))
(terpri)
(princ "Testing case 3: ")(princ (forNested 5 testThree))
(terpri)
(princ "Testing case 4: ")(princ (forNested 5 testFour))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  enforce-limit function: Regardless if the list being passed in is nested or not,
;;  every integer values beigger than the limit becomes the limit. When scanning over the
;;  symbolic value, that value will remain and be inclueded in the final list.
;;
;;  parameters:
;;		outterLimit - A limit that is being specified, and every element in the list being passed to the
;;		enforce-limit must be lessthan, or equal to outterLimit.
;;  assumptions:
;;  	1.) The limit will always be an integer.
(setf testOne '(1 2 1 (1 2 3) 2 3))
(setf testTwo '(1 6 2 (2 7 1) 4 7 8))
(setf testThree '(7 8 9 (8 9 10) 10 6))
(setf testFour '(6 2 (7 num 7) num 7 1))
(defun 	enforce-limit (outterLimit lst)
	(cond
		((not lst) ())
		;((not (numberp (car lst))) (cons (car lst) (enforce-limit outterLimit (cdr lst))))
		((listp (car lst)) (cons (forNested outterLimit (car lst)) (enforce-limit outterLimit (cdr lst))))
		((not (numberp (car lst))) (cons (car lst) (enforce-limit outterLimit (cdr lst))))
		((> (car lst) outterLimit) (cons outterLimit (enforce-limit outterLimit (cdr lst))))
		(t (cons (car lst) (enforce-limit outterLimit (cdr lst))))
	)
)
;;  test plan for enforce-limit:
;;  category / description		  data		            expected result
;;----------------------------------------------------------------------------------------------------------------------------
;; ALL TEST CASES BELOW ASSUMES
;; THAT THE LIMIT IS 5, UNTIL CASE 6.
;; ALL THE TEST CASES BELOW ASSUMES
;; THAT ALL THE LIST BEING EVALUTED
;; HAVE AT LEAST ONE NESTED LIST
;; ELEMENT, EXCEPT CASE 5 AND 6.
;;1.)A list that is composed of
;;integers, those are 
;;all less than the limit.       (1 2 1 (1 2 3) 2 3)           (1 2 1 (1 2 3) 2 3)
;;2.)A list that is composed of
;;some integers those are
;;bigger than the limit.         (1 6 2 (2 7 1) 4 7 8)         (1 5 2 (2 5 1) 4 5 5)
;;3.)A list that has all the 
;;elements bigger than
;;the specified limit.           (7 8 9 (8 9 10) 10 6)          (5 5 5 5 5)
;;4.)A list that has some
;;words, and numbers those
;;are bigger than the limits     (6 2 (7 num 7) num 7 1)         (5 2 (5 num 5) num 5 1)
;;5.) A test case from the
;;HW, with 5 at the bound.		(6 2 kitty 5 -16)				(5 2 kitty 5 -6)
;;6.) A test case from the
;;HW, with 8 as the bound.		(1 66 2 kitty -16)				(1 8 2 kitty -16)
;;7.) A test case from the
;;HW, with 33 as the bound.		(20 (35 9) 7 100 2 () 2)		(20 (33 9) 7 33 2 () 2)		
(princ "Testing case 1 of enforce-upper: ")(princ (enforce-limit 5 testOne))
(terpri)
(princ "Testing case 2 of enforce-upper: ")(princ (enforce-limit 5 testTwo))
(terpri)
(princ "Testing case 3 of enforce-upper: ")(princ (enforce-limit 5 testThree))
(terpri)
(princ "Testing case 4 of enforce-upper: ")(princ (enforce-limit 5 testFour))
(terpri)
(princ "Testing case 5 of enforce-upper: ")(princ (enforce-limit 5 '(6 2 kitty 5 (- 16))))
(terpri)
(princ "Testing case 6 of enforce-upper: ")(princ (enforce-limit 8 '(1 66 2 kitty (- 16))))
(terpri)
(princ "Testing case 7 of enforce-upper: ")(princ (enforce-limit 33 '(20 (35 9) 7 100 2 () 2)))