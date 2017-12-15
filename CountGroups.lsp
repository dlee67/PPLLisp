;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  trimGroup function: When CountGroup function receives a list that has a duplicate
;;  on the very first of the list, and the second to first element of the list,
;;  then the given list get's trimmed until there aren't any more duplicates. 
;;  When there is no duplicates to trim in the list, then the list is returned as it is.
;;  parameters:
;;       lst – a list of duplicates on the first element of the list, and the second to
;;       first element of the list.
;;  assumptions:
;;       1. The duplicate elements are always adjacent to each other.
;;       2. There are no nested list in the passed in list for this function.
;;       3. The function will return the cdr of the passed in lst, even if it does not
;;       have the duplicates, for the function was intended to be used with the count-groups.
(setf testOne '(1 1 7 7 7 2 2))
(setf testTwo '(car 1 2 3 cat dog))
(setf testThree '(car car car 2 3 dog dog cat))
(setf testFour '(car car car 2 2 3 3 dog dog programmer))
(defun trimGroup (trimLst)
	(cond
		((not (eq (car trimLst) (car (cdr trimLst)))) (cdr trimLst))
		(t (trimGroup (cdr trimLst)))
	)
)	
;;  test plan for trimGroup:
;;  category / description		   data	                  	  expected result
;;  -------------------------------------------------------------------------------------------------------
;;1.)A list that is entirely     (1 1 7 7 7 2 2)				(7 7 7 2 2)
;; consisted of integers
;;2.)A list has some words
;; but not as a group          (car 1 2 3 cat dog)          (1 2 3 cat dog)
;;3.)A list that has duplicate
;; words.                      (car car car 2 3 dog dog cat)            (2 3 dog dog cat)

(princ "Testing first test case for trimGroup: ")(princ (trimGroup testOne))
(terpri)
(princ "Testing second test case for trimGroup: " )(princ (trimGroup testTwo))
(terpri)
(princ "Testing third test case for trimGroup: ")(princ (trimGroup testThree))
(terpri)
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  count-groups function - Returns the count of groups with duplicates.
;;  parameters:
;;       lst – a list of duplicates in groups.
;;  assumptions:
;;		1. The lst will never have any nested list.
(defun count-groups (lst)
	(cond
		((not lst) 0)
		((equal (car lst) (car (cdr lst))) (+ 1 (count-groups (trimGroup lst))))
		(t (count-groups (cdr lst)))
	)
)
;;  test plan for count_groups:
;;  category / description		   data	                  	  			expected result
;;  -------------------------------------------------------------------------------------------------------
;;1.)A list that is entirely
;;consisted of integers			(1 1 7 7 7 2 2)				   				3
;;2.)A list that has some
;; words, but not as a group.  (car 1 2 3 cat dog)             				0
;;3.)A list that has duplicate
;;words                        (car car car 2 3 dog dog cat)   				2
;;4.)A list that has duplicate 
;;words, and integers          (car car car 2 2 3 3 dog dog programmer)     4
;;5.)Test case from the HW.		'(a b c)									0
;;6.)Test case from the HW.		'(here kitty kitty)							1
;;7.)Test case from the HW.		'(happy happy joy joy)						2
;;8.)Test case from the HW.		'(yes no no 23 -101 yes yes yes)			2
;;9.)Test case from the HW.		'(7 7 7 7 7 7)								1
(princ "Testing first test case: ") (princ (count-groups testOne))
(terpri)
(princ "Testing second test case: ") (princ (count-groups testTwo))
(terpri)
(princ "Testing third test case: ") (princ (count-groups testThree))
(terpri)
(princ "Testing fourth test case: ") (princ (count-groups testFour))
(terpri)
(princ "Testing fifth test case: ") (princ (count-groups '(a b c)))
(terpri)
(princ "Testing sixth test case: ") (princ (count-groups '(here kitty kitty)))
(terpri)
(princ "Testing seventh test case: ") (princ (count-groups '(happy happy joy joy)))
(terpri)
(princ "Testing eigth test case: ") (princ (count-groups '(yes no no 23 -101 yes yes yes)))
(terpri)
(princ "Testing ninth test case: ") (princ (count-groups '(7 7 7 7 7 7)))