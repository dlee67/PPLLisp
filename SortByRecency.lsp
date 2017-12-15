;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  isThere function:
;;		Constructs a new list where word(explained below) is always the front of the list, and if there was word
;;	 	in the lst(explained below), then it is removed.
;;  parameters:
;;		 word - a subject of comparable.
;;       lst – a list elements those are composed of words.
;;  assumptions:
;;		1.) There are no duplicants.
(defun isThere (word lst)
	(cond
		((not lst) ())
		((equal word (car lst)) (cdr lst))
		(t (cons (car lst) (isThere word (cdr lst))))
	)
)
;;  test plan for isThere:
;;  category / description		data			 	 expected result
;;----------------------------------------------------------------------------------------------------
;;*A list with only the
;;numbers.					(1 2 3)				  	(1 3)
;;*A list with only the
;;symbols.					(thinker are you)		(are you)
;;*A list with number and	
;;symbols.					(No 39 thinker)			(thinker)
(princ "Starting test cases for isThere function: ")
(terpri)
(princ "Test case one: ")(princ (isThere 2 '(1 2 3)))
(terpri)
(princ "Test case two: ")(princ (isThere 'thinker '(thinker are you)))
(terpri)
(princ "Test case four: ")(princ (isThere 39 '(No 39 thinker)))
(terpri)

;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  make-recent function:
;;		Outputs a list that has the duplicate element as the very first element of the list. 
;;  parameters:
;;		 word - a subject of comparable.
;;       lst – a list elements those are composed of words.
;;  assumptions:
;;		1.) The incoming list has now duplicates.
;;		2.) The incoming list has no nested list.

(defun make-recent (word lst)
	(cons word (isThere word lst))
)
;;  test plan for make-recent:
;;  category / description		data			 	 expected result
;;----------------------------------------------------------------------------------------------------
;;1.)A list with only the
;;numbers, with 2 as the	  (1 2 3)				  	(2 1 3)
;;target word.. 			  
;;2.)A list with only the
;;symbols, with	thinker as 	(thinker are you)		(thinker are you)
;;the target word.	
;;3.)A list with number and	
;;symbols, with 39 as 		(No 39 thinker)			(39 thinker No)
;;the target word.					
;;4.)A test case from the
;;HW assig, with cat as 	(a cat came in)			(cat a came in)
;;the target word.		
;;5.)A test case from the
;;HW assig, with boy as 	(cat dog tree) 			(boy cat dog tree)
;;the target word.		
(princ "Starting test cases for make-recent function: ")
(terpri)
(princ "Test case one: ")(princ (make-recent 2 '(1 2 3)))
(terpri)
(princ "Test case two: ")(princ (make-recent 'thinker '(thinker are you)))
(terpri)
(princ "Test case three: ")(princ (make-recent 39 '(No 39 thinker)))
(terpri)
(princ "Test case four: ")(princ (make-recent 'cat '(a cat came in)))
(terpri)
(princ "Test case five: ")(princ (make-recent 'boy '(cat dog tree)))