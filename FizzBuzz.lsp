;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  fizzBuzz function: Outputs the list of integers from one through twenty; however, when number
;; 	is evenly divisible by 3, "fizz" is attached with it, and produced as a substring, and when the
;;  number is evenly divisible by 5, "buzz" is attached with it, and produced as a substring.
;; 	Finally, a list produced that contains substrings being mentioned above sentence.
;;  parameters:
;;       lst – a list of numbers on through twenty, without nested list.
;;  assumptions:
;;       1. no nested lists
;;       2. all list elements are integers
;;       3. No integers are less than one, or greater than 20.
(setf oneToTwenty '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(defun fizzBuzz (lst)
	(cond 
		((not lst) ())
		(
			(and 
				(= (mod (car lst) 3) 0)
				(= (mod (car lst) 5) 0)
			)
			(cons (append (list (car lst)) (append (list "fizz") (list "buzz"))) (fizzBuzz (cdr lst)))
		)
		((= (mod (car lst) 3) 0) (cons (append (list (car lst)) (list "fizz")) (fizzBuzz (cdr lst))))
		((= (mod (car lst) 5) 0) (cons (append (list (car lst)) (list "buzz")) (fizzBuzz (cdr lst))))
		(t (cons (car lst) (fizzBuzz (cdr lst))))
	)
)
;;  test plan for fizzBuzz:
;;  category / description		data		  expected result
;;  ----------------------------------------------------------------------------------------------------
;; list of One through twenty,  (1 2 ... 20)  (1 2 (3 fizz) 4 (5 buzz) (6 fizz) 7 8 (9 fizz) (10 buzz) 11 (12 fizz) 13 14 (15 fizz buzz) 16 17 (18 fizz) 19 (20 buzz))
;; where all the elements are
;; integers.
(terpri)
(princ "Starting test cases for the fizzBuzz: ")
(terpri)
(princ "Testing '(1 2 ... 20): ") (princ (fizzBuzz oneToTwenty))
;;  Dong(Bob) Lee – CS 3210 – Fall 2017
;;  ============================
;;  fizz-buzz function: 
;;		A function that calls the fizzBuzz in order to immitate the representation in the example of the problem number 1.	
;;  parameters:
;;       lst – a list of numbers on through twenty, without nested list.
;;  assumptions:
;;       1. no nested lists
;;       2. all list elements are integers
;;       3. No integers are less than one, or greater than 20.
(defun fizz-buzz ()
	(fizzBuzz oneToTwenty)
)
(fizz-buzz)
;;  test plan for fizz-buzz:
;;  category / description		data		  expected result
;;  ----------------------------------------------------------------------------------------------------
;; list of One through twenty,  (1 2 ... 20)  (1 2 (3 fizz) 4 (5 buzz) (6 fizz) 7 8 (9 fizz) (10 buzz) 11 (12 fizz) 13 14 (15 fizz buzz) 16 17 (18 fizz) 19 (20 buzz))
;; where all the elements are
;; integers.