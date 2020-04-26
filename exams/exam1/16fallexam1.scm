; CSc 335
; Fall 2016

; September 29

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 15 points)

;;;; Problem 2a - code (max 15 points)
;;;; Problem 2b - proof (max 20 points)

;;;; Problem 3 - code (max 15 points)
;;;; Problem 3 - proof (max 20 points)

;;;; Problem 4 - code (max 15 points)



;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the R5RS
; implementation provided by drracket and only those language features discussed so far in the
; context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  (15 points) Recursive patterns lurk everywhere.  Use the equation (n + 1)^2 = n^2 + 2n + 1 to develop a
; recursive program for computing n^2.  Explain the connection between the equation and your program. 

; The strategy is to compute (n + 1)^2 by computing n^2 and then adding 2n + 1 to it.
; Or, analogously: compute n^2 by first computing (n - 1)^2 and then adding 2(n - 1) + 1 = 2n - 1.

(define (square n)
  (cond ((= n 0) 0)
        (else (+ (* 2 n) -1 (square (- n 1))))))

; The precondition is that n is a nonnegative integer. We could easily extend it so that n can be negative:
; given that the square of a negative number is the square of its absolute value, if n < 0, we just call (square (- n)).

(define (square2 n)
  (cond ((= n 0) 0)
        ((< n 0) (square2 (- n)))
        (else (+ (* 2 n) -1 (square2 (- n 1))))))

(display "Problem 1 testing\n")

(square2 5) ; 25
(= (square2 -5) (square2 5)) ; #t

; The postcondition of both procedures is that the value of n^2 is returned.

; Clearly, square as well as square2 returns the right answer in the base case; n = 0, so n^2 = 0.

; If (n-1)^2 is correctly computed then so is n^2 because the function is modeled after the equation.
; By induction, the procedure is correct for all integers n >= 0 (square2 is correct for all negative integers as well.)
;  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; 2a.  (15 points) Write a properly recursive Scheme function num-digits-satisfying which inputs an integer n and a boolean function
; test? of one argument and which returns the number of digits in n for which (test? n) is true.  Thus the call
; (num-digits-satisfying 152535 (lambda (n) (= n 5))) should return 3, and (num-digits-satisfying -152535 odd?) would 
; return 5.

; Idea:
; 1. Extract the ones digit from the number.
; 2. Process it (add 1 to the running count if it passes the given test or 0 if it doesn't).
; 3. Repeat steps 1-2 until all digits have been processed.

(define (num-digits-satisfying n test?)
  (let ((ones-digit (remainder n 10)))
    (define boolean (if (test? ones-digit)
                        1
                        0))
    (cond ((and (< n 10) (> n -10)) boolean)
          (else (+ boolean
                   (num-digits-satisfying (quotient n 10) test?))))))

; Precondition: n is an integer and test? is a boolean function of one numerical argument.
; Postcondition: the number of digits in n for which test? is true is returned.

(display "\nProblem 2 testing\n")

(num-digits-satisfying 152535 (lambda (n) (= n 5))) ; 3
(num-digits-satisfying -152535 odd?) ; 5
(num-digits-satisfying 0 odd?) ; 0
(num-digits-satisfying 1 odd?) ; 1

; 2b.  (20 points) Give a complete proof showing that your function num-digits-satisfying is correct.

; In the base case, n is a one-digit number (between -9 and 9). Thus, we can just return boolean, which is
; defined as 1 if the ones-digit of n passed the test or 0 if it didn't.
; The count of the digits in n that passed the test is precisely that, since there's only
; one digit in n to consider.

; In the inductive case, n has more than one digit. In this case we can process the ones-digit
; and then process the rest of the number and add the two results.
 
; We want to show that if calling the function on the rest of the number (i.e., (quotient n 10)) returns
; the correct result, then the result of adding boolean to it (adding 1 if the ones-digit satisfies our test or 0 if it doesn't)
; returns the correct result for the whole number, in the outer call.

; Obviously, if calling the function on the number with the ones digit is removed returns the actual number of digits
; for which test? is true, adding 1 to it iff test? is true for the ones digit gives us a correct number.
; It makes no contribution to the total count if it does not pass the test, whereas it
; gets included in the count it does pass it. If the procedure correctly evaluates all k-digit numbers then it will
; evaluate all (k+1)-digit numbers correctly, since evaluation of a (k+1)-digit number depends on the correct evaluation of a
; k-digit number.

; Because n is an integer and every call removes the ones digit from n, we know the number of digits left to process
; will eventually be reduced to 1 and we will enter the base case whether n is positive or negative and the
; program will terminate.

; By induction on the length of n, then, we can conclude that num-digits-satisfying works on all integer inputs.
; ----------------------------------------------------------------------------------------------------------

; 3a.  (15 points) Now rewrite num-digits-satisfying so that it generates an iterative process.

; We follow the same process of processing each digit one at a time and adding 1 to the "count" if test? is true
; and 0 if it isn't. In this case, we keep count in a state variable initialized to 0.

(define (num-digits-satisfying n test?)
  (define (iter a count)
    (let ((ones-digit (remainder a 10)))
      (define ones-digit-tested (if (test? ones-digit)
                                    1
                                    0))
    (cond ((and (< a 10) (> a -10)) (+ ones-digit-tested count))
          (else (iter (quotient a 10) (+ ones-digit-tested count))))))
  (iter n 0))

(display "\nProblem 3 testing\n")

(num-digits-satisfying 152535 (lambda (n) (= n 5))) ; 3
(num-digits-satisfying -152535 odd?) ; 5

; 3b.  (20 points) Give a complete (invariant-based) proof showing that your rewritten num-digits-satisfying is correct. 

; Note that (remainder a 10) returns the ones digit of a, and (quotient a 10) returns a with its ones digit removed.

; The invariant is that count is equal to the number of digits in the processed part of the number (the
; complement of a, which is the processed part)

; On the initial call, a = n, so the unprocessed part is the entire number <=> the processed part is empty.
; Thus the count is zero; no digits have been tested yet.

; On each call, we process the ones digit: if it passes the test then we add 1 to the count; otherwise we add nothing.

; If in the previous call count was equal to the actual number of digits -- previous to this ones-digit -- that passed
; the test, the processing of the current ones-digit leads to the correct count again: it gets included (1 is added)
; or it doesn't (0 is added).

; Does the invariant imply correctness of the program on termination? The program terminates when a is a one-digit number.
; Thus, we have one digit left to process, and the result of that test should be added to count.
; This is reflected in the return value (+ ones-digit-tested count).

;  ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; 4. (15 points) Write a function make-repeated which inputs a function f of one argument and which returns a function
; g of two arguments b and n so that ((make-repeated f) b n) applies f n times to b.  Demonstrate your function by 
; showing how to use it to apply the function square 8 times to 2.


(define (compose f g)
  (lambda (x) (f (g x)))) ; the function of x that applies f to g(x)

(define (make-repeated f)
  (lambda (b n)
    (cond ((= n 1) (f b))
          (else (f ((make-repeated f) b (- n 1)))))))

(display "\nProblem 4 testing\n")

(define (square x) (* x x))
((make-repeated square) 2 1) ; 4
((make-repeated square) 2 2) ; 16
((make-repeated square) 2 3) ; 256
((make-repeated square) 2 8) ; (((((((2^2)^2)^2)^2)^2)^2)^2)^2

