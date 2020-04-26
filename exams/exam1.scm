; CSc 335
; Fall 2018

; October 11

; First Midterm Exam - 2 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Seyson Chen

; TYPE YOUR FULL EMAIL ADDRESS HERE: schen023@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 15 points)  15
;;;; Problem 1 - proof (out of 5 points)  3
;;;; Problem 1 - synergy: connection between proof and code (out of 5 points)  3

;;;; Problem 2 - code (out of 15 points)  15
;;;; Problem 2 - proof (out of 8 points)  8
;;;; Problem 2 - synergy: connection between proof and code (out of 7 points)  7

;;;; Problem 3 - code and specification (out of 10 points)  10

;;;; Problem 4 - code (out of 20 points)  20
;;;; Problem 4 - proof (out of 8 points)  8
;;;; Problem 4 - synergy (out of 7 points)  7



;;;; Total  96
;;;; Letter Grade  A+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; Problem 1a (15 points) Write a Scheme function quo of one integer argument d which returns a function of one integer
; argument n so that ((quo d) n) is the quotient on dividing n by d.  For example, ((quo 4) 6) = 1, as 4 divides 6 just one time.  
; Assume n >= 0 and d > 0.  The function returned by (quo d) should be properly recursive, and should work by repeated subtraction.

; Problem 1b (10 points, including synergy) Prove by induction that the call (quo d) of your function quo returns a function which actually
; does compute the quotient on dividing n by d, assuming n >= 0 and d > 0.  Don't forget to include a termination argument. 


; INSERT YOUR ANSWER HERE

(define (quo d)
  (lambda (n)
    (cond ((< n d) 0)
          (else (+ 1 ((quo d) (- n d)))))))

;;;; good

(display "\nproblem 1 testing:\n")
(define x 5645)
(define y 9539101)
(equal? ((quo x) y) (quotient y x))

; Precondition: n and d are integers and n >= 0 and d > 0
; Postcondition: the quotient of n divided by d is returned.

; In the base case, we are computing the quotient of n divided by d where d > n. The answer returned is correct: 0.
; In the inductive case, we are computing the quotient of n/d where d <= n. Assuming that ((quo d) (- n d))
; correctly computes the quotient of (n-d) divided by d, we want to know that ((quo d) n) then correctly computes
; the quotient of n divided by d. Clearly, since the only difference between the two numbers is that the number n is
; now n + d, one more d can be 'factored into' the previous quotient. Thus, on adding 1 to the previous quotient,
; the new quotient would be correct given that the previous one is as well.

;;;; you need to consider n+1, not n+d, if you are to prove that the function works for all n

; Thus by induction, ((quo d) n) returns the quotient on dividing n by d for all n >= 0 (and d > 0).

; Because d > 0, infinite recursion due to perpetually subtracting 0 is not possible. Instead, because we are
; continually subtracting a positive d from n, we will eventually reach the base case; n diminishes so that eventually
; n < d.
; ------------------------------------------------------------------------------------------------------------------------------------
              
; Problem 2a (15 points) A positive integer n > 1 is said to be perfect if it is the sum of its proper divisors. Thus 6 is perfect,
; as its proper divisors are 1, 2 and 3. The next perfect number is 28, given as 1 + 2 + 4 + 7 + 14. Write an iterative Scheme program
; perfect? which inputs an integer n > 1 and which outputs #t if n is perfect and #f otherwise. Use a local function, and try to make
; your program reasonably efficient.

; Problem 2b (15 points, including synergy) Give a complete proof of correctness for the function perfect? you developed in part a.
   

; INSERT YOUR ANSWER HERE

(define (perfect? n)
  (define (iter count sum-so-far)
    (let ((next-digit (+ count 1)))
      (cond ((= count (- n 1)) (= sum-so-far n))
            ((zero? (remainder n next-digit)) (iter next-digit (+ next-digit sum-so-far)))
            (else (iter next-digit sum-so-far)))))
    (iter 1 1))

; Precondition: n is an integer > 1
; Postcondition: #t is returned iff n is perfect - otherwise #f is returned

; The technique is to iteratively determine what the proper divisors of n are; whenever we have run into one, we add it to a sum
; (initially 0).
; Then iff the sum at the end of that exhaustive test is equal to our number, then we return #t. The iterating occurs in the
; test for proper divisors (whose candidates are i, 1 <= i <= n-1).

; The invariant is that sum-so-far is the sum of the proper divisors (of n) d, 1 <= d <= count.
; On the first call, the invariant holds: 1 is the count, and 1 is always a proper divisor of n: thus,
; sum-so-far is 1. This is the only divisor d we have to check in 1 <= d <= 1. 

; Suppose the invariant holds on the kth call; suppose sum-so-far is indeed the sum of the proper divisors from 1 to count.
; Then it holds on the (k+1)st call because on the (k+1)st call we have updated the state variables in the following way:
; count <- (+ count 1)
; sum-so-far <- next-digit + sum-so-far (iff next-digit divides n properly, i.e., the remainder on dividing is 0)
; sum-so-far <- sum-so-far (iff next-digit does NOT divide n properly).

; Clearly, because sum-so-far already contains all the proper divisors up to count, by adding to it the divisor count+1
; iff count+1 is a proper divisor of n, we will have included all the proper divisors up to count+1 (which is the new count).
; Thus the invariant holds with regard to incorporating those divisors from 1 to count into sum-so-far.

; Is the invariant strong enough to show the program returns the correct value? Does it terminate?
; Yes. Count is initialized to 1
; and is increased by 1 on each call. On some eventual call count will be equal to n (since n > 1).
; At that point the program will terminate and the value of (= sum-so-far n) is returned, which
; at that point will have become equivalent to (is the sum of the proper divisors of n equal to n?)
; = (is n perfect?).

(display "\nproblem 2 testing:\n")
(perfect? 2)
(perfect? 3)
(perfect? 4)
(perfect? 5)
(perfect? 6) ; #t
(perfect? 7)
(perfect? 8)
(perfect? 20)
(perfect? 27)
(perfect? 28) ; #t
(perfect? 29) 

;;;; excellent, all the way



; ------------------------------------------------------------------------------------------------------------------------------------

; Problem 3  (10 points) Write a Scheme function genper? which generalizes the function you developed for Problem 2a. Your function should have at
; least two parameters - n as before, a function parameter, combiner, and perhaps an additional parameter or so, if necessary. Show
; how to use genper? to test whether n > 1 is the product of all of its proper divisors. What are the pre- and post-conditions for
; your genper?


; INSERT YOUR ANSWER HERE

(define (genper? n combiner)
  (define (iter count sum-so-far)
    (let ((next-digit (+ count 1)))
      (cond ((= count (- n 1)) (= sum-so-far n))
            ((zero? (remainder n next-digit)) (iter next-digit (combiner next-digit sum-so-far)))
            (else (iter next-digit sum-so-far)))))
    (iter 1 1))

(display "\nproblem 3 testing:\n")
(genper? 2 +)
(genper? 4 +)
(genper? 5 +)
(genper? 6 +) ; #t
(genper? 7 +)
(genper? 27 +)
(genper? 28 +) ; #t
(genper? 29 +)

(genper? 6 *) ; #t since (* 1 2 3) = 6
(genper? 28 *) ; #f since (* 1 2 4 7 14) != 28

; Precondition: n is an integer > 1, combiner specifies a well-defined function from integers to integers
; Postcondition: #t is returned iff n is the "combiner" of all its proper divisors, i.e., combining the proper divisors
; leads to a result equal to n; otherwise #f is returned.

; The code is the same as in the previous problem except that the operation (+ next-digit sum-so-far) is replaced by
; (combiner next-digit sum-so-far). In other words, the combiner argument supplied to the procedure determines what
; operation to do to next-digit (if it is a proper divisor of n) and sum-so-far so that sum-so-far "stores" the
; cumulative result of repeated
; applications of that operation. Of course, sum-so-far is now a misnomer.

;;;; excellent

; ------------------------------------------------------------------------------------------------------------------------------------

; Problem 4a (20 points)  Write a program reverse-number to reverse the digits of a number. For example, (reverse-number 0) = 0,
; (reverse-number 1234) = 4321. You may assume that the input is a non-negative integer.  You will likely need some auxilliary
; functions

; Problen 4b (15 points, including synergy) Give a proof of correctness for the function reverse-number you developed in part a.
; Concentrate on the proof of the main function, making use of the specifications you give for your auxilliary functions.



; INSERT YOUR ANSWER HERE

(define (reverse-number n)
  (define (count-digits x)
    (if (< x 10)
        1
        (+ 1 (count-digits (quotient x 10)))))
  (let ((number-excluding-ones-digit (quotient n 10))
        (ones-digit (remainder n 10)))
    (let ((exp (count-digits number-excluding-ones-digit)))
      (cond ((zero? number-excluding-ones-digit) ones-digit)
            (else (+ (* (expt 10 exp) ones-digit) (reverse-number number-excluding-ones-digit)))))))

(display "\nproblem 4 testing:\n")
(reverse-number 321321) ; 123123
(reverse-number 1909) ; 9091
(reverse-number 5006) ; 6005
(reverse-number 19) ; 91
                       
; First, a recursive idea: the number with all k digits of n reversed is the number with all k-1 highest-order digits reversed
; with the ones-digit added to the front. For example, the reversal of 1234 involves inserting 4 to the front of the reversal of 123,
; which is 321. Of course (in the base case) the reversal of a one-digit integer would be the integer itself <=> the result
; of inserting ones-digit to the front of number-excluding-ones-digit = 0 would be ones-digit.

; Precondition: n is a nonnegative integer.
; Postcondition: the number that is n except that its digits are reversed is returned.

; First we prove that (count-digits x) returns the number of digits in a positive integer x. It utilizes recursion.
; In the base case, when x < 10, it is necessarily a one-digit number; so we correctly return 1.
; In the inductive case, when x has more than one digit, we compute the number of digits in x by computing
; 1 + (count-digits (quotient x 10)).
; (quotient x 10) is x transformed such that its ones digit is removed. Clearly, if count-digits correctly
; counts the number of digits in (quotient x 10), adding 1 to it (representing the ones digit) would give us
; the correct count
; for x. Since (quotient x 10) shortens x by one digit on each call, on some call x will be reduced to a
; one-digit number and the program will terminate.

; Remember that (count-digits x) can only input a positive integer.

; Our reverse-number function first works by defining number-excluding-ones-digit and ones-digit.
; Then we decide how many digits are in number-excluding-ones-digit, which is the number
; we are about to reverse. We called the result 'exp'; thus, by computing 10^exp, we get a number
; with g trailing zeroes where g = the number in number-excluding-ones-digit. If we then multiply
; an integer i by this number, we get i00000...0 where the number of zeroes matches the number
; of digits in number-excluding-ones-digit. It is in this way that we can append any number i
; to the front of number-excluding-ones-digit. Clearly, number-excluding-ones-digit is always
; an integer (quotient always returns an integer); thus, we can use (count-digits x) which
; specifies that x be a positive integer. (And since n is never negative, x cannot be negative).

; The main part of our computation works as follows:

; In the base case, number-excluding-ones-digit is zero, which means that there is nothing to reverse.
; Thus, we just return ones-digit (the result of appending ones-digit to the front of 0).

; In the inductive case, number-excluding-ones-digit is NOT zero, which means there are some digits
; that we have to reverse. In that case, if (reverse-number number-excluding-ones-digit) correctly
; returns number-excluding-ones-digit but reversed, we simply need to append ones-digit to that
; result (to the front). Clearly, because ones digit was the very last digit and is now the very
; first digit, the number is the same as the original input but its digits are in reverse order.

; Perhaps it is worthy to note that if the ones-digit of our original number n is 0, then the
; reversal will be missing the 0, since the 0 will be moved to the front and disappear.
; Thus, for a proper reverse, we could indicate that n only has digits between 1 and 9
; in the precondition (or just that its ones-digit is not 0).


;;;; you are the only one so far to notice this!  Great!  The thing to do is to strengthen the precondition,
;;;; to block inputs with trailing 0s.



