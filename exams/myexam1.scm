; CSc 335
; Spring 2020

; February 27

; First Exam - 1.25 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Abtahi Chowdhury

; TYPE YOUR FULL EMAIL ADDRESS HERE: achowdh008@citymail.cuny.edu

; (I will email your graded paper to this address - DO NOT use a gmail address, as gmail seems to block name.scm 
;  files.  Please use your citymail address.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; SAVE YOUR EXAM AS Lastname.Firstname.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - 30 points  0
;;;; Problem 1b - 30 points  0

;;;; Problem 2 - 40 points   36


;;;; Total  36
;;;; Letter Grade  D


;;;; My comments in your exam are behind (at least) 4 semicolons (;;;;)
;;;;
;;;; Detailed scoring found below each problem


;;;; Your score does not reflect your understanding of the material -- you lost all credit for problem 1b
;;;; by not following instructions.  I can't give you points for solving a problem different than what I asked
;;;; for, but I want to be sure you know that my opinion of your work is in fact much better than the D grade shown here.

;;;; Next time, can you PLEASE read the instructions carefully?!!!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework:

; no lists, no vectors, no strings, no assignment ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed in your backpack/briefcase.  They are not to leave the room, nor are
; they to be visible at any point during the exam.  Even the sight of a phone, or a flashdrive, or any other 
; electronic device not supplied by the course staff will result in our picking up your paper, resulting in an F
; grade for the exam.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; For both problems:  a complete development is one which features mutually reinforcing logic and code, 	
; resulting in a program which has been proved correct.  

; Grading will be based on your logic, your code, and the evidence of synergy between these. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1a (30 points)  An integer p is a perfect square if if p = q^2, for some integer q.  Thus 4 is a perfect square,
; but neither 2 nor 3 is a perfect square.  It is known that an integer p >= 0 is a perfect square if and only if it has an
; an odd number of divisors.  In this problem you are asked to write a recursive procedure perfect-square? based on
; this fact.  Your procedure may not use square, nor sqrt.

; INSERT YOUR ANSWER HERE

;Design Idea: induct on number, so that it is decremented by 1 on each call
;             possibly do N%n on each step and return 1 + (perfect-square? (- number 1)) based on the result of N%n?

;IH: n is a perfect square if
;IS: 
;BS: number is zero?

;(define (perfect-square? number)
;  (cond ((= number 0) 0)
;        ((= number


;;;; Problem 1 Section M Exam 1 Scoring
;;;;
;;;;
;;;; Precondition discussion leading to weakest precondition (out of 10)  
;;;;
;;;; Design discussion, leading to guess code and guess induction hypothesis and termination argument (out of 10) -- 1
;;;;
;;;; Final recursive and working code (out of 10)
;;;;
;;;; Final induction argument:  Basis, IH, IS (out of 15)
;;;;
;;;; Tests  (out of 5)  
;;;;
;;;; Total Problem 1 -- 1
;;;;





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1b (30 points) Now write an iterative procedure perfect-square-I? to solve the same problem.  Make use of 
; the fact that a number n is a perfect square if and only if it is a sum of consecutive odd numbers, starting at 1.  

; INSERT YOUR ANSWER HERE


;Design idea: make an aux funtion that will calculate the number of divisors and return it. Then check if its odd or even
;             funtion requires parameters to keep track of number of divisors so far, number we are checking right now, and
;             original number to compare for termination





;;;; WHY ARE YOU COUNTING THE NUMBER OF DIVISORS?  PROBLEM 1B INSTRUCTS YOU TO CHECK WHETHER THE INPUT IS THE
;;;; SUM OF CONSECUTIVE ODD NUMBERS, STARTING AT 1

;;;; IT APPEARS YOU ARE GIVING AN ITERATIVE VERSION OF THE APPROACH REQUIRED FOR PROBLEM 1A

;;;; I AM HAPPY TO DISCUSS YOUR SOLUTION, BUT I CANNOT GIVE YOU POINTS HERE -- YOU HAVE SOLVED A PROBLEM DIFFERENT
;;;; FROM THE ONE I ASKED FOR





;gI: divisor-total = number of divisors of N in the interval [1,count] + number of divisors of N in the interval [count+1,N], assuming 1<=count<=N

;pre: number is a non-negative number, count=1, divisot-total=0
;post: returns the number of divisors of number
;(define (count-divisors number count divisors-total)
;  (cond ((> count number) divisors-total)
;        ((= (modulo number count) 0) (count-divisors number (+ count 1) (+ divisors-total 1)))
;        (else (count-divisors number (+ count 1) divisors-total))))

;some test cases
;(count-divisors 5 1 0)
;(count-divisors 5 1 0)

;proof of correctness
;Init: initially divisor-total should be 0 due to count being 1 and the number of checked integers being 0. This is seen in the code
;      [(count-divisors number 1 0)] where it is initialized to 0
;Termination: terminates when count is outside of the range of [1,N] and this can be seen in the code [((> count number) (modulo divisors-total 2))]
;             where the p-o-c expression will end due to another call not being called.
;Preservation: assuming that k is some number between 1 and N, and divisor-total is correct so far, there are two cases that can happen on the k+1th call:
;              case 1: on the k+1th call, count is a divisor of N and divisors-total will be incremented by 1. This can be seen in the code
;                      [(count-divisors number (+ count 1) (+ divisors-total 1))] where divisor-total is incremented by 1 on the k+1th call.
;              case 2: on the k+1th call, count is not a divisor of N and divisors-total will not be incremented by 1. This can be seen in the code
;                      [(count-divisors number (+ count 1) divisors-total)] where divisor-total is not incremented by 1 on the k+1th call.
;
;Since we know that count-divisors works, we simply do remainder division on it to see if its even or odd. 


;pre: number is a non-negative number
;post: returns true if number is a perfect square and false otherwise
(define (perfect-square-I? number)

  (define (count-divisors number count divisors-total)
    (cond ((> count number) divisors-total)
          ((= (modulo number count) 0) (count-divisors number (+ count 1) (+ divisors-total 1)))
          (else (count-divisors number (+ count 1) divisors-total))))
  
  (cond ((= (modulo (count-divisors number 1 0) 2) 0) #f)
        (else #t)))

;some test cases
(perfect-square-I? 4)
(perfect-square-I? 5)
(perfect-square-I? 16)
(perfect-square-I? 28)
(perfect-square-I? 36)


;;;; Problem 1b Section R Exam 1 Scoring
;;;;
;;;;
;;;; Design discussion, leading to guess code and guess invariant and termination argument (out of 10) 
;;;;
;;;; Final iterative and working code (out of 7)
;;;;
;;;; Final certification:  weak enough, strong enough, invariance (out of 10)
;;;;
;;;; Tests  (out of 3)
;;;;
;;;; Total Problem 1b  -- 0 -- SEE MY NOTE ABOVE.  REALLY QUITE A SHAME. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Problem 2 (40 points)  Give a complete development of an iterative procedure sum-perfect-squares which inputs
; two numbers m and n and which returns the sum of the perfect squares between m and n.  You may use either one of
; the perfect-square predicates from Problem 1, but you must make this function local -- that is, taking full
; advantage of variables' scope, you are to recast the perfect squares predicate as a function private to 
; to sum-perfect-squares.

; Make your precondition as weak as possible.



; INSERT YOUR ANSWER HERE

;Design Idea:use previous check function and iterate through number between m and n and sum numbers that are perfect squares

;gI: sum of perfect squares in the interval [m,count] + sum of the perfect squares in the interval [count+1,n] = sum of the perfect
;    squares in the interval [m,n], with the assumption that n>m and m<count<n

;;;; GOOD



;pre: bound=larger of the two integers m and n, count=smaller of the two integers m and n, sum=0


;;;; CAN M, N BE FLOATS?  CAN THEY BE NEGATIVE?  (YES, TO BOTH QUESTIONS ... ). The idea is to write a wrapper function
;;;; to handle the special cases, and to set up for calling your main function.


;post: sum of all perfect squares between m and n
;(define (sum-perfect-squares-iter bound count sum)
;    (cond ((> count bound) sum)
;          ((perfect-square-I? count) (sum-perfect-squares-iter bound (+ count 1) (+ sum count)))
;          (else (sum-perfect-squares-iter bound (+ count 1) sum))))

;some test cases
;(sum-perfect-squares-iter 9 4 0)
;(sum-perfect-squares-iter 16 3 0)
;(sum-perfect-squares-iter 21 1 0)
;(sum-perfect-squares-iter 101 15 0)

;proof of correctness
;init: initially sum is 0 because count=m and the sum of the perfect squares in the interval [m,m] is 0 and this can be
;      seen in the code [(sum-perfect-squares-iter n m 0)].
;terimation: terminates when count is outside of the range of [m,n] and reutrns the sum of all perfect squares between m and n. This can
;            be seen in the code [((> count bound) sum)]
;preservation: assuming that k is some number between m and n, and sum is correct so far, there are two cases:
;              case 1: on the k+1th call, count is a perfect square and is added to sum. This is seen in the code
;                      [(sum-perfect-squares-iter bound (+ count 1) (+ sum count))]
;              case 2: on the k+1th call, count is not a perfect square and is not added to sum. This is seen in the code
;                      [(sum-perfect-squares-iter bound (+ count 1) sum)]



;pre: m and n are non-negative integers
;post:sum of all perfect squares between n and m
(define (sum-perfect-squares m n)
  ;in the previous question this function was proven to be correct
  (define (count-divisors number count divisors-total)
    (cond ((> count number) divisors-total)
          ((= (modulo number count) 0) (count-divisors number (+ count 1) (+ divisors-total 1)))
          (else (count-divisors number (+ count 1) divisors-total))))

  ;in the previous question this function was proven to be correct
  (define (perfect-square-I? number)
    (cond ((= (modulo (count-divisors number 1 0) 2) 0) #f)
          (else #t)))
  
  (define (sum-perfect-squares-iter bound count sum)
    (cond ((> count bound) sum)
          ((perfect-square-I? count) (sum-perfect-squares-iter bound (+ count 1) (+ sum count)))
          (else (sum-perfect-squares-iter bound (+ count 1) sum))))

  (cond ((> n m) (sum-perfect-squares-iter n m 0))
        (else (sum-perfect-squares-iter m n 0))))

;test cases
(sum-perfect-squares 4 9)
(sum-perfect-squares 9 4)
(sum-perfect-squares 4 16)
(sum-perfect-squares 651 102)
  

;;;; Problem 2 Section R Exam 1 Scoring
;;;;
;;;; Discussion leading to weakest possible precondition  (out of 5)  1 -- you should develop the wrapper
;;;; I mention above, and then certify that it establishes the invariant you propose.

;;;;
;;;; Design discussion, leading to guess code and guess invariant and termination argument (out of 10) 10
;;;;
;;;; Final iterative and working code with local perfect-square predicate (out of 10) 10
;;;;
;;;; Final certification:  weak enough, strong enough, invariance (out of 10) 10
;;;;
;;;; Tests  (out of 5)   5
;;;;
;;;; Total Problem 2  36
;;;;  
;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;







