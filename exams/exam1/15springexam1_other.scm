; CSc 335
; Spring 2015

; March 3

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Lukas Rascius

; TYPE YOUR FULL EMAIL ADDRESS HERE: lrascius@gmail.com
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 10 points)     10
;;;; Problem 1 - proof (max 15 points)    15

;;;; Problem 2 - code (max 10 points)     10
;;;; Problem 2 - proof (max 15 points)    15

;;;; Problem 3 - code (max 10 points)     10
;;;; Problem 3 - proof (max 15 points)    15

;;;; Problem 4a - code (max 5 points)      5
;;;; Problem 4b - code (max 5 points)      5
;;;; Problem 4c - code (max 5 points)      5
;;;; Problem 4d - code (max 10 points)     5


;;;; Total  95
;;;; Letter Grade  A+


;;;; my comments are inserted in-line, always behind the symbol ;;;;

;;;; Thank you for doing such an excellent job with this exam.  I would very much like to 
;;;; meet you - could you possibly come to office hours tomorrow?  








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the subset of the pretty
; big scheme implementation provided by drracket and discussed so far in the context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  

; Problem 1 (25 points) Design and certify an iterative scheme procedure mymul which accepts integer inputs
; m >= 0 and n, and which returns the product of m and n.  Your procedure should use repeated addition
; to accomplish the multiplication; it should not use primitive multiplication procedures.  Your proof
; should be based on an invariant. Don't forget the termination argument. 

(define (mymul m n)
  (define (iter count result)
    (cond ((= count n) result)
          (else (iter (+ count 1) (+ result m))))
    )
  (iter 0 0))

; Proof

;Invariant: result = (m * count)

; Beggining: At the start of the loop, count is initialized to 0 and result is 0, this obvious because 0 = 0 * m
; Induction: Assume that it works for kth iteration, meaning that result = (m * count). We need to show that this 
; works for k+1 iteration. The k+1 iteration will return (iter (+ count 1) (+ result m)), so we will have that (+ result m) = (* (+ count 1) m )

; At terimation, count will equal to n, so result will return m*n which is the definition of multiplication. We need to also show that the 
; program terminates, which is clear because count is an integer and it is incremented by 1 on each iteration so it must eventually,
; reach n


;;;; good job


; Problem 2 (25 points) Use the equation (n - 1)^2 = n^2 -2n + 1 to develop a non-iterative, recursive scheme
; procedure which inputs an integer n >= 0 and which returns n^2.  Give correctness and termination arguments.

(define (square n)
  (cond ((= n 1) 1)
        (else (+ (square (- n 1)) (* 2 n) -1)))
  )
(square 5)

; Base Case : If n = 1, then just return 1 because the square of 1^2 = 1
; Induction Hypothesis: Assume that it works for (square n) meaning that the value returned is n^2
; Induction Step: We need to show that it works for (square ( + n 1)). Well (square (+ n 1)) will 
; return (+ (square (- (+ n 1) 1)) (* 2 (+ n 1)) -1)
;       =(+ (square (n)) (* 2 (+ n 1)) -1)
;       which mathematically is equivalent to n^2 + 2(n+1) - 1 = (n + 1)^2 
; To show that the program terminates, we can say since n is an integer and it is decremented by 1 on each call to 
; square then n will equal to 1 eventually.

;;;; excellent






; Problem 3 (25 points) Write and prove correct a scheme procedure which inputs a function f and integers
; a and b with a <= b and which outputs an integer k, a <= k <= b, such that the value f(k) of f at k is
; smallest among all values in the set {f(a), f(a+1), ... , f(b)}.  Specify how your procedure handles
; the case when there are several points k between a and b at which the value of f(k) is smallest.  Remember
; to include in your pre-condition whatever constraints you need to impose on the function parameter f;
; remember to give a termination argument.

(define (prob3 f a b)
  (define (iter count smallest)
    (cond ((= count (+ b 1)) smallest)
          ((> smallest (f count)) (iter (+ count 1) (f count)))
          (else (iter (+ count 1) smallest ))))
  (iter (+ a 1) (f a)))

(define (randomfunc x) 
  (cond ((= x 1) 3)
        ((= x 2) 3)
        ((= x 3) 1)
        ((= x 4) 5)
        ((= x 5) 2)
        ((= x 5) 5)
        ((= x 6) 10)))

(prob3 randomfunc 1 5) ; returns 1 which is smallest
; Proof
; Invariant: The variable (smallest) is the smallest number returned by the function call f(k) where k is between a <= k < count 

; Beggining: At the start of the loop we initialize smallest to (f (a)) and count to a+1 which is correct because on the first call
; the first value should be the smallest since we haven't check any of the other values yet. 
; Induction:Assume that it works for the kth iteration, meaning that smallest is the smallest number from functions calls f(k) from a <= k < count
; We need to show that the invariant is true on the k+1st call. On the k+1st call, the function will return (iter (+ count 1) (f count)) if
; f(count) is smaller than the variable (smallest) which is correct because are updating the (smallest) variable and incrementing count. Else the function
; returns the (iter (+ count 1) smallest ) which is also correct because we didn't change (smallest) and count was also incremented.
; Termination: The iteration finishes when count = b+1 because we still want to check that f(b) is not the smallest. Therefore when the iteration finishes 
; (smallest) will be returned which is correct answer because k has went through all
; the values of f(a <= k <= b). Lastly, to show that the program eventually terminates, we note that count is an integer and it is incremented
; on each iteration, so it will eventually reach b+1. 


;;;; also excellent.  


; Problem 4 (25 points) First, for 5 points (this was homework), write a procedure compose that implements
; function composition - for f and g functions of one argument, (compose f g) is the function which
; computes (f (g (x))).  For example, ((compose square inc) 6) = 49.  
(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose2 f g)
  (define (comp x)
    (f (g x)))
  comp)

((compose2 square inc) 6)

;;;; good



; Second, for 5 points (this was homework), write a procedure that takes as inputs
; a procedure that computes a function f of one argument and a positive integer n and which returns the
; procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

(define (repeated f n)
  (define (counter count result)
   (cond ((= count n) result)
         (else (counter (+ count 1) (compose2 f result)))))
  (counter 1 f))
  
 ((repeated square 2) 5)
; 625

;;;; good

; Third, for 5 points, write a function deriv which takes two arguments - a function g and a small
; number dx - and which returns a function (deriv g dx) which approximates the derivative of g using
; the difference quotient
 
 (define (deriv g dx)
   (define (retfunc x)
     (/ (- (g (+ x dx)) (g x)) dx)
     )
   retfunc)
  
(define (cube x) (* x x x))
(deriv cube .0001)
((deriv cube .0001) 5)


;;;; good



;  g(x + dx) - g(x)
; -----------------
;        dx

; Thus, assuming cube is the function (lambda (x) (* x x x)), ((deriv cube .0001) 5) will be a number
; quite close to 75.  

; And finally, for 10 points, show how to use repeated and deriv to compute a function nth-deriv approximating
; the nth derivative of its input function.  That is, the call

; ((nth-deriv cube 2 .0001) 5)

; will be a number quite close to 30


(define (nth-deriv f n dx)
  (define (retfunc x)
    (cond ((= n 0) (deriv f dx))
          (else (repeated (nth-deriv f (- n 1))))
          ))
    retfunc)

;;;; hmmmmm.  what is the 0th derivative?  
;;;; and ... you have only 2 args to the last call of nth-deriv


;((nth-deriv cube 2 .0001) 5)
; NO PROOFS ARE REQUIRED FOR PROBLEM 4



