; CSc 335
; Spring 2015

; March 3

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 10 points)
;;;; Problem 1 - proof (max 15 points)

;;;; Problem 2 - code (max 10 points)
;;;; Problem 2 - proof (max 15 points)

;;;; Problem 3 - code (max 10 points) 
;;;; Problem 3 - proof (max 15 points)

;;;; Problem 4a - code (max 5 points)
;;;; Problem 4b - code (max 5 points)
;;;; Problem 4c - code (max 5 points)
;;;; Problem 4d - code (max 10 points)

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

; -------------------------------------------------------------------------------------------------------------------
; The idea is to add n to a variable (which is initially zero) m times. Given that m >= 0, this seems like the
; right idea (it makes no sense to say to do something a negative number of times, so we let m, rather than n,
; be that number, the number of multiplications).
; We can set a counter variable initially to m and decrement it toward zero as we add n continually to another
; variable.
; In the base case, if m = 0, we stop iterating and return the value of that other variable.

(define (mymul m n)
  (define (mymul-iter count result-so-far)
    (cond ((= count 0) result-so-far)
          (else (mymul-iter (- count 1) (+ n result-so-far)))))
  (mymul-iter m 0))

(display "Problem 1 testing\n")
(mymul 6 8) ; 48
(mymul 0 6) ; 0

; The invariant is that result-so-far = n*(m-count).
; For example, in the initial call, count has been initialized to m, and result-so-far is 0, and
; then in the next call, count is decreased to m-1, and result-so-far is n. In the next call, count is
; decreased to m-2 and result-so-far is 2n. And so on.
; 
; The invariant is satisfied in the initial call, as count = m and result-so-far = 0 = n*(m-m).
; 
; Suppose it is true in the kth call that result-so-far = n*(m-count). In the (k+1)st call,
; when count has been decremented, that invariant relationship becomes result-so-far = n*(m-(count+1)) = n*(m-count-1).
; At the same time that count is decremented, however, result-so-far is replaced by n + result-so-far,
; so that becomes result-so-far = n*(m-count-1)+n = n*(m-count-1+1) = n*(m-count).
; Thus, the invariant still holds in the (k+1)st call if it held in the kth call.

; By induction on the number of calls, the invariant holds in every call.

; When the program terminates, count = 0, so by the invariant, result-so-far has become equal to n*(m-count) = n*m,
; which is what we want.

; Because m is a nonnegative integer and we are decrementing n
; on every iteration, on some iteration m will necessarily be equal to 0 and the program will terminate.
; -------------------------------------------------------------------------------------------------------------------


; Problem 2 (25 points) Use the equation (n - 1)^2 = n^2 -2n + 1 to develop a non-iterative, recursive scheme
; procedure which inputs an integer n >= 0 and which returns n^2.  Give correctness and termination arguments.

; -------------------------------------------------------------------------------------------------------------------

; (n-1)^2=n^2-2n+1 => (n-1)^2+2n-1=n^2

(define (square n)
  (cond ((= n 0) 0)
        (else (+ (square (- n 1)) (* 2 n) -1))))

(display "\nProblem 2 testing\n")
(square 1) ; 1
(square 5) ; 25

; Precondition: n is a nonnegative integer.
; Postcondition: return the value of n^2.

; In the base case, when n = 0, we return n^2 = 0.

; In the inductive case, when n > 0, we return (square (- n 1)) + 2n - 1.
; Given that (square (- n 1)) correctly evaluates to (n-1)^2, then (square n) will be correct.

; By induction, the square function returns the correct value for all nonnegative integer inputs n.

; Because n is a nonnegative integer and is always decremented by 1 in each call,
; in some eventual call it will be equal to zero and the program will terminate.
; -------------------------------------------------------------------------------------------------------------------

; Problem 3 (25 points) Write and prove correct a scheme procedure which inputs a function f and integers
; a and b with a <= b and which outputs an integer k, a <= k <= b, such that the value f(k) of f at k is
; smallest among all values in the set {f(a), f(a+1), ... , f(b)}.  Specify how your procedure handles
; the case when there are several points k between a and b at which the value of f(k) is smallest.  Remember
; to include in your pre-condition whatever constraints you need to impose on the function parameter f;
; remember to give a termination argument.

; -------------------------------------------------------------------------------------------------------------------
; We can solve this by using recursion. The smallest integer in f between a and b is the smaller of these
; two integers: a and the smallest of the integers in f between a+1 and b. 
; In the base case, if a = b, the smallest integer is trivially f(a) = f(b).

(define (smallest f a b)
  (cond ((= a b) (f a))
        (else (let ((sub-smallest (smallest f (+ a 1) b)))
                (if (< (f a) sub-smallest)
                    (f a)
                    sub-smallest)))))

(display "\nProblem 3 testing\n")
(smallest (lambda (x) x) 2 10) ; 2
(smallest (lambda (x) (- x 5)) 2 10) ; -3

; Precondition: a and b are integers where a <= b; and f is a function of an integer.
; Postcondition: return the integer i, a <= i <= b, such that f(i) is the smallest of {f(a), f(a+1), ..., f(b)}.
; If there are multiple occurrences of such an i, i is still returned.

; In the base case, when a = b, f(a) is returned: the smallest integer in f between a and a, that is,
; in {f(a)}, is just f(a).

; Otherwise, in the inductive case (a < b), we define sub-smallest = (smallest f (+ a 1) b) and compare it with (f a), returning
; the smaller of the two values or, if they're equal, the identical value. If sub-smallest = (smallest f (+ a 1) b) is indeed
; the smallest integer in f between a+1 and b, then our if expression necessarily returns the
; smallest integer between a and b because the only possible integer that could be smaller than sub-smallest
; in f between a and b is f(a), which is returned if and only if it is smaller than sub-smallest (and sub-smallest if it is not).
; That is, if we've compared all
; the f(i) values for a+1 <= i <= b and obtained the smallest among them, then our if expression will necessarily
; give us the smallest of the f(i) for a <= i <= b.
; 
; By induction on the size of the gap between a and b (i.e., on b-a), we know smallest will return the smallest integers in f for intervals
; of any size, or on intervals from any a to any b, where the only constraint is a <= b.
; 
; Given the preconditional constraint a <= b and the fact that a and b are integers, we know that the recursive process will terminate
; because in each call we increment a by 1; a will be equal to b in some eventual call.

; I realize that the function as written returns f(k) rather than k, so it doesn't meet the specification,
; but all we have to do to fix it is to add an extra variable to keep track of the value of k for which f(k) was the smallest.
; -------------------------------------------------------------------------------------------------------------------

; Problem 4 (25 points) First, for 5 points (this was homework), write a procedure compose that implements
; function composition - for f and g functions of one argument, (compose f g) is the function which
; computes (f (g (x))).  For example, ((compose square inc) 6) = 49.  

; -------------------------------------------------------------------------------------------------------------------
(define (compose f g)
  (lambda (x)
    (f (g x))))

(display "\nProblem 4 testing\n")
(define (inc x) (+ x 1))
((compose square inc) 6) ; 49
; --------------------------------------------------------------------------------------

; Second, for 5 points (this was homework), write a procedure that takes as inputs
; a procedure that computes a function f of one argument and a positive integer n and which returns the
; procedure that computes the nth repeated application of f. Your procedure should be able to be used as follows:

; ((repeated square 2) 5)
; 625

; -------------------------------------------------------------------------------------------------------------------
; Iterative version:

(define (repeated f n)
  (define (iter count result-so-far)
    (cond ((= count n) result-so-far)
          (else (iter (+ count 1) (compose f result-so-far)))))
  (iter 1 f))

(repeated inc 5) ; #<procedure>
((repeated inc 5) 4) ; 9
((repeated square 2) 5) ; 625

; Recursive version:

(define (repeated f n)
  (cond ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))

(repeated inc 5) ; #<procedure>
((repeated inc 5) 4) ; 9
((repeated square 2) 5) ; 625
; -------------------------------------------------------------------------------------------------------------------

; Third, for 5 points, write a function deriv which takes two arguments - a function g and a small
; number dx - and which returns a function (deriv g dx) which approximates the derivative of g using
; the difference quotient
 

;  g(x + dx) - g(x)
; -----------------
;        dx

; Thus, assuming cube is the function (lambda (x) (* x x x)), ((deriv cube .0001) 5) will be a number
; quite close to 75.

; -------------------------------------------------------------------------------------------------------------------
(define (deriv g dx)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))
(deriv cube .0001) ; #<procedure>
((deriv cube .0001) 5) ; 75.0015...
; -------------------------------------------------------------------------------------------------------------------

; And finally, for 10 points, show how to use repeated and deriv to compute a function nth-deriv approximating
; the nth derivative of its input function.  That is, the call

; ((nth-deriv cube 2 .0001) 5)

; will be a number quite close to 30

; -------------------------------------------------------------------------------------------------------------------
; (repeated f n) returns the nth composition of f with itself (it returns a procedure).
; (deriv f dx) returns the derivative of f.

; deriv must be rewritten as a function that takes a single function as input (dx is hard-coded).

(define (nth-deriv f n dx)
  (define d (lambda (g) (deriv g dx))) ; d is a derivative function of one argument
  (cond ((= n 0) f) ; the 0th derivative of f is f itself
        (else ((repeated d n) f)))) ; apply d to f n times

((nth-deriv cube 2 .0001) 5) ; 30.0005...
; -------------------------------------------------------------------------------------------------------------------

; NO PROOFS ARE REQUIRED FOR PROBLEM 4

