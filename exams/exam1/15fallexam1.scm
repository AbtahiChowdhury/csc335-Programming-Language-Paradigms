; CSc 335
; Fall 2015

; October 13

; First 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 15 points)
;;;; Problem 1 - proof (out of 10 points)

;;;; Problem 2 - code (out of 10 points)
;;;; Problem 2 - proof (out of 15 points)

;;;; Problem 3 - code (out of 20 points)
;;;; Problem 3 - proof (out of 20 points)

;;;; Problem 4 (out of 10 points)

;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no lists, no strings, no assignment.  

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Here are the examination problems.  


; Problem 1 (25 points) Write a scheme function expo of one integer argument b which returns a function of
; one argument e so that, for e a non-negative integer, ((expo b) e) returns b raised to the eth power.
; The function returned by (expo b) should work by repeated multiplication, and should be be properly
; recursive; give a proof that this function is correct.  Don't forget the termination argument.

; ---------------------------------------------------------------------------------------------------------------------------
(define (expo b)
  (define (expo-recur e)
    (if (= e 0)
        1
        (* b (expo-recur (- e 1)))))
  expo-recur)

(display "Problem 1 testing\n")
((expo 2) 0) ; 1
((expo 2) 1) ; 2
((expo 2) 2) ; 4
((expo 2) 3) ; 8
((expo 2) 4) ; 16

; Precondition: b is any integer; e is any nonnegative integer.
; Postcondition: return b^e.

; (expo b) = expo-recur. So ((expo b) e) = (expo-recur e).

; The base case occurs when e = 0, in which case the function returns 1 (correctly because b^0 = 1 for any b).
; The inductive case occurs when e != 0 (i.e., when e > 0).
; In that case, the function returns the value of b multiplied by (expo-recur (- e 1)).

; If (expo-recur (- e 1)) correctly returns b^(e-1), then (expo-recur e) correctly returns b^e
; since it just multiplies the result of the recursive call by b.
; If we assume that expo-recur is correct for the parameter e, we can be sure it is correct for the parameter e+1.
; Since it
; is correct for e = 0 (the base case), we can conclude by induction that it is correct for all e > 0.

; Given that e is a nonnegative integer and (- e 1) is the replacing parameter
; in the recursive call, e will eventually equal zero and the recursive process will terminate.
; ---------------------------------------------------------------------------------------------------------------------------


; Problem 2 (25 points) Design and certify an iterative scheme procedure count-digits which inputs a single
; non-negative integer n and which returns the number of digits in n.  Your proof should be based on an
; invariant.  Again, do not forget the termination argument.

; ---------------------------------------------------------------------------------------------------------------------------
(define (count-digits n)
  (define (count-iter a result-so-far)
    (if (< a 10)
        (+ 1 result-so-far)
        (count-iter (quotient a 10) (+ 1 result-so-far))))
  (count-iter n 0))

(display "\nProblem 2 testing\n")
(count-digits 0) ; 1
(count-digits 123) ; 3
(count-digits 95483) ; 5
(count-digits 954830) ; 6

; Precondition: n is a nonnegative integer.
; Postcondition: the number of digits in n is returned.

; Let n_x denote the number of digits in an integer x.
; The invariant of count-iter is n_n = n_a + result-so-far.

; Does the invariant hold in the first call? Well, the first call to our iterative procedure is (count-iter n 0).
; The number of digits in n is certainly the number of digits in n plus 0 (i.e., n_n = n_n + 0).

; a is a number that is initially equal to n and loses its digits one at a time in every next call, whereas
; result-so-far is a count of the number of digits lost, which is initially equal to 0 and increases by 1 in every next call.

; If the invariant holds in the kth call, does it hold in the (k+1)st call as well (assuming the kth call is not the last call)?
; Suppose on the kth call the arguments are a and result-so-far.
; In the (k+1)st call, the arguments are going to be (quotient a 10) and (+ 1 result-so-far).
; The first argument has lost its ones digit, and result-so-far has increased by 1.
; We know the invariant continues to hold in the (k+1)st call since n_a is decreased by 1 and result-so-far is increased by 1,
; and n_n = n_a + result-so-far <=> n_n = (n_a - 1) + (result-so-far + 1).

; By our invariant, is the correct answer returned when the program terminates? And does the program terminate?
; The program terminates when a is a one-digit number, in which case it returns (+ 1 result-so-far).
; In that final call the invariant would be that n_n = n_a + result-so-far = 1 + result-so-far, so it does return n_n if it
; terminates.

; The program always terminates because a loses its ones digit on every call; since n is nonnegative, a will eventually become
; a one-digit number.
; ---------------------------------------------------------------------------------------------------------------------------

; Problem 3 (40 points)  Write and certify a scheme function scramble with arguments n and f, where n is a
; positive integer and f is a function from the set {0,1,2,...,9} of digits to the set of non-negative
; integers, and which returns the number formed from n by replacing each digit j by the digits (in order)
; of the value (f j).

; Thus if f is the function which squares each digit, (scramble 403612 f) returns 16093614

; Your function can be either recursive or iterative, as you see fit: be sure to say which, and to give
; a proof (induction or invariant based) which matches your choice. Again, don't forget the termination argument.


; (Hint: work from the right, and perhaps make use of your function count-digits. Your proof, should you use
; count-digits, will need to show that the count-digits precondition holds each time it is called; your proof should also
; indicate how the post-condition of count-digits contributes to the main argument. )

; ---------------------------------------------------------------------------------------------------------------------------
(define (scramble f n)
    (define (scramble-iter a k result-so-far)
      (let ((val (f (remainder a 10))))
        (cond ((= a 0) result-so-far)
              (else (scramble-iter (quotient a 10)
                                   (+ k (count-digits (f (remainder a 10))))
                                   (+ result-so-far (* val ((expo 10) k))))))))
  (scramble-iter (quotient n 10)
                 (count-digits (f (remainder n 10)))
                 (f (remainder n 10))))

(display "\nProblem 3 testing\n")
(scramble (lambda (x) (* x x)) 403612) ; 16093614
(scramble (lambda (x) (+ x 1)) 9990) ; 1010101
(scramble (lambda (x) (+ x 1)) 0) ; 1

; Precondition: n is a positive integer and f is a function that maps a 0-9 digit to a nonnegative integer.
; Postcondition: return the number that is the number n with each digit j replaced by f(j).

; The strategy is to, every iteration,
; 1. extract the ones digit from a, denoted j in this analysis,
; 2. evaluate f(j) and multiply it by 10^k, where k is the number of digits we want to "shift" f(j) by, and then
; 3. add it to result-so-far.

; For example, if the f is the square function and n = 123, j will be 3, 2 and then 1: we start with 9 as our partial
; result, add 40 (4 shifted one digit) and then add 1000 (1 shifted three digits).

; The invariant is that result-so-far is the correct (partial) result for the part of the string processed so far,
; which is precisely the last k digits of n, and that a is the part of the string yet to be processed.
; So after the second iteration in the above example, the processed part is 23 and the partial result is correctly 49.

; Note that (quotient a 10) returns the number a with its ones digit removed, and (remainder a 10) returns
; the ones digit of a.

; First we ought to show that our argument k always represents the amount by which we should shift the next f(j).
; In the initial call, k = 0 because we don't need to shift the first f(j). For example, if our function is the successor
; function and n = 123, obtaining the correct result 234 involves starting with 4 (no shifting is yet needed).
; If k is the correct value in the current call, it will be correct on the next call if we count the number of digits
; of the current f(j) and add it to the previous count. For example, if we have the partial result 555 and our f(j) is 16
; (so that the updated result will be 16555), and we know that f(j) = 16 has to be shifted 3 digits (i.e., we add 16000 to
; the result), we know, given that (count-digits 16) = 2, that the next f(j) will then have to be shifted 3 + 2 = 5 digits.

; Each next call transforms the previous value of result-so-far in the following way:
; 1. It retains the digits of result-so-far in their original ordering and placement.
; 2. For the next digit j, it affixes f(j) to the left of those digits.

; If result-so-far satisfies its part in the invariant in the kth call, will it continue to do so?
; Given that f(j) is correctly computed and that it would be correctly shifted k digits before being incorporated into the
; partial result (as we already proved), yes!

; The next term to be added has the form f(j)00..0, where the number of zeroes = k.
; Observe that (* (f (remainder a 10)) ((expo 10) k)) applies f to the ones digit to obtain the f(j) and then
; multiplies the result by 10^k, giving it k trailing zeroes.

; The invariant is true in the first call since k = (count-digits (f (remainder n 10))) (the count of the digits
; in the ones digit of n), result-so-far = (f (remainder n 10)) (f applied to the ones digit of n) and a = (quotient n 10)
; (n with its ones digit removed).

; It is also true in the last call when a = 0, which signifies that there are no digits left to process <=> we have processed
; all of them and our partial result is now a complete result, and
; result-so-far is correctly returned.

; Will there always be a last call (that is, will the program always terminate)? Given that a is replaced by (quotient a 10) on every
; next call and it cannot be negative (since n is not negative), it will at some point lose all its digits and become 0.
; ---------------------------------------------------------------------------------------------------------------------------

; Problem 4 (10 points) The function

(define add-1
  (lambda (y) (+ y 1)))

; can be generalized

(define add-x
  (lambda (x)
    (lambda (y)
      (+ x y))))

; so that add-1 can be realized as (add-x 1).

; In this problem, you are asked to go a step further, and generalize this pattern to allow its use with any function
; of two arguments (not just +).  Name the new function curry-2, and show how to use it to realize add-x.  Show
; further how to use it to define a function expo-b, which inputs a non-negative integer e and outputs b raised to the eth
; power (you can use the scheme primitive, expt).

; No proofs are required for problem 4.

; ---------------------------------------------------------------------------------------------------------------------------
(define curry-2
  (lambda (op)
    (lambda (x y)
      (op x y))))

(display "\nProblem 4 testing\n")

((curry-2 +) 5 6) ; 11

; add-x would be realized as (curry-2 +)

(define expo-b
  (lambda (b)
    (lambda (e)
      ((curry-2 expt) b e))))

((expo-b 5) 0) ; 1
((expo-b 5) 1) ; 5
((expo-b 5) 2) ; 25
((expo-b 5) 3) ; 125


; alternatively,

(define curry-2
  (lambda (op) ; op must be a function of two arguments, x and y
    (lambda (x)
      (lambda (y)
        (op x y)))))

(((curry-2 +) 5) 6) ; 11

(define expo-b
  (lambda (b)
    (lambda (e)
      (((curry-2 expt) b) e))))

((expo-b 5) 2) ; 25