; CSc 335
; Spring 2018

; March 6

; First Midterm Exam - 2 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 10 points)
;;;; Problem 1 - proof (out of 5 points
;;;; Problem 1 - synergy: connection between proof and code (out of 5 points)

;;;; Problem 2 - code (out of 20 points)
;;;; Problem 2 - proofs (out of 10 points)
;;;; Problem 2 - synergy: proof and code developed at the same time (out of 10 points)

;;;; Problem 3 - code (out of 12 points)
;;;; Problem 3 - discussion of specification and proof (out of 8 points)

;;;; Problem 4 - code (out of 10 points)
;;;; Problem 4 - proofs (out of 10 points)



;;;; Total
;;;; Letter Grade

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

; Problem 1 

; It is conjectured that the sum of the cubes of the first n positive integers is equal to the square of the sum of the first n
; positive integers.  That is, that

;         (1^3 + 2^3 + ... + n^3) = (1 + 2 + ... + n)^2

; Write and prove correct an iterative program to check this conjecture.  Your program must either return #t, if the conjecture
; is true for the input value of n, and otherwise the first integer <= n for which the conjecture is false.  Your program should
; be reasonably efficient -- for example, no cube should be computed more than once.  

; INSERT YOUR ANSWER HERE

(define (p n)
  (define (square x) (* x x))
  (define (cube x) (* x x x))
  (define (p-iter a lhs rhs result-so-far)
    (cond ((not result-so-far) a)
          ((= a n) result-so-far)
          (else (let ((next-lhs (+ lhs (cube (+ a 1))))
                      (next-rhs (+ rhs (+ a 1))))
                  (p-iter (+ a 1)
                          next-lhs
                          next-rhs
                          (equal? next-lhs (square next-rhs)))))))
  (p-iter 1 1 1 #t))


; Precondition: n is a positive integer
; Postcondition: if the conjecture is true for the given n (and for all positive integers <= n) then #t is returned; otherwise
; the first integer i, 0 <= i <= n, for which the conjecture is false is returned.
;
; a is the variable that counts from 0 up to n.
; lhs is the value of the lefthand side.
; rhs is a misnomer: it is not the value of the whole rhs but just the value of the sum in the parentheses.
; However, we can square this number to get the value of the actual righthand side of the equation.
; result-so-far is true if and only if lhs = (rhs)^2 for all integers i, 1 <= i <= a.
; The AND of the above rules is our invariant.
;
; In the initial call,
; a = 1,
; lhs = 1,
; rhs = 1,
; result-so-far = #t.
; 
; Clearly the invariant holds on the first call because, for a = 1, lhs = (rhs)^2 = 1, and there is only one
; number i, 1 <= i <= 1, to check.
;
; If the invariant holds on the kth call, then does the invariant hold on the (k+1)st call?
; Suppose on the kth call that we have lhs = (rhs)^2 for all integers from 1 to a.
; On the (k+1)st call, we increment a by 1, which means we simply have to check whether the conjecture holds
; for this new value of a (we know it held for all integers from 0 to a-1).
; 
; If and only if the equation holds for this new value of a should result-so-far be set to true;
; otherwise it should be set to false.
; 
; In the kth call, we checked this by comparing next-lhs and (next-rhs)^2 for equality.
; Thus on the (k+1)st call, the value of result-so-far is updated with respect to the new values of a, lhs, rhs
; (it reflects whether lhs = (rhs)^2 for all integers from 1 <= i <= a).
; 
; On the (k+1)st call, we get the assignments
; a <- a+1
; lhs <- lhs + (a+1)^3
; rhs <- rhs + (a+1)
; result-so-far <- #t (iff lhs = (rhs)^2)
; 
; Clearly, the lhs is updated correctly: (1^3 + 2^3 + ... + (a-1)^3) + a^3 = 1^3 + 2^3 + ... + a^3.
; The rhs is also updated correctly: (1 + 2 + ... + n-1) + n = 1 + 2 + ... + n.
;
; Thus, the test that determines the next value of result-so-far, lhs = (rhs)^2, is correct.
; 
; In addition to terminating when the test on the new value of a fails (in which case a is correctly returned),
; the program terminates when a = n (none of the individual tests failed, and we successfully counted up to n).
; 
; The invariant is strong enough to imply that the returned value, result-so-far, is correct because when a = n,
; by the invariant we have that the conjecture holds for all integers i, 1 <= i <= n.
; -------------------------------------------------------------------------------------------------------------------------------
              
; Problem 2

; We say tnat n is sorted if its digits, read left to right, are in increasing order.  Thus 1457 is sorted, while 7154 is not.
; In this problem you are asked to write and prove a properly recursive version of insertion sort which inputs a nonnegative integer and
; which outputs a sorted integer consisting of the same digits in sorted order.  Thus (sort 7154) = 1457

; Your main procedure, sort, must make use of an auxiliary procedure, insert.  Both sort and insert must be properly recursive; both
; functions must be proved correct.  You will recall from your Algorithms class that insert is a procedure which (in the present
; context) takes a sorted integer n and a new digit d, and returns the sorted integer obtained from n by inserting d into its correct
; position.  (Additional parameters may be desirable in the setting of the current problem.)  

; You may may find the built-in function expt to be of use.    

; INSERT YOUR ANSWER HERE

(define (insert d n)
  (let ((ones-digit (remainder n 10)))
    (cond ((> d ones-digit) (+ (* 10 n) d)) ; insertion occurs here
          ((= n 0) d) ; or here
          (else (+ (* 10 (insert d (quotient n 10))) ones-digit)))))

; Say we want to insert d = 4 into n = 12356.
; Given that d < (ones-digit of n) = 6, we can just insert it into (quotient n 10) = 1235.
; The result of that insertion would be 12345.
; To re-append the 6, multiply the result by 10, giving us 123450,
; and then add 6, giving us 123456.

; We can come up with this recursive solution just by understanding how recursion works: in a recursive call,
; the input or the instance of the problem always decreases in size. Obviously d is not the one to diminish; so
; n is. When the ones-digit of n is greater than d, we reduce the problem to inserting d into (quotient n 10).
; We then combine the result of that insertion with the ones-digit that we ignored in the beginning.

; We require that each digit be between 1 and 9, because inserting 0 at the beginning would cause
; it to disappear. We also require that n is already sorted and that d is a one-digit integer.

; In the base case, n = 0 -- we insert d into 0 -- so we just return d.

(define (sort n)
  (if (zero? (quotient n 10))
      n
      (insert (remainder n 10) (sort (quotient n 10)))))

; Insertion sort recursively sorts all the digits of n except its ones digit and then inserts its ones-digit into the proper
; place of that sorted result.
; In the base case, n is a one-digit number.

; Both sort and insert are guaranteed to terminate: they both involve replacing the original number n with (quotient n 10), which is n
; with one less digit. At some point, the number n diminishes to nothing.

; Remember that insert requires that its argument n be already sorted. It is clear by the line
; (insert (remainder n 10) (sort (quotient n 10)))
; that the ones digit is inserted into an already sorted number.
; -------------------------------------------------------------------------------------------------------------------------------

; Problem 3

; Now you are asked to abstract sort to accept a predicate pred as well as a nonnegative integer num.  Thus (sort num <) will
; realize the function you implemented in Problem 2, while (sort num >) will sort a number so that its digits are in decreasing order.
; Obviously, many other specializations -- such as (sort num >=) -- would be possible.

; Give a complete specification -- for example, must pred be transitive?

; Give complete code.

; While you do not need to give a complete proof, you should explain carefully how the complete proof would differ from the one you give
; for the code you produced for problem 2.  


; INSERT YOUR ANSWER HERE

(define (sort-abstr n pred)
  (define (insert d n)
    (let ((ones-digit (remainder n 10)))
      (cond ((not (pred d ones-digit)) (+ (* 10 n) d)) ; insertion occurs here
            ((= n 0) d) ; or here
            (else (+ (* 10 (insert d (quotient n 10))) ones-digit)))))
  (if (zero? (quotient n 10))
      n
      (insert (remainder n 10) (sort-abstr (quotient n 10) pred))))

(sort-abstr 24351 >) ; 54321
(sort-abstr 24351 <) ; 12345

; Precondition: pred is a function that takes two numbers as input and compares them, returning #t if they pass the test and #f otherwise,
; and must represent a relation that is transitive (a < b and b < c => a < c) so that the digits may be ordered in such a way that the digits
; of n can collectively be said to be in order from left to right from right to left.
; Postcondition: the result of rearranging the digits in n so that they follow the ordering (from left to right) imposed by pred is returned.

; This has essentially the same code as sort except the auxiliary insert replaces < with pred. As long as pred is true, we keep recursing:
; when it is found to not be true, then we stop and insert.

; Sort-abstr is the same since sort-abstr just calls insert.
; -------------------------------------------------------------------------------------------------------------------------------

; Problem 4

; Write and prove correct a procedure binom which inputs a positive integer n and which returns a function of integer k and
; numbers a and b such that ((binomial n) k a b) is the kth term in the binomial expansion of (a + b)^n.   You will recall that
; this term is the product of the kth entry in the nth row of Pascal's triangle with a^k and b^(n - k).
; (n choose k) * a^k * b^(n - k)

; You will need to write - and prove - a procedure to compute a given entry in Pascal's triangle, as well as the procedure binom.


; INSERT YOUR ANSWER HERE

; The kth entry in the nth row of Pascal's triangle is n choose k.
; The kth term in the binomial expansion of (a + b)^n is (n choose k) * a^k * b^(n - k)
; a^k and b^(n - k) can be computed just using expt since we are given a, k, b and n.

; What we need to do is find a way to compute n choose k.
; We can compute it recursively by means of the equality (n choose k) = ((n-1) choose k) + ((n-1) choose (k-1)).
; In the base case we have that (n choose 0) = 1 and n choose k = 1 if n = k.

(define (choose n k)
  (cond ((or (= k 0) (= n k)) 1)
        (else (+ (choose (- n 1) (- k 1))
                 (choose (- n 1) k)))))

(define (binom n)
  (lambda (k a b)
    (* (choose n k) (expt a k) (expt b (- n k)))))

; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
; ...

; In the base case, the entire first and second row of Pascal's triangle is correctly computed,
; as are the outer numbers, which are all 1's because k = 0 or n = k.
 
; In the inductive case, n choose k is computed by computing ((n-1) choose k) and ((n-1) choose (k-1)).
; Because all the leftmost and topmost terms are correctly computed in the base case, going up a row and possibly also to the
; left in order to compute the middle terms is always possible.
 
; We can prove correctness by induction on the row number (the value of n). The first and second rows (n = 0, 1) are computed
; correctly just using the base case.
; For the inductive case (n >= 2), it can be shown that if the nth row is correctly computed, the (n+1)st
; is also correctly computed.

; The last entry in any given row has no entry above it
; but that isn't a problem because it falls in the base case.
; Similarly, the first entry of any given row falls in the base case.
; Each middle term always has an entry above it and an entry to the left of that entry.
; -------------------------------------------------------------------------------------------------------------------------------

