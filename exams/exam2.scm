; CSc 335
; Fall 2018

; November 15

; Second Midterm Exam - 2 hours

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Seyson Chen

; TYPE YOUR FULL EMAIL ADDRESS HERE: schen023@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 25 points)  24
;;;; Problem 1 - proof, including synergy (out of 25 points)  24

;;;; Problem 2 - code (out of 10 points)  7
;;;; Problem 2 - proof, including synergy (out of 10 points)  na

;;;; Problem 3 - code (out of 10 points)  8

;;;; Problem 3b - code (out of 10 points)  9
;;;; Problem 3b - proof, including synergy (out of 10 points)  8
 


;;;; Total  80
;;;; Letter Grade  A-

;;;; nice work, as I have come to expect from you :  keep it up!

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


; Problem 1 (50 points).  A simple recursive idea for finding all prime factors of a positive integer n can be described as follows:

;;     prime-factors(1) = {} (the empty set)
;;     prime-factors(p) = {p} if p is prime
;;     prime-factors(n) = prime-factors(a) U prime-factors(b), where n = ab

; Using this approach, develop a complete R5RS program prime-factorization which inputs an
; integer n >= 1 and returns a list of pairs of the form <prime, power>,
; where prime occurs power times in the prime factorization of n.  Thus

;;    (prime-factorization 1) = ()
;;    (prime-factorization 7) = ((7 1))
;;    (prime-factorization 30) = ((2 1) (3 1) (5 1))
;;    (prime-factorization 100) = ((2 2) (5 2))
;;    (prime-factorization 1024) = ((2 10))

; The output list must be sorted by primes, with smaller prime numbers appearing before
; larger prime numbers.

; In addition,
;
;   the factors a and b of n must be chosen as close in size to each other as possible (for 200, for example, a = 10 and b = 20)
;
;   your code must be purely functional.
;
;   complete proofs of your major functions must be included.



; INSERT YOUR ANSWER HERE

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ 1 low) high))))

(define (prime? n)
  (define (aux count)
    (cond ((> count (truncate (sqrt n))) #t)
          (else (if (= (modulo n count) 0)
                    #f
                    (aux (+ count 1))))))
  (if (< n 2)
      #f
      (aux 2)))

(display "Problem 1 testing: prime-factorization\n")

(define (filter pred seq)
  (cond ((null? seq) seq)
        (else (if (pred (car seq))
                  (cons (car seq) (filter pred (cdr seq)))
                  (filter pred (cdr seq))))))

(filter prime? (enumerate-interval 0 17)) ; (2 3 5 7 11 13 17)

; The primality test for an integer n consists of checking whether the divisors d divide n (i.e., whether (modulo n d) = 0),
; where 2 <= d <= sqrt(n), i.e.,  <= d <= floor(sqrt(n)). The invariant is that we have checked the divisors up to count-1.
; The procedure halts only in two cases:
; (a) the test (= (modulo n count) 0) returned true, which means d divides n, so we halt and return #f
; (b) count has exceeded floor(sqrt(n)), which means we have tested all potential divisors and not halted, so we return #t.

; The invariant holds in the base case because 2 is the first divisor to check, and we have checked the divisors up to 1
; (i.e., we haven't checked anything yet). If it holds on the kth call that we've checked all divisors up to d,
; it holds on the (k+1)st call because during that call we check the divisor d+1 and continue iterating iff it did not
; properly divide n.

; Count goes from 2 to the point where it exceeds sqrt(n) (n > 2) with step size 1, so it will eventually halt
; because at some point, count > sqrt(n).

;;;; good


; First we want a way to generate the list of factors of any number n:

(define (list-of-factors n)
  (filter (lambda (x) (= (modulo n x) 0)) ; filter out non-factors
          (enumerate-interval 1 n)))

(list-of-factors 30) ; (1 2 3 5 6 10 15 30)

; Because it might be useful, list of factors only up to sqrt(n):

(define (list-of-factors-up-to-sqrt-n n)
  (filter (lambda (x) (= (modulo n x) 0)) ; filter out non-factors
          (enumerate-interval 1 (truncate (sqrt n)))))

(define (last-element lst)
  (cond ((null? (cdr lst)) (car lst))
        (else (last-element (cdr lst)))))

; The last element of list-of-factors-up-to-sqrt-n is the factor that is closest to its "other":

;;;; good!

; Now we want to be able to map a list of factors of n to unique (factor other) pairs where factor1 * other = n.

(define (factor-pairs list-of-factors n)
  (map (lambda (i) (list i (/ n i)))
       list-of-factors))

; prime-factors(1) = {} (the empty set)
; prime-factors(p) = {p} if p is prime
; prime-factors(n) = prime-factors(a) U prime-factors(b), where n = ab

(define (prime-factors n)
  (cond ((= n 1) '())
        ((prime? n) (list n))
        (else (let* ((a (last-element (list-of-factors-up-to-sqrt-n n)))
                     (b (/ n a)))
                 (append (prime-factors a) (prime-factors b))))))

; Now we need to sort and remove duplicates

(define (insert x lst)
  (cond ((null? lst) (list x))
        ((< x (car lst)) (cons x lst))
        (else (cons (car lst) (insert x (cdr lst)))))) ; x > (car lst), so insert in (cdr lst)

; If insert works on (cdr lst), it works on lst by inserting it before the car iff it is < the car.
; cdr and car are components of lst, so recursing eventually stops
; when it drops down to the base case definition of a lst: null.

(define (insertion-sort lst)
  (cond ((null? lst) lst)
        (else (insert (car lst) (insertion-sort (cdr lst))))))

; inserting the first element properly in the sorted cdr of the list results in a sorted list.
; (cdr lst) removes an element and lst eventually loses all its elements and recursing stops.

(define (remove-duplicates lst)
  (cond ((null? lst) lst) ; empty list has no duplicates
        ((null? (cdr lst)) lst) ; one-element list has no duplicates
        (else (if (eq? (member (car lst) (cdr lst)) #f)
                  (cons (car lst) (remove-duplicates (cdr lst)))
                  (remove-duplicates (cdr lst))))))

; if we can remove the duplicates in (cdr lst), we can remove the duplicates in lst by
; attaching (car lst) iff it does not occur a second time in the lst.

(remove-duplicates (insertion-sort (prime-factors 30))) ; (2 3 5)
(remove-duplicates (insertion-sort (prime-factors 100))) ; (2 5)
(remove-duplicates (insertion-sort (prime-factors 1024))) ; (2)

(define (prime-factorization n)
  (remove-duplicates (insertion-sort (prime-factors n)))) ; we need to map this list (prime ... prime)
                                                          ; to a list of <prime, power> pairs


; The proof would be by induction according to the definition of prime-factors. If prime-factors works for a and b (a, b < n)
; then it will work for n = a*b. In the base case the prime factors of a prime is just that prime, or the prime factors
; of 1 is empty.


;;;; instead of remove-duplicates, you might also just write a simple collection procedure


;;;; nice job! 




              
; Problem 2 (20 points).  Is an alternative approach to the problem of finding all prime factors of a positive integer n superior to the
; one described in Problem 1?  Give a complete solution making use of enumerate-interval and for which the primary function uses map.
; Your main functions should be proved correct, but proofs given for Problem 1 (eg, for the function prime?) need not be repeated.

;;;; HINT: think of a solution which does not need to use a sorting routine, but which nonetheless produces the same results as the program
;;;; for problem 1.  
   

; INSERT YOUR ANSWER HERE

; First we want a way to generate the list of factors of any number n:

(define (list-of-factors n)
  (filter (lambda (x) (= (modulo n x) 0)) ; filter out non-factors
          (enumerate-interval 1 n)))

(list-of-factors 30) ; (1 2 3 5 6 10 15 30)

; Because it might be useful, list of factors only up to sqrt(n):

(define (list-of-factors-up-to-sqrt-n n)
  (filter (lambda (x) (= (modulo n x) 0)) ; filter out non-factors
          (enumerate-interval 1 (truncate (sqrt n)))))

; List of prime factors:

(define (list-of-prime-factors n)
  (filter prime? (list-of-factors-up-to-sqrt-n n)))

; Then we map the list of prime factors to a list of (prime power) pairs:

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

; (define (factor-pairs list-of-factors n)
;   (map (lambda (i) (list i (<power>))))
;        list-of-factors))
; ??

(list-of-prime-factors 100) ; (2 5)
(list-of-prime-factors 30) ; (2 3 5)
(list-of-prime-factors 1024) ; (2)

; I could not figure out how to determine the power, but I wrote the factor-pairs function above that would map the list of
; factors to the list of the unique <factor, power> pairs.


;;;; think of the way we solved the problem of representing pairs as products of powers of two primes ...



; Problem 3a  (10 points).  Write a deeply recursive version of the generalized member? function, that is, your function deep-member?
; ought to take both a predicate and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.
; For example,

; (deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t. NOTE that I am not requiring a proof for 3a.  


; INSERT YOUR ANSWER HERE


; We check not only every subtree of the tree against pred but also call deep-member? on each individual subtree to check each of its own subtrees.
; If a tree is an atom, we just test the atom against pred. If a tree is empty, we return #f.

; tree ::= () | atom | (tree ... tree)

(define (deep-member? pred tree)
  (cond ((null? tree) #f)
        ((not (pair? tree)) (pred tree)) ; if tree is atom, just check tree

        ;;;; still, tree itself might satisfy pred, even when tree is not an atom
        
        ((pred (car tree)) #t) ; check the foremost subtree
        (else (or (deep-member? pred (car tree)) ; check the foremost subtree's own subtrees
                  (deep-member? pred (cdr tree)))))) ; check the remaining subtrees (including their subtrees)

(display "\nProblem 3a testing: deep-member?\n")

(deep-member? (lambda (t) (eq? t 1)) 1) ; #t
(deep-member? (lambda (t) (eq? t 1)) 2) ; #f
(deep-member? (lambda (t) (eq? t 1)) '(1)) ; #t
(deep-member? (lambda (t) (eq? t 1)) '(2 (3 (1)))) ; #t
(deep-member? (lambda (t) (eq? t 1)) '(2)) ; #f
(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g))))) ; #t
(deep-member? (lambda (t) (equal? t '(b c))) '((a (b c)) ((c (d e)) (f (g))))) ; #t
(deep-member? (lambda (t) (equal? t '(f (g)))) '((a (b c)) ((c (d e)) (f (g))))) ; #t
(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d 0)) (f (g))))) ; #f

; Problem 3b.  (20 points) Write and prove correct a procedure remove-left-most which inputs a predicate and a tree, and which removes only the left-most subtree
; of the input tree for which the predicate is true.  For example,

; (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;
; =
;
; '((a (b c)) ((f (g (c (d e))))))


; Hint: use your deep-member? function.


; INSERT YOUR ANSWER HERE

(display "\nProblem 3b testing: remove-left-most\n")

(define (remove-left-most predicate tree)
  (cond ((null? tree) '()) ; an empty list has no subtrees to remove
        ((not (pair? tree)) tree) ; an atom has no subtrees to remove ---

        ;;;; not so!  what if the atom satisfies predicate?
        
        ((predicate (car tree)) (cdr tree)) ; if the foremost subtree satisfies pred, we lose it
        (else (if (deep-member? predicate (car tree)) ; else the thing we're looking for is either in (car tree) or (cdr tree)
                  (cons (remove-left-most predicate (car tree)) (cdr tree))
                  (cons (car tree) (remove-left-most predicate (cdr tree)))))))

(remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; ((a (b c)) ((f (g (c (d e))))))
(remove-left-most (lambda (t) (equal? t '(a (b c)))) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; (((c (d e)) (f (g (c (d e))))))
(remove-left-most (lambda (t) (equal? t 'a)) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; (((b c)) ((c (d e)) (f (g (c (d e))))))
(remove-left-most (lambda (t) (equal? t 'f)) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; ((a (b c)) ((c (d e)) ((g (c (d e))))))
(remove-left-most (lambda (t) (equal? t '(g (c (d e))))) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; ((a (b c)) ((c (d e)) (f)))

; Proof:
; There are perhaps three base cases:
; (i) Tree is null, so we return null.
; (ii) Tree is an atom, so we return the tree itself, since it has no subtrees.
; (iii) Tree is a list, and its first element (car tree) satisfies pred, so we remove it by just returning (cdr tree).

; In the inductive case, we have to apply remove-left-most to (car tree) itself, which is itself a tree -- but only
; if deep-member? says that the target subtree is in there. Otherwise, we remove-left-most to (cdr tree), as the leftmost
; desired subtree of the cdr is also that of the entire tree (since it doesn't exist in the car).

; Inductive hypothesis: Remove-left-most correctly returns the (car lst) or (cdr lst) with the leftmost subtree that satisfies
; pred removed.
; Inductive step: In dealing with lst, we remove-left-most from the car if it appears, giving us the correct lst since there
; are no desired subtrees to consider to the left of the car. If deep-member? says it doesn't exist in the car, however,
; the leftmost subtree can only occur in the cdr: we've already checked all the potential subtrees to its left. So we
; rely on the call to remove-left-most on the cdr. In either case, we reattach the remainder of the lst (the cdr if
; we operated on the car and the car if we operated on the cdr).

; Since (car lst) and (cdr lst) are components of lst, lst will eventually be reduced to the base case of the recursive definition,
; in which which lst is an atom or null, so the program terminates.


;;;; the only problem I see here is your analysis of the second case, as I mentioned above.


