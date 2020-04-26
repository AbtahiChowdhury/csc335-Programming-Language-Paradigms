

; CSc 335
; Fall 2016

; November 3

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: Muhammad Rehman

; TYPE YOUR FULL EMAIL ADDRESS HERE: mrehman000@citymail.cuny.edu
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - code (max 10 points)  4

;;;; Problem 1b - proof (max 10 points)  2

;;;; Problem 1c - code (max 10 points)  7

;;;; Problem 1d - proof (max 10 points) 5

;;;; Problem 1e - code (max 10 points)  5

;;;; Problem 1f - proof (max 10 points) na



;;;; Problem 2a - code (max 10 points) 9

;;;; Problem 2b - code (max 15 points) 5

;;;; Problem 2c - proof (max 15 points) na


; Total Code Score (max 55 points)   30

; Total Proof Score (max 45 points)  7

; Total Score (max 100 points)  37

;;;; Letter Grade  D

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the R5RS
; implementation provided by drracket and only those language features discussed so far in the
; context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones and smart watches and smart rings and ... are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Note that you will need to furnish specifications for your functions before you can prove them correct.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1a.  (10 points) Write a pair of mutually recursive programs odd-indexed-elements and even-indexed-elements to produce from one list L
; the two lists consisting, respectively, of the elements of L with even index and the elements of L with odd index.  For example,
; (odd-indexed-elements '(a b c d e f g h)) = '(b d f h) and (even-indexed-elements '(a b c d e f g h)) = '(a c e g)

(define (even-indexed-elements lst)
  (define (aux lst n)
    (cond
      ((null? lst) lst)
      ((equal? n 0) '())
      ((equal? (modulo n 2) 0)
       (aux (cdr lst) (- n 1)))
      (else
        (cons (car lst) (aux (cdr lst) (- n 1))))))
  (aux lst (- 1 (length lst)) ))

(define (odd-indexed-elements lst)
  (define (aux lst n)
    (cond
      ((null? lst) lst)
      ((equal? n 0) (car lst))
      ((equal? (modulo n 2) 0)
       (cons (car lst) (aux (cdr lst) (- n 1))))
      (else
       (aux (cdr lst) (- n 1)))))
  (aux lst (- 1 (length lst)) ))

(display "test for odd\n")
(odd-indexed-elements '(a b c d e f g h))
(odd-indexed-elements '(0 1 2 3 4 5 6 7 8 9 10 11))
(odd-indexed-elements '(a))

;;;; should return '(), not a


(display "test for even\n")
(even-indexed-elements '(a b c d e f g h))
(even-indexed-elements '(0 1 2 3 4 5 6 7 8 9 10 11))
(even-indexed-elements '(1))

;;;; should return '(1), not '()


;;;; your functions are not mutually recursive!  

; 1b.  (10 points) Prove that both functions are correct (Hint: use one proof for both function).
;Let f-even = even-indexed elements
;Let f-odd = odd-indexed elements

;Guess invariant: f-odd returns a list with odd indexed elements/f-even returns a list with even indexed elements


;;;; these are not suitable guess-invariants!



;;;; with two independent functions, it would actually be best to offer two separate proofs.



;Correct on termination: The program terminates when n == 0, and this happens because n is decremented on each
;                        recursive call to aux. Upon termination n == 0, and we get the last element that is cons with the rest of the
;                        elements that satisfy our condition, which is either odd or even index
;Correct on first call: If we have a null list, this means length == 0, and n == 0, then we return a null list because there are no
;                       elements with an index. If we have a list with elements, we get the length n, and figure out if this is an odd or even index
;                       using the modulo. For f-even, if n%2==0, we cons (car lst) aux with n-- because that is an even index we want to keep. For f-odd, if
;                       n%2 == 0, we ignore the (car lst) because that is not odd and call aux with n--. 
;Correct on the next call: It is correct on the next call because we decrement the value of n and we get closer to termination which is 0. 
;                          We continue checking n%2 and doing the appropriate call for the function.
;Terminates?: Yes the program terminates because n decrements on each call of aux. 



; 1c.  (10 points)  Write a procedure merge which takes two lists of numbers, with each list sorted in order from smallest to largest, which
; produces a third list that consists of all the (distinct) elements of the original lists and is also in sorted order.

(define (merge l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    ((<= (car l1) (car l2))
     (cons (car l1) (merge (cdr l1) l2)))
    (else
     (cons (car l2) (merge l1 (cdr l2))))))

   
(merge '(1 2 5) '(1 4 7))

;;;; as your example shows, you have not solved the problem of avoiding duplicates in the output.  You could do this by checking another case -- (= (car l1) (car l2))

;;;; missing pre-condition to ensure absence of duplicates as well



; 1d.  (10 points) Prove that your merge procedure is correct.
;Precondition: l1 and l2 are sorted lists

;;;; need to add: without duplicates


;;;; start off by stating clearly what you are inducting on -- in this case, we induct simultaneously on the lengths of l1 and l2 -- ie, the recursive calls
;;;; work because one argument or the other (or both, for the case you have omitted) has been shortened





;Basis Step: When l1 and l2 are null lists, then you just return the other list, because you don't have to merge anything

;Induction Hypothesis: the recursive calls work
;Induction Step: When merge is called, it checks whether the first element in the l1 is <= the first element in l2. If it is then you make
               ; the recurisve call which merges the (cdr l1) with l2 because now we want to compare the second element of l1 with the first in l2.
;                The program continues to take the cdr of each list depending on the recursive call and shortening it down to a empty list.

;;;;; this amounts to a ... kind of argument, rather than an induction!


;Termination: The program terminates because the recursive call takes the cdr of each list which eventually results in an empty list. 
; 1e.  (10 points) Using your procedures for 1a and 1c, write a procedure merge-sort which inputs a single (unsorted) list of numbers and returns the list
; consisting of the elements of the input list, in sorted order.

(define (merge-sort l)
  (define (aux1 left right)
    (cond
      ((null? l) (quote()))
      (else
       (merge (merge-sort left) (merge-sort right)))))
  (aux1 (even-indexed-elements l) (odd-indexed-elements l)))

;(merge-sort '(1 5 6 2 3 0))


;;;;; throws  a type error when evaluated

;;;; logic problems



  
; 1f.  (10 points)  Prove that your merge-sort procedure is correct.



; 2a.  (10 points) Write a deeply recursive version of the generalized member? function, that is, your function deep-member? ought to take both a predicate
; and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.  For example,

; (deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t.  NOTE that I am not requesting a proof for this function.
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
(define (deep-member? p tr)
  (cond
    ((null? tr) #f) ;;;; couldn't the predicate hold for '()?
    ((atom? tr)
     (p tr)) ;;;; good
    ((p tr) #t) ;;;; good
    (else  ;;;; good
     (or (deep-member? p (car tr))
         (deep-member? p (cdr tr))))))




 (deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))
(deep-member? (lambda (t) (equal? t 4)) '(1 2 3 7))




; 2b.  (15 points) Write a procedure remove-left-most which inputs a predicate and a tree, and which removes only the left-most subtree
; of the input tree for which the predicate is true.  For example,

; (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;
; =
;
; '((a (b c)) ((f (g (c (d e))))))


(define (remove-left-most p tr)
  (cond
    ((null? tr) (quote()))
    
;;;; what if tr is an atom?
    
    ((atom? (car tr))
     (cond
       ((deep-member? p tr)
        (cdr tr)))) ;;;; logic
       
    ((deep-member? p tr)
     (cdr tr)) ;;;; logic
    (else
     (cons (remove-left-most p (car tr))
           (remove-left-most p (cdr tr))))))

 (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))

;;;; does not remove anything!



(remove-left-most (lambda (t) (equal? t 4)) '(1 3 7 4))


;;;; does not remove the right thing





; Hint: use your deep-member? function.

; 2c.  (15 points) Prove that your remove-left-most procedure is correct. 













