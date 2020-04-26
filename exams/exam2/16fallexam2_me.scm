

; CSc 335
; Fall 2016

; November 3

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - code (max 10 points)  

;;;; Problem 1b - proof (max 10 points)  

;;;; Problem 1c - code (max 10 points)  

;;;; Problem 1d - proof (max 10 points) 

;;;; Problem 1e - code (max 10 points)  

;;;; Problem 1f - proof (max 10 points) 



;;;; Problem 2a - code (max 10 points) 

;;;; Problem 2b - code (max 15 points) 

;;;; Problem 2c - proof (max 15 points)


; Total Code Score (max 55 points)   

; Total Proof Score (max 45 points)  

; Total Score (max 100 points)  

;;;; Letter Grade  

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

; see 18springexam2.scm

; 1b.  (10 points) Prove that both functions are correct (Hint: use one proof for both function).

; see 18springexam2.scm

; 1c.  (10 points)  Write a procedure merge which takes two lists of numbers, with each list sorted in order from smallest to largest, which
; produces a third list that consists of all the (distinct) elements of the original lists and is also in sorted order.

; see 18springexam2.scm

; 1d.  (10 points) Prove that your merge procedure is correct.

; see 18springexam2.scm
  
; 1f.  (10 points)  Prove that your merge-sort procedure is correct.

; see 18springexam2.scm

; 2a.  (10 points) Write a deeply recursive version of the generalized member? function, that is, your function deep-member? ought to take both a predicate
; and a tree as arguments, and return #t if the tree contains a subtree for which the predicate is true, and #f otherwise.  For example,

; (deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g)))))

; should return #t.  NOTE that I am not requesting a proof for this function.

; First, tree ::=  atom | () | (tree ... tree)

(define (deep-member? pred t)
  (cond ((null? t) #f) ; if t is ()
        ((not (pair? t)) (pred t)) ; if t is an atom
        (else (or (pred (car t)) ; check the car (leftmost subtree) against pred 
                  (deep-member? pred (car t)) ; check the car's subtrees against pred
                  (deep-member? pred (cdr t)))))) ; check the subtrees other than the leftmost subtree against pred

(deep-member? (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g))))) ; #t

; I assume that a tree is equivalent to an sexp. Thus, we must check not just every element in the list against pred, but also every
; element within every element.
; I did not consider a tree to be a subtree of itself. This would create a problem in calling deep-member? on the tree's cdr,
; as we would then be checking (cdr t) against pred, even though (cdr t) is not a subtree. For example, if t = '(1 2 3 (4 5)),
; (cdr t) = '(2 3 (4 5)), which is not a subtree of t. The subtrees are: 1, 2, 3 and (4 5). The subtree (4 5) also has deeper subtrees 4 and 5.
; So the plan is to take the car, the leftmost subtree, and test it against pred and store the result of the test before removing it from the list
; and repeating the steps on the next car. 
; This plan is embodied in the two expressions (pred (car t)) and (deep-member? pred (cdr t)).
; The third expression, (deep-member? pred (car t)), is to test the subtrees of the car itself. This is what makes the function "deep".
; Recursing stops when a test returns #t or when we reach the base case: t is an atom (or null).


; 2b.  (15 points) Write a procedure remove-left-most which inputs a predicate and a tree, and which removes only the left-most subtree
; of the input tree for which the predicate is true.  For example,

; (remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e)))))))
;
; =
;
; '((a (b c)) ((f (g (c (d e))))))

; Hint: use your deep-member? function.

(define (remove-left-most pred t)
  (cond ((null? t) t)
        ((not (pair? t)) t)
        ((pred (car t)) (cdr t))
        (else (if (deep-member? pred (car t))
                  (cons (remove-left-most pred (car t)) (cdr t))
                  (cons (car t) (remove-left-most pred (cdr t)))))))

(remove-left-most (lambda (t) (equal? t '(c (d e)))) '((a (b c)) ((c (d e)) (f (g (c (d e))))))) ; ((a (b c)) ((f (g (c (d e))))))

; 2c.  (15 points) Prove that your remove-left-most procedure is correct. 

; In the first base case, t is null, so returning null is correct (there are no subtrees to check)
; In the second base caes, t is an atom, so we return t, since there are no subtrees to remove.
; In the third base case, the car itself satisfies pred so we just return (cdr t).

; In the inductive case, it is clear that t is a list whose car itself does not satisfy pred. There may be a subtree belonging to
; (car t) that satisfies pred or a subtree in the (cdr t) that satisfies pred.

; Suppose for some pred that remove-left-most works on (car t) or (cdr t). That is, it removes the leftmost subtree for which pred
; is true (it returns the original list if such a subtree does not exist) from (car t) or (cdr t).

; Then remove-left-most will work on t because we simply remove the target subtree from the car if it exists in the car --
; i.e., (deep-member? pred (car t)) is true -- and cons the result to (cdr t); otherwise we remove it from the cdr, since the first
; subtree for which pred is true can only occur there, and we cons (car t) back to that result.


