

; CSc 335
; Fall 2015

; November 17, 2015

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE: 

; TYPE YOUR FULL EMAIL ADDRESS HERE: 
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 10 points)  
;;;; Problem 1 - proof (out of 10 points) 

;;;; Problem 2 - code (out of 10 points)  

;;;; Problem 3 - code (out of 20 points)  

;;;; Problem 4 - code (out of 10 points)  
;;;; Problem 4 - proof (out of 10 points) 

;;;; Problem 5 - code (out of 15 points)  
;;;; Problem 5 - proof (out of 15 points) 

;;;; Total  
;;;; Letter Grade  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no strings, no assignment.  

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or
;   an arbitrary symbol, which we call a variable

; Here l1 and l2 are arbitrary L-expressions, so this is an inductive definition once 
; we add 'and nothing else is an L-expression'

; For the following, you need give proofs only for the interesting functions - small subfunctions (especially those
; which are neither iterative nor recursive) need not be proved. For the interesting functions, proofs should 
; display your understanding of a proper integration of subfunction specifications: if a function f calls a subfunction
; g, you need to show that the pre-condition of g is satisfied when it is called, and also how f makes use of 
; the postcondition of g when g returns. 

; Pre- and post-conditions should be given for all functions which are not predefined in Scheme. 


; (1) Write and certify a function lexp? which checks whether an S-expression is an L-expression.

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (lexp? sexp)
  (cond ((null? sexp) #f)
        ((atom? sexp) #t)
        ((or (eq? (car sexp) 'AND)
             (eq? (car sexp) 'OR)) (and (= (length sexp) 3)
                                        (lexp? (cadr sexp))
                                        (lexp? (caddr sexp))))
        ((eq? (car sexp) 'NOT) (and (= (length sexp) 2)
                                    (lexp? (cadr sexp))))
        (else #f)))

; lexp ::= atom | (AND lexp lexp) | (OR lexp lexp) | (NOT lexp).

; According to this recursive definition, an sexp is an lexp (logical expression) if
; (i) the sexp's car is NOT, the sexp has length 2 and the 2nd element is itself an lexp,
; (ii) the sexp's car is AND or NOT, the sexp has length 3 and the 2nd and 3rd elements are themselves lexps, or
; (iii) the sexp is an atom.

; If we can test to see whether the individual elements of an sexp are lexps (induction hypothesis)
; then deciding whether the entire expression is an lexp is just a matter of checking whether the car is NOT, AND or OR
; (which indicates what type of l-expression it is), and then whether the number of other elements in the sexp matches to the type,
; and then whether the other elements are themselves lexps (induction step). This is structural induction: induction on
; the recursively defined structure of an lexp.

; (2) Design a datatype (constructors, selectors, classifiers) for use with programs which work with L-expressions.  A proof
;     is not requested (and besides: first we'd have to talk about what, exactly, a proof of a datatype might be!)

; constructors for creating lexps

(define (make-and e1 e2) ; precondition: e1 and e2 are lexps. postcondition: return the lexp for "e1 and e2".
  (list 'AND e1 e2))
(define (make-or e1 e2) ; precondition: e1 and e2 are lexps. postcondition: return the lexp for "e1 or e2".
  (list 'OR e1 e2))
(define (make-not e) ; precondition: e is an lexp. postcondition: return the lexp for "not e".
  (list 'NOT e))

; selectors for accessing the different parts of an lexp

(define (operator e) ; precondition: e is any lexp. postcondition: return the lexp's operator.
  (car e))
(define (first-operand e) ; precondition: e is any lexp. postcondition: return the first operand of the lexp.
  (cadr e))
(define (second-operand e) ; precondition: e is an AND or OR lexp. postcondition: return the second operand of the lexp.
  (caddr e))

; classifiers for testing what type of lexp an lexp is.

(define (and-exp? e)
  (eq? (operator e) 'AND))
(define (or-exp? e)
  (eq? (operator e) 'OR))
(define (not-exp? e)
  (eq? (operator e) 'NOT))


; (3) Write a function covered? of an L-expression lexp and a list of symbols los that tests
;     whether all the variables in lexp are in los; your function should make use of your solution to 
;     Problem (2).  A proof is not requested.  


(define (deep-member? x lst) ; takes a list and an s-expression x and returns #t iff x is in it
  (cond ((null? lst) #f)
        ((atom? (car lst)) (or (eq? (car lst) x) (deep-member? x (cdr lst))))
        (else (or (deep-member? x (car lst)) (deep-member? x (cdr lst))))))

; a list has three forms: (), (list list) and (atom list). The base case takes care of the first form.
; If the first component is an atom, x is either equal to it or is a member of the second component if x exists in the list.
; If the first component is a list, it is a member of the first component or the second if x exists in the list.

(define (covered? lexp los)
  (cond ((atom? lexp) (deep-member? lexp los))
        ((or (and-exp? lexp) (or-exp? lexp)) (and (covered? (first-operand lexp) los) (covered? (second-operand lexp) los)))
        (else (covered? (first-operand lexp) los))))
  

; (4) For the evaluation of L-expressions we need association lists, or alists.  An alist for
;     L-expressions is a list of (variable, value) pairs.  The variable component is always a symbol, and
;     the value component is either the number 0 (for false) or 1 (for true). Write and certify an iterative function
;     lookup of the symbol var and the association list al, so that (lookup var al) returns the 
;     value of the first pair in al whose car is eq? to var.

(define (lookup var al)
  (let ((next-pair (car al)))
    (cond ((eq? (car next-pair) var) (cadr next-pair))
          (else (lookup var (cdr al))))))

; precondition: var is an atom that exists in al, a nonempty list of (variable, value) pairs.
; postcondition: return the first pair in al whose car is eq? to var.

; If the car of the next pair is eq? to var, then return the cadr (the value) of the pair.
; Otherwise keep looking in (cdr al).

; (5) If the list of symbols in an alist for L-expressions contains all the variables of an L-expression
;     lexp, then lexp is called _closed_ with respect to this alist.  A closed L-expression can be evaluated,
;     essentially by substituting the values of the variables given in the alist for the variable occurrences
;     in the L-expression.  You are asked to write and certify a function (name it value) of an L-expression 
;     lexp and an alist al, which, after verifying that lexp is closed with respect to al,
;     determines whether lexp means true or false.  If lexp is not closed with respect to al, then (value lexp al)
;     should return the symbol not-covered.  Your solution should make use of at least one higher-order function
;     which returns function values; it should also make use of your solutions to problems (2), (3) and (4). 

(define (my-and a b)
  (if (and (eq? a 'true) (eq? b 'true))
      'true
      'false))
(define (my-or a b)
  (if (or (eq? a 'true) (eq? b 'true))
      'true
      'false))
(define (my-not x)
  (cond ((eq? x 'false) 'true)
        ((eq? x 'true) 'false)))

(define (truth-value n)
  (cond ((= n 1) 'true)
        ((= n 0) 'false)))

(define (value lexp al)
  (cond ((not (covered? lexp al)) 'not-covered)
        ((atom? lexp) (truth-value (lookup lexp al)))
        ((and-exp? lexp) (my-and (value (first-operand lexp) al)
                                 (value (second-operand lexp) al)))
        ((or-exp? lexp) (my-or (value (first-operand lexp) al)
                               (value (second-operand lexp) al)))
        ((not-exp? lexp) (my-not (value (first-operand lexp) al)))))

; The value of an lexp is determined by the value(s) of its component lexp(s).
; my-and, my-or and my-not test those values to determine the value of an lexp.
; The value of an atomic lexp is true if its value in the association list is 1 or false if it's 0.


(define al '((a 0) (b 1) (c 0) (d 0) (e 1)))

(define w (make-not 'b)) ; #f
(define x (make-not 'a)) ; #t
(define y (make-or 'a 'b)) ; #t
(define z (make-or 'c 'd)) ; #f
(define v (make-and 'b 'e)) ; #t
(define g (make-and 'c 'd)) ; #f
(define h (make-and 'a 'b)) ; #f

(value w al)
(value x al)
(value y al)
(value z al)
(value v al)
(value g al)
(value h al)
