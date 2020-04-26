

; CSc 335
; Spring 2018

; April 17

; Second 2 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (out of 10 points)
;;;;           - proof, and synergy of proof and code (out of 10 points)

;;;; Problem 2a - code (out of 10 points)
;;;; Problem 2b - code (out of 10 points)

;;;; Problem 3 - code (out of 25 points)
;;;;           - proof, and synergy of proof and code (out of 10 points)

;;;; Problem 3b EXTRA CREDIT - code (out of 10 points)

;;;; Problem 4a - code and proof and synergy (out of 10 points)
;;;; Problem 4b - code and proof and synergy (out of 15 points) 

;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using R5RS within drracket,
; and using only language features discussed so far in the context of the homework: no strings, no vectors, no assignment.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.  


; Problem 1 (20 points) Write a certify an iterative program, using lists, named
; nth-row-of-pascal-triangle which inputs a positive integer n and which outputs
; the nth row of the pascal triangle,

; Thus (nth-row-of-pascal-triangle 3) returns the list (1 2 1), and
; (nth-row-of-pascal-triangle 4) returns (1 3 3 1).  Your program should of course
; work for other positive integers n -- for example, (nth-row-of-pascal-triangle 10)
; should return (1 9 36 84 126 126 84 36 9 1)

; (The main program should be iterative, but you are free to use properly recursive
; auxiliary functions if it seems useful to do so.)

; Be sure to include (WORKING) tests of your function.  

;;;; INSERT YOUR ANSWER HERE

; 1st row: (1)
; 2nd row: (1 1)
; 3rd row: (1 2 1)
; 4th row: (1 3 3 1)
; 5th row: (1 4 6 4 1)
; The nth row: ((n-1)-choose-0 (n-1)-choose-1 ... (n-1)-choose-(n-2) (n-1)-choose-(n-1))

; We could write a function that computes the kth term in the nth row for 0 <= k <= n.

(define (pascal n k)
  (cond ((= k 1) 1) ; the first element of each row is always 1 (basis step)
        ((= n k) 1) ; the last (nth) element of each row is always 1 (basis step)
        (else (+ (pascal (- n 1) k)
                 (pascal (- n 1) (- k 1)))))) ; pascal(n, k) = pascal(n-1, k) + pascal(n-k, k-1)

; Induction on the row number. If all the rows up to the ith row are correctly computed, then the (i+1)st row
; will be correctly computed since its value would be computed solely from values from the ith row.
; The program will terminate because n is always replaced by n-1 and/or k is replaced by k-1. As a result,
; it will eventually be the case that either k = 1 or, alternately, n = k (since k < n and n gets decremented
; while k stays the same in half the calls).

; Then, for a given n, we just need the mapping from (0 1 ... n) to (pascal(n, 0) pascal(n, 1) ... pascal(n, n)).

(define (enumerate-interval low high) ; gives us (low low+1 ... high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (pascal-1 n) ; change to a one-argument pascal
  (lambda (k)
    (pascal n k)))

(define (nth-row-of-pascal n)
  (map (pascal-1 n) (enumerate-interval 1 n)))

; However, the question asks for an iterative main function, so we can't use map (or accumulate), which is
; recursive.

(define (nth-row-of-pascal-iter n)
  (define (aux result i)
    (cond ((= i 1) result)
          (else (aux (cons (pascal n (- i 1)) result) (- i 1)))))
  (aux '(1) n))

; The invariant is that result = (pascal(n,i) pascal(n,i+1) ... pascal(n,n)).
; The program terminates when i = 1, i.e., when result = (pascal(n,0) ... pascal(n,n)), which is the end goal.
; On the initial call the invariant holds because result = (1) = (pascal(n,n)).
; If on the kth call result satisfies the invariant, on the (k+1)st call we cons pascal(n,i-1) to the front
; so that for the new i = i-1 we now have pascal(n,i) in front of (pascal(n,i+1) ... pascal(n,n)), giving us
; (pascal(n,i) ... pascal(n,n)).

; That is, of course, very inefficient: we called recursive pascal, which is incredibly inefficient in itself, n times.

; A better implementation would be one that first computes the first and second rows, then the third, then the fourth,
; and so on (iteratively)...
; Instead of recomputing all the terms multiple times, we compute all the necessary terms just once.

(define (add-lists l1 l2) ; precondition: l1 and l2 are equally-sized lists of numbers
  (cond ((null? l1) '())
        (else (cons (+ (car l1) (car l2)) (add-lists (cdr l1)
                                                     (cdr l2))))))

; If l1 is null (and thus, by the precondition, l2 is null) we correctly return null.

; If add-lists returns the correct result of adding (cdr l1) and (cdr l2) (say they each have k elements), we simply need
; to cons the sum of (car l1) and (car l2) to the front to get the correct result for the whole list (with k+1 elements).

(define (nth-row-better n)
  (define (aux result i)
    (cond ((= n i) result)
          (else (aux (add-lists (cons 0 result)
                                (append result '(0)))
                     (+ i 1)))))
  (aux '(1) 1))

; The invariant is that result is the list containing the elements of the ith row of Pascal's triangle.

; On the initial call it holds: the 1st row is indeed '(1).

; If on the kth call result is the kth row, then on the (k+1)st call we want to be sure that it will have become the (k+1)st
; row.
    
; To get the next row we add together the current row with a 0 in front and the current row with a 0 at the end.
; For example, adding (0 1 3 3 1)
;                 and (1 3 3 1 0) gives us (1 4 6 4 1).
; In general this works because the first and last elements are always 1, and as for the middle elements, the new kth element is always
; the old kth element plus the old (k-1)st element (i.e., pascal(n,k) = pascal(n-1,k) + pascal(n-1,k-1)).

; i is replaced by i+1 on each next call so that it will eventually be equal to n, at which point result is returned, which will be
; the nth row according to the invariant.

; The invariant continues to hold on each next call because in moving from i to i+1 we also moved from the current (ith) row to the
; next row.

(nth-row-better 1) ; returns (1)
(nth-row-better 2) ; returns (1 1)
(nth-row-better 3) ; returns (1 2 1)
(nth-row-better 4) ; returns (1 3 3 1)
(nth-row-better 5) ; returns (1 4 6 4 1)

(nth-row-better 10) ; returns (1 9 36 84 126 126 84 36 9 1)

; Problem 2a (10 points)

; We defined accumulate

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))


; Use this function to implement the function mymap so that a call (mymap p seq), where p is a procedure and seq is a
; list, returns the same value as (map p seq), where map is as defined in Scheme.

; No proof is necessary, but I do expect you to include (WORKING) test cases. 


;;;; INSERT YOUR ANSWER HERE

(define (mymap proc seq)
  (accumulate (lambda (x y) (cons (proc x) y))
              '()
              seq))

; (accumulate op init seq) for seq = (a b c) would give us (op a (op b (op c init))). It is done recursively, so the
; elements of seq are accumulated into init in backward order via the operation op. Notice that the op takes two
; arguments: an element of seq and init, which holds the cumulative result of the previous application sof op.
; Hence we want such an op of two arguments that (1) applies proc to the seq element x, and (2) conses the result onto the
; other argument y.
; That op is precisely (lambda (x y) (cons (proc x) y)).
; For seq = (1 2 3) we would essentially get (cons (proc 1) (cons (proc 2) (cons (proc 3) '()))).
; First apply proc to 3 and then cons the result to '(), our resulting map. Then apply proc to 2 and then cons the result to
; to that map. Then apply proc to 1 and then cons the result to that map.

(display "\nquestion 2 testing\n")
(define (square x) (* x x))
(mymap square '(0 1 2 3 4)) ; (0 1 4 9 16)
(mymap car '((a 1) (b 2) (c 3) (d 0))) ; (a b c d)

; Problem 2b (10 points)

; Implement a function count-leaves using accumulate and map, so that for any list t,
; (count-leaves t) returns the number of leaves of t (thinking of t as a tree).

; No proof is necessary, but I do expect you to include (WORKING) test cases.



;;;; INSERT YOUR ANSWER HERE

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

; This is a natural implementation of count-leaves, but it doesn't use accumulate and map.

; The idea is to use map to transform a list lst of elements into another list lst' of elements that are the same
; elements (in the same order) as those of lst but transformed individually by a given proc. We could use count-leaves
; as proc: the map would give a count of the leaves for each element of the list (for each subtree of the tree).
; Then use accumulate to add the elements together.

; For example, (map count-leaves '(1 2 3 4)) would give us the map (1 1 1 1). Accumulating this map would give us 1+1+1+1=4.
; (map count-leaves '((1 1) 2 3 (4 4)) would give us the map (2 1 1 2). Accumulating this map would give us 2+1+1+2=6.
; The latter tree looks like this:
;        .
;       /|\ \
;      . 2 3 .
;     / \   / \
;    1  1  4  4
; The leaves are always atoms. So counting leaves in a tree is the same as counting the number of atoms in the
; (logically equivalent) s-exp.
; Each branch stemming from the root node represents a distinct element. Each branch is, indeed, a tree.
; The recursive calls to count-leaves (carried out by the map function) work in this context because they operate on subtrees,
; which are always simpler than the original tree. The trees as arguments eventually dwindle down to singleton trees (leaves),
; in which case 1 is returned and recursing stops.

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (accumulate + 0 (map count-leaves t)))))


(count-leaves '(1 2 3 4)) ; 4
(count-leaves '((1 1) 2 3 (4 4))) ; 6
(count-leaves '()) ; 0
(count-leaves 5) ; 1
(count-leaves '(5)) ; 1
(count-leaves '((1 2) (3 (4)) 5 (6 ((7 8 ((9) 10)))))) ; 10

; Problem 3a (40 points)

; As you recall, any proposition constructed with the connectives ~ (not), ^ (and), v (or) and => (implies) can be transformed
; to a logically equivalent proposition constructed using only ~ and v.  The main equivalences used are De Morgan's law, namely
; (a ^ b) = ~ (~a v ~b), and p => q = ~p v q.

; In this problem you are asked to define a datatype (with suitable representations) of propositions, and to use the resulting
; collection of constructors, selectors and classifiers to write and prove a procedure normalize which inputs a proposition using
; ~, ^, v, and => and which returns a logically equivalent proposition which uses only v and ~.

; For example, if e is the representation of ~ (a ^ b), (normalize e) will return the representation of ~a v ~b.

; As an indication of what I have in mind for the proposition datatype, e would be built as (make-not (make-and 'a 'b)).

; The procedure normalize should progress by recursive descent; your proof should be inductive -- be sure to say what you
; are inducting on.

; Be sure to include (WORKING) test cases.  



;;;; INSERT YOUR ANSWER HERE

; Start with a definition:
;   prop ::= variable | (~ prop) | (prop ^ prop) | (prop v prop) | (prop => prop)

; Variables are letters/atoms that presumably evaluate to either #t or #f.

; Constructors:

(define (make-not p)
  (list '~ p))
(define (make-and p1 p2)
  (list p1 '^ p2))
(define (make-or p1 p2)
  (list p1 'v p2))
(define (make-implies p1 p2)
  (list p1 '=> p2))

; Selectors:

(define (operator p)
  (cond ((eq? '~ (car p)) '~) ; if the first elt is ~ then the operator is that
        (else (car (cdr p))))) ; otherwise the operator is the second elt

(define (first-operand p)
  (cond ((eq? (car p) '~) (car (cdr p))) ; if the first elt is ~ then the second elt is the first (and only) operand
        (else (car p)))) ; otherwise the first operand has to be the first elt

(define (second-operand p)
  ; precondition: p is not a not-expression.
  (caddr p)) ; the second operand has to be the third elt

(display "\nquestion 3 testing\n")

(define p1 (make-implies 'a 'b))
(define p2 (make-and 'c 'd))
(define p3 (make-or p1 p2))
(define g (make-not p3))

p1 ; (a => b)
(operator p1) ; =>
(first-operand p1) ; a
(second-operand p1) ; b

p3 ; ((a => b) v (c ^ d))
(operator p3) ; v
(first-operand p3) ; (a => b)
(second-operand p3) ; (c ^ d)

g ; (~ ((a => b) v (c ^ d)))
(operator g) ; ~
(first-operand g) ; ((a => b) v (c ^ d))

; Classifiers:

(define (not-exp? e)
  (cond ((not (pair? e)) #f)
        (else (eq? (operator e) '~))))

(define (or-exp? e)
  (cond ((not (pair? e)) #f)
        (else (eq? (operator e) 'v))))

(define (and-exp? e)
  (cond ((not (pair? e)) #f)
        (else (eq? (operator e) '^))))

(define (implies-exp? e)
  (cond ((not (pair? e)) #f)
        (else (eq? (operator e) '=>))))


; (a ^ b) should be normalized to (~ ((~ a) v (~ b)))
(define (normalize-and e)
  (make-not
   (make-or (make-not (first-operand e))
            (make-not (second-operand e)))))

; a => b should be normalized to ((~ a) v b)
(define (normalize-implies e)
  (make-or (make-not (first-operand e))
           (second-operand e)))

(define (normalize e)
  (cond ((not (pair? e)) e)
        ((not-exp? e) (make-not (normalize (first-operand e))))
        ((or-exp? e) (make-or (normalize (first-operand e))
                              (normalize (second-operand e))))
        ((and-exp? e) (normalize (normalize-and e)))
        ((implies-exp? e) (normalize (normalize-implies e)))))
  
; normalize works by eliminating ^ and => one by one from the given expression e.
; (normalize-implies e) removes one => from e; whereas (normalize-and e) removes one ^ from e.

; In the base case, e is not a list, so it is an atom: just return e, as it has neither of the problematic connectives.

; (i) If the expression's outer operator/connective is not problematic (if it is either a not-exp or an or-exp) then we
; merely have to normalize the subexpression(s) (and make a not-exp or or-exp out of the normalized subexpression(s)).
; In this case the expressions passed to the recursive calls each have at least one fewer connective in it than the main
; expression.
; (We go from operating on (exp1 connective exp2) to operating on either exp1 or exp2.)

; (ii) If the expression's outer operator/connective is problematic (if it is either an and-exp or an implies-exp) then we
; ought to transform it to a normalized expression before then normalizing the subexpressions, as in (i).
; The argument passed to the single recursive call in this case does not have fewer connectives, but it does have one
; fewer problematic connectives.
; We go from operating on (a => b) to operating on ((~ a) v b), or from (a ^ b) to (~ ((~ a) v (~ b))).

; The recursive calls in case (i) devolve into the base case; they eventually stop because every call reduces the complexity
; (number of connectives) of the argument until we end up dealing with an atom. Case (ii) immediately devolves into case (i).

; If we assume in the inductive hypothesis that the function works on expressions with up to k connectives, we know
; the function will work on expressions with k+1 connectives by the discussion above. The connective that is being processed
; in each call is removed if and only if it is problematic. If it is problematic we just replace the expression with
; a logical one without the connective and start over. If it is not then we keep the connective but normalize each subexpression.
; The normalizing always returns a logically equivalent proposition, so by the induction step is just that (exp1 con exp2)
; is equivalent to ((normalize exp1) con (normalize exp2)).






; Problem 3b (10 points - Extra Credit)

; Write a function remove-double-not which works with your representation of propositions to remove double negations.  For
; example, if ~ ~ ((~ x) v (~ y)) is represented as (~ (~ ((~ x) v (~ y)))), then
; (remove-double-not '(~ (~ ((~ x) v (~ y))))) returns ((~ x) v (~ y)).

; Note that double negations may occur at arbitrary depth, so that a recursive routine is necessary.  Test your procedure
; by composing it with normalize -- what output do you get for your representation of ~ (a ^ b)?  

; You do not need to give a proof.  You do need to show that your code works on some well-chosen test cases. 


(define (remove-double-not e)
  (cond ((not (pair? e)) e)
        ((not-exp? e) (cond ((not-exp? (first-operand e)) (remove-double-not (first-operand (first-operand e))))
                            (else (make-not (remove-double-not (first-operand e))))))
        ((or-exp? e) (make-or (remove-double-not (first-operand e))
                              (remove-double-not (second-operand e))))
        ((and-exp? e) (make-and (remove-double-not (first-operand e))
                                (remove-double-not (second-operand e))))
        ((implies-exp? e) (make-implies (remove-double-not (first-operand e))
                                        (remove-double-not (second-operand e))))))

(remove-double-not '(~ (~ ((~ x) v (~ y))))) ; ((~ x) v (~ y))
(remove-double-not '(~ (~ ((~ (~ (~ x))) v (~ (~ y)))))) ; ((~ x) v y)
(remove-double-not '(~ x)) ; (~ x)
(remove-double-not '(~ (~ x))) ; x
(remove-double-not '(~ (~ (~ x)))) ; (~ x)
(remove-double-not '(~ (~ (~ (~ x))))) ; x


; Problem 4a.  (10 points) Write and prove correct a pair of MUTUALLY RECURSIVE programs odd-indexed-elements and
; even-indexed-elements to produce from one list L the two lists consisting, respectively, of the elements of L with
; even index and the elements of L with odd index.  For example, (odd-indexed-elements '(a b c d e f g h)) = '(b d f h)
; and (even-indexed-elements '(a b c d e f g h)) = '(a c e g).  (Hint: use one proof for both functions).  

; The odd indexed elements of (0 1 2 3 4 5 ...) are (1 3 5 ...).
; If we want to define odd-indexed-elements in terms of even-indexed-elements, then note that the even-indexed-elements
; of (cdr (0 1 2 3 4 5 ...)) = (1 2 3 4 5 ...) are (1 3 5 ...).

(define (odd-indexed-elements l)
  (if (null? l)
      '()
      (even-indexed-elements (cdr l))))

(define (even-indexed-elements l)
  (if (null? l)
      '()
      (cons (car l) (odd-indexed-elements (cdr l)))))

; Each recursive call changes l to (cdr l) so that the parameter l eventually becomes null after losing one element at a time.
; In the base case, '() is correctly returned for an empty list: there are no oddly or evenly indexed elements in an empty list.

; Suppose the correct elements are returned for either function for a list with k-1 elements. Then we know that the correct
; elements are returned by either function for a list with k elements: simply return the elements with odd (if you wanted even)
; or even (if you want odd) indexes of the cdr of the list, which has k-1 elements.

; In the case of even indexes, (cdr lst) loses the 0th element, but we
; cons it back to the odd-indexed-elements of (cdr lst) (which would be the 2nd, 4th, 6th, ..., elements), giving us
; the 0th, 2nd, 4th, 6th, ..., elements.

(display "\nquestion 4 testing\n")

(odd-indexed-elements '(0 1 2 3 4 5)) ; (1 3 5)
(even-indexed-elements '(0 1 2 3 4 5)) ; (0 2 4)

; 4b.  (15 points) Write and prove correct a procedure mergesort which inputs a single (unsorted) list of numbers
; and which outputs the list consisting of the elements of the input list, in sorted order. 
; Your procedure should make use of your solution to 4a.

; (HINT: start by writing a procedure merge which takes two lists of numbers, with each list sorted in order from
; smallest to largest, and which produces a third list containing all the (distinct) elements of the original lists,
; in sorted order.)


; Again, for both problems, I expect you to give (working) test cases.  

; The idea of mergesort is to divide the list into 2, recursively mergesort them,
; and then merge the two resulting lists. When a list has 0 or 1 element, we've reached the base case: just return the list itself.
; The task at hand is divided in half (in terms of the input size) each time and ends when we have 1 element;
; so mergesort has a logarithmic time complexity.

; An easy way to break a list into two roughly equal halves would be to use odd-indexed-elements and even-indexed-elements.

(define (merge lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((< (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) lst2)))
        ((= (car lst1) (car lst2)) (cons (car lst1) (merge (cdr lst1) (cdr lst2))))
        (else (cons (car lst2) (merge lst1 (cdr lst2))))))

; Precondition: lst1 and lst2 are sorted lists of numbers
; Postcondition: a single list containing the elements of lst1 and lst2 in sorted order is returned.

; Basis step: one of the lists is null, so the merged list is just the other list.
; Inductive hypothesis: Suppose merge correctly merges lists each of which has a maximum of k elements
; (the number of elements in the two lists may vary, but let's say k is the maximum of their lengths).
; Inductive step: The inductive step is that merge is therefore able to deal with lists with up to k+1 elements.
; How? By first computing the merge result for the cdr of
; one of the lists, which would be correct by the induction hypothesis, and consing the car onto the result
; (once we've determined that the particular car is the smaller of the two lists' cars and thus the smallest of all
; the elements in both lists). 

; The recursive call is either (merge (cdr lst1) lst2) or (merge lst1 (cdr lst2)). One of the lists eventually becomes empty
; and the program reaches the base case and terminates.

(define (mergesort lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst) ; lst has just 1 element
        (else (merge (mergesort (odd-indexed-elements lst))
                     (mergesort (even-indexed-elements lst))))))

; Precondition: lst is a list of numbers.
; Postcondition: the sorted list with the same elements as lst is returned.

; lst always becomes smaller on each recursive call and dwindles down to one or zero elements; in the base case, lst correctly returned.
; Merge requires that the two list arguments
; be sorted, but they're clearly going to be if mergesort works correctly.

; Suppose mergesort works correctly on lists with up to k
; elements (k > 1). Then it will work correctly on lists with up to k+1 elements because we just divide any list with k+1 elements into
; sublists with less than k+1 elements (i.e., up to k elements) and call mergesort on those, which will be sorted and then merged together.

(mergesort '()) ; ()
(mergesort '(3)) ; (3)
(mergesort '(4 3 2 9 0 5 1)) ; (0 1 2 3 4 5 9)
(mergesort '(1 3 2 3 4 9 15)) ; (1 2 3 4 9 15)
(mergesort '(0 3 1 1 1 2 2 1 1 2 1 2 1 1 1)) ; (0 1 2 3)