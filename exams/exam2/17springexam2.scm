; CSc 335
; Spring 2017

; May 2

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TYPE YOUR NAME HERE:

; TYPE YOUR FULL EMAIL ADDRESS HERE:
; (I will email your graded paper to this address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1 - code (max 7 points)
;;;; Problem 1 - proof (max 7 points)
;;;; Problem 1 - synergy between proof and code (max 6 points)

;;;; Problem 2 - code (max 7 points)
;;;; Problem 2 - proof (max 7 points)
;;;; Problem 2 - synergy between proof and code (max 6 points)

;;;; Problem 3a - code (max 10 points)
;;;; Problem 3b - code (max 10 points)

;;;; Problem 4 - code (max 15 points)
;;;; Problem 4 - proof (max 15 points)
;;;; Problem 4 - synergy between proof and code (max 10 points)


;;;; Total
;;;; Letter Grade

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the implementation of R5RS
; provided by drracket.  ALL FUNCTIONS ARE TO BE GIVEN IN PURE FUNCTIONAL SCHEME.  YOU SHOULD NOT USE STRINGS OR
; VECTORS -- lists will suffice.  

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.
 
; BE SURE TO SAVE YOUR WORK FREQUENTLY.

; DO NOT ERASE EITHER THE QUESTIONS OR THE SCORING RUBRIC, ABOVE.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




; Here are the examination problems.



; Problem 1  (20 points)

; Background: Let us say that one list l1 of numbers is smaller than another list l2 of numbers
; if the largest element of l1 is less than the largest element of l2.  Thus '(1 12 15) is smaller than '(17)

; The Problem: Design and prove a predicate list-less-than? which implements this relation.  You should use an iterative
; auxilliary procedure list-max which inputs a list of numbers and which returns the largest element of that
; list.

; Give a proof of your iterative procedure list-max, and make use of this to show that your predicate
; list-less-than? is correct. 

; Here's a recursive version:

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (list-max-recur lst)
  (accumulate max 0 lst))

; Precondition: lst is a list of numbers.
; Postcondition: if lst is empty, 0 is returned; otherwise, the largest number in lst is returned.

; Iterative version:

(define (list-max-iter lst)
  (define (aux lst2 max-so-far)
       (cond ((null? lst2) max-so-far)
             (else (aux (cdr lst2) (if (> (car lst2) max-so-far)
                                       (car lst2)
                                       max-so-far)))))
  (aux (cdr lst) (car lst)))

; Precondition: lst is a nonempty list of numbers.
; Postcondition: the largest number in lst is returned.

; The invariant is that max-so-far is the largest number in the list of numbers lst\lst2 (the complement of lst
; relative to lst2). 

; In the initial call, the invariant says that (car lst) is the largest number in lst\(cdr lst) = ((car lst)),
; so it trivially holds.
; Then suppose that the invariant holds on the kth call. Does it hold on the (k+1)st call?
; On the kth call, max-so-far is the highest number in lst\lst2.
; On the (k+1)st call, lst2 has been set to (cdr lst2). The complement lst\(cdr lst2) has one new element:
; (car lst2). The highest number in the old complement (the old max-so-far) has to be compared to (car lst2),
; and (car lst2) replaces that highest element iff it is higher than it. So again the invariant will hold.

; The invariant holds on every call, so when the program terminates, the invariant will say that max-so-far is the
; highest element in lst\nil = lst, which is what we want and what is returned. The program must terminate since
; lst2 is replaced by (cdr lst2) on every recursive call, so the argument eventually becomes the empty list.

(define (list-less-than? l1 l2)
  (< (list-max-iter l1) (list-max-iter l2)))

(display "problem 1 testing:\n")
(list-less-than? '(1 12 15) '(17)) ; #t
(list-less-than? '(1 12 17) '(15)) ; #f
(list-less-than? '(1 2 3) '(1 2 3)) ; #f
(list-less-than? '(15 16) '(18)) ; #t

; By definition, l1 < l2 if and only if the maximum element in l1 < the maximum element in l2, which is what
; the function tells us.


; Problem 2 (20 points)

; Background: Let p? be an order predicate, and suppose seq is a list of elements which is
; ordered with respect to p?.  That is, if i and j are legal indices for seq, then i < j implies that

;     (p? (list-ref seq i) (list-ref seq j))

; is true.  For example, if p? is the usual less-than relation on integers, then the list (1 2 3 5 6)
; is ordered with respect to p?.

; The Problem: Design and certify a recursive procedure insert which inputs an element e,
; a sequence seq ordered by p? as we have just described, and the predicate p?, and which returns
; the list consisting of e and the elements of seq, all in p? order.  That is, the procedure should
; insert the element e into its p?-appropriate position in seq.  For example, if e were 4, then for p? = <
; and the list shown above, the returned list is just (1 2 3 4 5 6):  that is,
;
;                          (insert 4 '(1 2 3 5 6) <) = '(1 2 3 4 5 6)

(define (insert e lst p?)
  (cond ((null? lst) (list e))
        ((p? e (car lst)) (cons e lst))
        (else (cons (car lst) (insert e (cdr lst) p?)))))

; Precondition: p? is an order predicate, seq is a list of elements ordered with respect to p?
; (such that i < j <=> (p? ith-element j-th element)), e is an element that can be inserted such that
; ordering is maintained.
; Postcondition: the same list with e inserted that is still ordered is returned.

; In the base case, lst is null, so inserting e results in just (list e).

; Suppose e can be inserted into (car lst) or (cdr lst). Then we know that it may be inserted into lst by the
; following logic:
; (a) If (p? e (car lst)) is true, then e should come before the first element of lst: so we return
; (cons e lst).
; (b) Otherwise -- if (p? (car lst) e) is instead true -- then e should come after the first element of lst.
; So we have to insert it into the cdr. The car is then consed back onto the result of inserting into the cdr.

; Since (car lst) and (cdr lst) are conceptually simpler trees (or structural components) of lst, the recursing will
; eventually stop when the lst parameter becomes null.


; Problem 3a (10 points)

; Using the insert procedure developed in 2a, write a sorting procedure insertion-sort which inputs
; a list l of elements and an order predicate appropriate for these elements, and which returns a list
; l' with the same elements as l, but in sorted order.  Thus if l = '(d b a c) and the order predicate
; specifies a < b < c < d, the returned list is '(a b c d).  (A proof is not requested.)

(define (insertion-sort l pred)
  (cond ((null? l) l)
        (else (insert (car l) (insertion-sort (cdr l) pred) pred))))

; To insertion-sort a list l, we insertion-sort (cdr l) and insert (car l) into the result.

(insertion-sort '(12 14 20 0 9 8 3 2 91 84 1 7) <) ; (0 1 2 3 7 8 9 12 14 20 84 91)
(insertion-sort '(12 14 20 0 9 8 3 2 91 84 1 7) >) ; (91 84 20 14 12 9 8 7 3 2 1 0)

; Problem 3b (10 points)

; Finally, using the functions you have developed in the previous problems, write a procedure that inputs
; a list of lists of numbers and which returns the list with the largest number in it.  Calling this
; function list-with-largest-number,  (list-with-largest-number '((16 43 7) (25 98) (57 2 89 14))) should
; return '(25 98).  (A proof is not requested)

; We have:
; (i) a procedure list-less-than? that returns whether l1 < l2 (the max element of l1 < the max element of l2)
; (ii) a insertion-sort procedure that sorts a list according to some order predicate

; The simplest way to find the sublist with the largest number using these functions is just to insertion-sort
; them via the predicate list-less-than?. The last element then becomes the "largest" list (the list whose
; largest element is the largest of all the lists' largest elements).

(define (last-element lst) ; lst is a non-empty list
  (cond ((null? (cdr lst)) (car lst)) 
        (else (last-element (cdr lst)))))

(define (list-with-largest-number lst-of-lsts)
  (last-element (insertion-sort lst-of-lsts list-less-than?)))
  
(list-with-largest-number '((16 43 7) (25 98) (57 2 89 14))) ; (25 98)

; Problem 4 (40 points)

; In this problem, you are asked to implement and prove correct a procedure bfs for breadth-first
; search of a binary tree.  For emphasis;  BREADTH FIRST, NOT DEPTH FIRST!

; Binary trees have nodes and branches, and leaves.  Leaves can be represented as atoms; binary trees
; which are not just leaves can be represented as lists of the form

;             (node left-subtree right-subtree)

; For example, (13 (5 6 1) (45 7 18)) is a binary tree with root node 13, and left and right subtrees.
; (5 6 1) is a binary tree with root node 5, left subtree 6 (which is a leaf), and right subtree 1 (also a leaf).

; Given a binary tree t, and an element e, your bfs program is to return #t if e occurs as a node or leaf element of t,
; and #f otherwise.  Thus, referring to the example just given as sample-tree, (bfs sample-tree 0) is #f, while
; (bfs sample-tree 13), (bfs sample-tree 5) and (bfs sample-tree 18) are all #t. 

; You may assume that nodes and leaves are atoms.

; You will want to write selectors (node, leaf?, subtrees) appropriate for this data structure and then to make use of these in your
; breadth-first search program. A constructor, say make-tree, could also be put to good use in setting up test data
; for your function.   


; Recall from your algorithms course that breadth-first search proceeds by maintaining a list of 
; the subtrees that must be returned to:

;   When a leaf is reached, it is examined - if it is the searched for element, return #t; otherwise, search the list of remaining subtrees.

;   When a node is reached, it is examined - if it is the searched for element, return #t; otherwise, add its subtrees to the end of the
;   search list, and then continue the search.

;   If the search list is empty, the search has failed.

; You should define a function aux, with one parameter, search-list, so that bfs can be defined in terms of aux, as follows:


; (define (bfs t e)
;   (define (aux search-list) ... )
; (aux (list t))
 

; Here, t is the binary tree, which is searched for the element e -- where e, if it occurs at all in t, occurs as either a node or a leaf. 

(define (root bt) ; precondition: bt is not just a leaf/atom
  (car bt))

(define (left-subtree bt) ; precondition: bt is not just a leaf/atom
  (cadr bt))

(define (right-subtree bt) ; precondition: bt is not just a leaf/atom
  (caddr bt))

(define (leaf? bt) 
  (and (not (null? bt))
       (not (pair? bt))))

; binary tree ::= atom/leaf | (node left-subtree right-subtree)

(define (make-tree node left-subtree right-subtree)
  (list node left-subtree right-subtree))

; The constructor make-tree is relevant if we're making a full-blown tree, as atoms don't need to be constructed.

(define (bfs t e)
  (define (aux search-list)
    (if (null? search-list)
        #f
        (let ((next-tree (car search-list)))
          (cond ((leaf? next-tree) (if (= next-tree e)
                                       #t
                                       (aux (cdr search-list))))
                ((= (root next-tree) e) #t)
                (else (aux (append (cdr search-list)
                                   (list (left-subtree next-tree))
                                   (list (right-subtree next-tree)))))))))
  (aux (list t)))

; For the tree (13 (5 6 1) (45 7 18)) we would check the elements in the order 13, 5, 45, 6, 7, 1, 18.
;     13
;    /  \
;   5   45
;  / \  / \
; 6  1 7  18


; We maintain a list of lists (i.e., a tree of trees), "search-list", whose lists are to be searched from left to right.
; Initially this list of lists is just (list t), the list that just contains our entire tree.
; In a given call, we first search the root of the foremost tree in search-list.
; Then we call the function again with the foremost tree removed from search-list and the removed tree's left and right subtrees
; appended to the *end* of the list. Therefore, on the second call, search-list would be (left-subtree right-subtree).
; The process continues: search the root of the foremost tree, left-subtree, and then extract left-subtree and place left-subtree's
; subtrees at the end: search-list becomes (right-subtree left-subtree-of-left-subtree right-subtree-of-left-subtree).
; Then on the next call we search the node of right-subtree, and search-list becomes...

; This replacement process stops when the foremost tree is simply a leaf (or when the element has been found). The leaf is still extracted,
; but its subtrees (of which there are none) aren't inserted at the end.

(bfs '(13 (5 6 1) (45 7 18)) 0) ; #f
(bfs '(13 (5 6 1) (45 7 18)) 1) ; #t

; If on the first two lines of the function definition for aux we added

;   (display search-list)
;   (display "\n")

; We would see the life cycle of search-list:

; Searching for 0 gives us

; ((13 (5 6 1) (45 7 18)))
; ((5 6 1) (45 7 18))
; ((45 7 18) 6 1)
; (6 1 7 18)
; (1 7 18)
; (7 18)
; (18)
; ()
; #f

; But searching for 1 gives us

; ((13 (5 6 1) (45 7 18)))
; ((5 6 1) (45 7 18))
; ((45 7 18) 6 1)
; (6 1 7 18)
; (1 7 18)
; #t

; Note that with this implementation we don't allow binary trees to have a left-branch and not a right-branch or a right-branch and
; not a left-branch.


(define (bfs t e)
  (define (aux search-list)
    (if (null? search-list)
        #f
        (let ((next-tree (car search-list)))
          (cond ((leaf? next-tree) (if (= next-tree e)
                                       #t
                                       (aux (cdr search-list))))
                ((= (root next-tree) e) #t)
                (else (aux (append (cdr search-list)
                                   (list (left-subtree next-tree))
                                   (list (right-subtree next-tree)))))))))
  (aux (list t)))

; Some of the design roles:
; The leftmost tree in search-list is the next tree we have to search.
; 
; Proof:
; The invariant is that search-list contains the trees we have to search in order. In particular, the root of the foremost tree in this list
; is the next element to check.
; On the initial call, we simply want to check t (that is, we will first want to check t's root node). Thus (list t) is the correct search-list
; to pass to the function originally.
; If on the kth call the invariant holds, we move on to the (k+1)st call iff the next-tree's root node, or next-tree itself if it's an atom,
; is not equal to e. In that case, on the (k+1)st call the invariant must hold. So we extract the next-tree and place next-tree's subtrees
; at the end (this process mirrors that of a queue), if they exist. Clearly, the new foremost tree is going to be the next tree to check,
; given that the invariant held on the previous call. The subtrees of the "just-processed" tree would then be checked last, which is how
; a BFS (as opposed to a DFS) works.
; When search-list is empty, there are no more trees to check (<=> we've checked them all) according the invariant, so we correctly return #f.