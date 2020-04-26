
; CSc 335
; Spring 2016

; April 12

; Second 1.25 Hour Exam

; Professor Troeger


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Scoring Template - Do NOT Erase!

;;;; Problem 1a - code (max 15 points)   
;;;; Problem 1b - code (max 15 points)   

;;;; Problem 2a - code (max 15 points)   
;;;; Problem 2b - proof (max 15 points)  

;;;; Problem 3 - code (max 40 points)    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; You have full access to drracket for this exam. All problems are to be solved using the R5RS
; implementation provided by drracket and only those language features discussed so far in the
; context of lectures and homework.

; Collaboration of any kind is not permitted: you are to work alone; email and internet access have been disabled.

; Smart phones are to be switched off and placed on the desk in front of you.  They are not to leave the room.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; YOU SHOULD INSERT YOUR ANSWERS INTO THE EXAM DIRECTLY FOLLOWING EACH QUESTION.

; BE SURE TO SAVE YOUR WORK FREQUENTLY.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Here are the examination problems.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1a.  (15 points) Write a pure functional scheme procedure replace-every-nth which takes as input

;        a tree tr (that is, tr is a list, but not necessarily a list of atoms)
;        a positive integer n
;        an atom, old
;        an atom, new

; (replace-every-nth tr n old new) should replace every nth occurrence of old in 
; lst by new (and leave everything else unchanged).  If n = 3, for example, then every third occurrence
; of old will be replaced by new.



; Problem 1b.  (15 points) Argue by induction that the function you give for Problem 1a is correct.




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 2a.  (15 points) Write a procedure pairs that takes as argument a positive integer n and
; which returns a list of all pairs (i j) with 1 <= i <= n, 1 <= j <= n and i <> j (that is, i
; not equal to j).  Thus, for n = 3, the output should include precisely the pairs (1 2) (1 3)
; (2 1) (2 3) (3 1) (3 2).

(define (enumerate-interval low high)
  (cond ((> low high) '())
        (else (cons low
                    (enumerate-interval (+ 1 low) high)))))

(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq)
                  (accumulate op init (cdr seq))))))

(define (flatmap proc seq) ; takes a list of lists ((x) (y) ...) and returns (x y ...)
  (accumulate append '() (map proc seq)))

(define (pairs n)
  (flatmap (lambda (i)
         (map (lambda (j)
                (list i j))
              (append (enumerate-interval 1 (- i 1))
                      (enumerate-interval (+ i 1) n))))
       (enumerate-interval 1 n)))

(pairs 1) ; ()
(pairs 2) ; ((1 2) (2 1))
(pairs 3) ; ((1 2) (1 3) (2 1) (2 3) (3 1) (3 2))
(pairs 4) ; ((1 2) (1 3) (1 4) (2 1) (2 3) (2 4) (3 1) (3 2) (3 4) (4 1) (4 2) (4 3))

; Problem 2b. (15 points) Prove that the function you give for Problem 2a is correct. 

; For each i in (1 2 ... n) we ought to have in our resulting list all the lists (i j) where j is not equal to
; i but is also in (1 2 ... n).

; The outer map goes through each i in (1 2 ... n) and transforms it to the map from j in
; (1 ... i-1) U (i+1 ... n) to (list i j). That is, for each i, we got (i j) for each j in that "second" interval.

; Each i is transformed to a list, so the result if we did not use flatmap would be a list of i lists. We use flatmap
; to remove the outer parentheses around each of these sublists, giving us a flat list. When we append one list
; to another, it just becomes one list. The last list eats up all the other lists.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Problem 3 (40 points)

; In this problem, you are asked to implement and prove correct a procedure bfs for breadth-first
; search of a binary tree.

; Binary trees have nodes and branches, and leaves.  Leaves can be represented as atoms; binary trees
; which are not just leaves can be represented as lists of the form

;             (node left-subtree right-subtree)

; For example, (13 (5 6 1) (45 7 18)) is a binary tree with root node 13, and left and right subtrees.
; (5 6 1) is a binary tree with root node 5, left subtree 6 (which is a leaf), and right subtree 1 (also a leaf).

; Given a binary tree t, and an element e, your bfs program is to return #t if e occurs as a node or leaf element of t,
; and #f otherwise.  Thus, referring to the example just given as sample-tree, (bfs sample-tree 0) is #f, while
; (bfs sample-tree 13), (bfs sample-tree 5) and (bfs sample-tree 18) are all #t. 

; You may assume that nodes and leaves are atoms.

; You will want to write selectors appropriate for this data structure and then to make use of these in your
; breadth-first search program. A constructor, say make-tree, could also be put to good use in setting up test data
; for your function.


; Recall from your algorithms course that breadth-first search proceeds by maintaining a list of 
; the subtrees that must be returned to.  When a node is reached, it is examined - if it is the
; element searched for, return #t; otherwise, add its subtrees to the end of this
; search list, and then continue the search.  If the search list is empty, the search has failed.
; I leave it to you to work out what to do when a leaf is reached.

; (No proof is requested, though of course it is always a good idea to provide the grader with some
; comments explaining your intentions.)


; See 17springexam2.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


