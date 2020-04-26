; Exercise 1

;gI: atoms of L checked so far + atoms of L not checked yet = the total number of atoms in L

;proof of correctness
;init: initially lst is L and new-lst is the empty list because no atoms were checked yet. thic can be seen in the code
;      [ (tmp lst '() n old new) ]
;termination: terminates when one of 3 cases are meet:
;             case 1: lst is null, meaning all atoms were checked and the list of checked atoms is returned. This can be
;                     seen in the code [ ((null? lst) (list n new-lst)) ]
;             case 2: n is equal to 1 and the current atom being looked at is equal to old, meaning the current atom is
;                     replaced with the new and appened between new-lst and lst and returned. This can be seen in the code
;                     [ ((and (equal? n 1) (equal? (car lst) old)) (list (- n 1) (append new-lst (list new) (cdr lst)))) ]
;             case 3: n is 0, meaning the n-th instance of old has already been replaced with new and new-lst and lst are
;                     appended and returned. This can be seen in the code [ ((= n 0) (list n (append new-lst lst))) ]
;preservation: assuming all calls up to now are correct, there are 3 cases:
;              case 1: n does not equal 0 and the current atom being looked at equals old, meaning the current atom is the k-th
;                      instance of n, with k being less than n. In this case, in the next iteritive call, the current atom being
;                      looked at is appended to new-lst and n is decremented by 1. This can be seen in the code
;                      [ ((and (not (equal? n 0)) (equal? (car lst) old)) (tmp (cdr lst) (append new-lst (list old)) (- n 1) old new)) ]
;              case 2: the current element of lst being looked at is another list, meaning the next iterivive call will check the
;                      atoms of the current element for the n-th instance of old and update the parameters of the iteritive call
;                      accordingly. the result of the call tmp on the sub-list is used as the new value of n and what will be appended
;                      to new-lst. This can be seen in the code
;                      [ ((list? (car lst)) (tmp (cdr lst) (append new-lst (list (car (cdr (tmp (car lst) '() n old new))))) (car (tmp (car lst) '() n old new)) old new)) ]
;              case 3: the current atom does not equal old and n is not 0 therefore the next iteritive call will simply append
;                      the current atom to new-lst. This can be seen in the code
;                      [ (else (tmp (cdr lst) (append new-lst (list (car lst))) n old new)) ]
;Since we now know that tmp works, we simply return the resultant list which is the cdar of the returned list

;guess idea: check car if list? if yes iterate on it and returen new n and list from that and continue in the super list
(define (replace-nth lst n old new)
  (define (tmp lst new-lst n old new)
    (cond ((null? lst) (list n new-lst))
          ((and (equal? n 1) (equal? (car lst) old)) (list (- n 1) (append new-lst (list new) (cdr lst))))
          ((= n 0) (list n (append new-lst lst)))
          ((and (not (equal? n 0)) (equal? (car lst) old)) (tmp (cdr lst) (append new-lst (list old)) (- n 1) old new))
          ((list? (car lst)) (tmp (cdr lst) (append new-lst (list (car (cdr (tmp (car lst) '() n old new))))) (car (tmp (car lst) '() n old new)) old new))
          (else (tmp (cdr lst) (append new-lst (list (car lst))) n old new))))
  (car (cdr (tmp lst '() n old new))))

(replace-nth (list 1 5 7 3 (list 6 3 (list 4 4 4)) 5 6 3 7 2 7) 3 4 9)
(newline)

;Exercise 2.27
(define atom?
  (lambda (x)
    (and (not (null? x)) 
	 (not (pair? x)))))

(define (my-reverse lst)
  (cond ((null? lst) '())
        (else (append (my-reverse (cdr lst)) (list (car lst))))))

(define (my-deep-reverse lst)
  (cond ((null? lst) '())
        ((not (list? (car lst))) (my-reverse lst))
        (else (append (my-deep-reverse (cdr lst))
                      (list (my-deep-reverse (car lst)))))))

(define (reverse-test)
  (let ((x (list (list 1 2) (list 3 4))))
    (display x)
    (newline)
    (display (my-reverse x))
    (newline)
    (display (my-deep-reverse x))
    (newline)
    (newline)
    ))
(reverse-test)

;Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (selector-left-branch mobile)
  (car mobile))
(define (selector-right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))
(define (selector-length branch)
  (car branch))
(define (selector-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((and (number? (selector-structure (selector-left-branch mobile)))
              (number? (selector-structure (selector-right-branch mobile))))
         (+ (selector-structure (selector-left-branch mobile))
            (selector-structure (selector-right-branch mobile))))
        ((number? (selector-structure (selector-left-branch mobile)))
         (+ (selector-structure (selector-left-branch mobile)) (total-weight (selector-structure (selector-right-branch mobile)))))
        ((number? (selector-structure (selector-right-branch mobile)))
         (+ (selector-structure (selector-right-branch mobile)) (total-weight (selector-structure (selector-left-branch mobile)))))
        (else (+ (total-weight (selector-structure (selector-left-branch mobile)))
                 (total-weight (selector-structure (selector-right-branch mobile)))))))

(define (balanced? mobile)
  (cond ((= (* (selector-length (selector-left-branch mobile))
               (total-weight (selector-structure (selector-left-branch mobile))))
            (* (selector-length (selector-right-branch mobile))
               (total-weight (selector-structure (selector-right-branch mobile)))))
         #t)
        (else #f)))


(total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 3 3)
                                                       (make-branch 3 3)))
                           (make-branch 2 (make-mobile (make-branch 3 3)
                                                       (make-branch 3 3)))))
(total-weight (make-mobile (make-branch 2 (make-mobile (make-branch 3 3)
                                                       (make-branch 3 4)))
                           (make-branch 2 (make-mobile (make-branch 3 3)
                                                       (make-branch 3 3)))))
(balanced? (make-mobile (make-branch 2 (make-mobile (make-branch 3 3)
                                                    (make-branch 3 3)))
                        (make-branch 2 (make-mobile (make-branch 3 3)
                                                    (make-branch 3 3)))))
(balanced? (make-mobile (make-branch 2 (make-mobile (make-branch 3 4)
                                                    (make-branch 3 3)))
                        (make-branch 2 (make-mobile (make-branch 3 3)
                                                    (make-branch 3 3)))))


;Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) (list x))) rest)))))

(subsets '(1 2 3))


;Exercise 2.37
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (curry f x)
  (lambda args (apply f x args)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (curry matrix-*-vector cols) m)))

(dot-product
 (list 1 2 3)
 (list 5 6 1))

(matrix-*-vector
 (list (list 2 6 4)
       (list 1 6 5)
       (list 5 9 2))
 (list 1 2 3))

(transpose (list (list 1 2 3)
                 (list 4 5 6)))

(matrix-*-matrix (list (list 2 6 4)
                       (list 1 6 5)
                       (list 5 9 2))
                 (list (list 4 5 9)
                       (list 9 5 2)
                       (list 1 7 3)))

;Exercise 2.41
(define (sum-to-s n s)
  (define (add-to-lst lst i j k s)
    (cond ((= (+ i j k) s) (append lst (list (list i j k))))
          (else lst)))
  (define (tmp lst i j k n s)
    (cond ((and (= i n) (= j n) (= k n)) (add-to-lst lst i j k s))
          ((and (= j n) (= i n)) (tmp (add-to-lst lst i j k s) 1 1 (+ k 1) n s))
          ((= i n) (tmp (add-to-lst lst i j k s) 1 (+ j 1) k n s))
          (else (tmp (add-to-lst lst i j k s) (+ i 1) j k n s))))
  (tmp '() 1 1 1 n s))

(sum-to-s 5 10)

;Exercise 2.42

