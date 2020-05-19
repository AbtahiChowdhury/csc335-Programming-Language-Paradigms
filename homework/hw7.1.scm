; An L-expression is an S-expression which is either:
;   (AND l1 l2), or
;   (OR l1 l2), or
;   (NOT l1), or
;   an arbitrary symbol, which we call a variable

; lexp ::= (AND lexp lexp) | (OR lexp lexp) | (NOT lexp) | atom

; Part A
;function to determine if something is an lexp
;check if list, if yes check car for AND or OR or NOT and if their are, check coresponding lexps are indeed lexps.
;if its not, then its an atom (ie a variable)
(define (lexp? l)
  (cond ((and (not (pair? l)) (not (null? l))) #t)
        (else (cond ((or (eq? (car l) 'AND) (eq? (car l) 'OR)) (cond ((and (lexp? (car (cdr l))) (lexp? (car (cdr (cdr l))))) #t)
                                                                     (else #f)))
                    ((eq? (car l) 'NOT) (cond ((lexp? (car (cdr l))) #t)
                                              (else #f)))
                    (else #f)))))

;(lexp? 0)
;(lexp? 1)
;(lexp? 2)
;(lexp? (list 'AND 0 1))
;(lexp? (list 'OR 1 1))
;(lexp? (list 'NOT 1))
;(lexp? (list 2 3 4))


; Part B
;preform a tree walk and leaf nodes (atoms) are checked if their in los
(define (in-los? l los)
  (cond ((null? los) #f)
        ((eq? (car los) l) #t)
        (else (in-los? l (cdr los)))))

;(in-los? 'x (list 'x 'y 'z))
;(in-los? 'w (list 'x 'y 'z))

(define (covered? l los)
  (cond ((and (not (pair? l)) (not (null? l))) (cond ((in-los? l los) #t)
                                                     (else #f)))
        (else (cond ((or (eq? (car l) 'AND) (eq? (car l) 'OR)) (and (covered? (car (cdr l)) los) (covered? (car (cdr (cdr l))) los)))
                    (else (covered? (car (cdr l)) los))))))

;(covered? (list 'AND 'x 'y) (list 'x 'y 'z))
;(covered? (list 'AND 'x 'y) (list 'y 'z))
;(covered? (list 'NOT 'x) (list 'x 'y 'z))


; Part C
;cdr down alist until car of car of alist eq? var, then return
(define (make-alist-comp key value)
  (list key value))
(define (get-key alist-comp)
  (car alist-comp))
(define (get-value alist-comp)
  (car (cdr alist-comp)))

(define (lookup var al)
  (cond ((null? al) '())
        ((eq? (get-key (car al)) var) (get-value (car al)))
        (else (lookup var (cdr al)))))

;(list (make-alist-comp 'x 1)
;      (make-alist-comp 'y 0)
;      (make-alist-comp 'z 1))
;(lookup 'x (list (make-alist-comp 'x 1)
;                 (make-alist-comp 'y 0)
;                 (make-alist-comp 'z 1)))
;(lookup 'y (list (make-alist-comp 'x 1)
;                 (make-alist-comp 'y 0)
;                 (make-alist-comp 'z 1)))


; Part D

(define (make-los-lst al)
  (cond ((null? al) '())
        (else (append (list (get-key (car al))) (make-los-lst (cdr al))))))
;(make-los-lst (list (make-alist-comp 'x 1)
;                    (make-alist-comp 'y 0)
;                    (make-alist-comp 'z 1)))

;check if closed, if so then find its value

(define (bool-and l1 l2)
  (cond ((and (= l1 1) (= l2 1)) 1)
        (else 0)))
(define (bool-or l1 l2)
  (cond ((and (= l1 0) (= l2 0)) 0)
        (else 1)))
(define (bool-not l1)
  (cond ((= l1 0) 1)
        (else 0)))

(define (value lexp al)
  (cond ((and (not (pair? lexp)) (not (null? lexp))) (lookup lexp al))
        (else (cond ((eq? (car lexp) 'AND) (bool-and (value (car (cdr lexp)) al) (value (car (cdr (cdr lexp))) al)))
                    ((eq? (car lexp) 'OR) (bool-or (value (car (cdr lexp)) al) (value (car (cdr (cdr lexp))) al)))
                    (else (bool-not (value (car (cdr lexp)) al)))))))

(list 'NOT (list 'OR (list 'AND 'x 'y) 'z))
(list (list 'x 1) (list 'y 0) (list 'z 0))

(value
 (list 'NOT (list 'OR (list 'AND 'x 'y) 'z))
 (list (list 'x 1) (list 'y 0) (list 'z 0)))
 













  