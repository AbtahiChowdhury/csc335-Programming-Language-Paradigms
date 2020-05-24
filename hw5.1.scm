; Exercise 1
(display "Exercise 1")
(newline)

(define make-+
  (lambda (e1 e2)
    (list e1 '+ e2)))
(define make-*
  (lambda (e1 e2)
    (list e1 '* e2)))
(define make-^
  (lambda (e1 e2)
    (list e1 '^ e2)))

(define add1
  (lambda (m) (+ m 1)))
(define sub1
  (lambda (m) (- m 1)))
(define plus 
  (lambda (m n)
    (cond ((= n 0) m)
          (else (add1 (plus m (sub1 n)))))))
(define times 
  (lambda (m n)
    (cond ((= n 0) 0)
          (else (plus m (times m (sub1 n)))))))
(define expon
  (lambda (base exponent)
    (cond ((zero? exponent) 1)
          (else (times base (expon base (sub1 exponent)))))))
(define operator
  (lambda (i-aexp) 
    (car (cdr i-aexp))))
(define first-operand
  (lambda (i-aexp)
    (car i-aexp)))
(define second-operand
  (lambda (i-aexp)
    (car (cdr (cdr i-aexp)))))
(define atom?
  (lambda (x)
    (and (not (null? x)) 
	 (not (pair? x)))))
(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x '+) plus)
      ((eq? x '*) times)
      (else expon))))
(define value
  (lambda (aexp)
    (cond ((atom? aexp) aexp)
          (else ((atom-to-function (operator aexp))
                 (value (first-operand aexp))
                 (value (second-operand aexp)))))))

(make-+ (make-* 4 5) (make-^ 2 3))
(value (make-+ (make-* 4 5) (make-^ 2 3)))

;(define value
;  (lambda (aexp)
;    (cond ((atom? aexp) aexp)
;          (else ((atom-to-function (operator aexp))
;                 (value (first-operand aexp))
;                 (value (second-operand aexp)))))))
;IH: assume the recursive calls of value work and is done on smaller aexps
;    also assume atom-to-function, operator, first-operand, and second-operand work
;    smaller aexps => the 3 components of an aexp are either atoms or other aexps
;                     so we take the cdar, car, and cddar of the operation, first
;                     operand, and second operand respectively
;Basis: the complexity of aexp is as low as it can get (ie aexp is an atom)
;IS: by the induction assumption, we see that we have the value of 2 aexp and the operator for the 2.
;    to fulfill the post condition, simply preform the overation on the 2 values. This can be seen in
;    the code in the else cause of the cond where the operator is converted into its respective procedure
;    and the value of the 2 operands are used in that procedure

; Exercise 2 ;; DO LATER
(newline)
(display "Exercise 2")
(newline)

(define (num-to-base-1 n)
  (cond ((= 0 n) '())
        (else (cons 1 (num-to-base-1 (- n 1))))))
(define (base-1-to-num lst)
  (cond ((null? lst) 0)
        (else (+ (car lst) (base-1-to-num (cdr lst))))))

(define make-+
  (lambda (e1 e2)
    (list e1 '+ e2)))
(define make-*
  (lambda (e1 e2)
    (list e1 '* e2)))
(define make-^
  (lambda (e1 e2)
    (list e1 '^ e2)))

(define add1
  (lambda (m) (+ m 1)))
(define sub1
  (lambda (m) (- m 1)))
(define plus 
  (lambda (m n)
    (cond ((= n 0) m)
          (else (add1 (plus m (sub1 n)))))))
(define times 
  (lambda (m n)
    (cond ((= n 0) 0)
          (else (plus m (times m (sub1 n)))))))
(define expon
  (lambda (base exponent)
    (cond ((zero? exponent) 1)
          (else (times base (expon base (sub1 exponent)))))))
(define operator
  (lambda (i-aexp) 
    (car (cdr i-aexp))))
(define first-operand
  (lambda (i-aexp)
    (car i-aexp)))
(define second-operand
  (lambda (i-aexp)
    (car (cdr (cdr i-aexp)))))
(define atom-to-function
  (lambda (x)
    (cond 
      ((eq? x '+) plus)
      ((eq? x '*) times)
      (else expon))))
(define value
  (lambda (aexp)
    (cond ((number? (car aexp)) (base-1-to-num aexp))
          (else ((atom-to-function (operator aexp))
                 (value (first-operand aexp))
                 (value (second-operand aexp)))))))

(make-+ (make-* (num-to-base-1 4) (num-to-base-1 5)) (make-^ (num-to-base-1 2) (num-to-base-1 3)))
(value (make-+ (make-* (num-to-base-1 4) (num-to-base-1 5)) (make-^ (num-to-base-1 2) (num-to-base-1 3))))


; Exercise 2.2
(newline)
(display "Exercise 2.2")
(newline)

(define make-segment
  (lambda (start end)
    (list start end)))
(define start-segment
  (lambda (segment)
    (car segment)))
(define end-segment
  (lambda (segment)
    (car (cdr segment))))

(define make-point
  (lambda (x y)
    (list x y)))
(define x-point
  (lambda (point)
    (car point)))
(define y-point
  (lambda (point)
    (car (cdr point))))

(define (point-segment-test)
  (let ((x1 3) (y1 5) (x2 4) (y2 6))
    (let ((point1 (make-point x1 y1)) (point2 (make-point x2 y2)))
      (let ((segment (make-segment point1 point2)))
        ;do things with the segment
        segment))))
(point-segment-test)

; Exercise 2.3
(newline)
(display "Exercise 2.3")
(newline)

(define make-rectangle
  (lambda (center-point length width)
    (list (make-point (- (x-point center-point) (/ length 2)) (+ (y-point center-point) (/ width 2)))
          (make-point (+ (x-point center-point) (/ length 2)) (+ (y-point center-point) (/ width 2)))
          (make-point (+ (x-point center-point) (/ length 2)) (- (y-point center-point) (/ width 2)))
          (make-point (- (x-point center-point) (/ length 2)) (- (y-point center-point) (/ width 2))))))
(define point-of-rectangle
  (lambda (rectangle n)
    (cond ((= n 1) (car rectangle))
          ((= n 2) (car (cdr rectangle)))
          ((= n 3) (car (cdr (cdr rectangle))))
          ((= n 4) (car (cdr (cdr (cdr rectangle))))))))
(define perimeter
  (lambda (rectangle)
    (+ (* 2 (- (x-point (point-of-rectangle rectangle 2)) (x-point (point-of-rectangle rectangle 1))))
       (* 2 (- (y-point (point-of-rectangle rectangle 2)) (y-point (point-of-rectangle rectangle 3)))))))
(define area
  (lambda (rectangle)
    (* (- (x-point (point-of-rectangle rectangle 2)) (x-point (point-of-rectangle rectangle 1)))
       (- (y-point (point-of-rectangle rectangle 2)) (y-point (point-of-rectangle rectangle 3))))))

(make-rectangle (make-point 0 0) 4 2)
(point-of-rectangle (make-rectangle (make-point 0 0) 4 2) 2)
(perimeter (make-rectangle (make-point 0 0) 4 2))
(area (make-rectangle (make-point 0 0) 4 2))

; Exercise 2.4
(newline)
(display "Exercise 2.4")
(newline)

(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

(my-car (my-cons 2 3))
(my-cdr (my-cons 2 3))
  