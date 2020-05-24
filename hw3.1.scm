; Exercise 1.29
(define (simpson f a b n)
  (define (simpsonIter f a b n h k sum)
    (cond ((> k n) (* (/ h 3) sum))
          ((or (= k 0) (= k n)) (simpsonIter f a b n h (+ k 1) (+ sum (f (+ a (* k h))))))
          ((= (modulo k 2) 0) (simpsonIter f a b n h (+ k 1) (+ sum (* 2 (f (+ a (* k h)))))))
          (else (simpsonIter f a b n h (+ k 1) (+ sum (* 4 (f (+ a (* k h)))))))))
  (simpsonIter f a b n (/ (- b a) n) 0 0))

(simpson (lambda (x) (expt x (/ 1 3))) 0 1 1000)

; Exercise 1.34

(define (f g) (g 2))
(f (lambda (x) (* x x)))

; Exercise 1.37

(define (cont-frac n d k)
  (define (cont-frac-rec n d k i)
    (cond ((= k i) (/ (n i) (d i)))
          (else (/ (n i) (+ (d i) (cont-frac-rec n d k (+ i 1)))))))
  (cont-frac-rec n d k 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 100000)

; Exercise 1.41

(define (double f)
  (define (do-twice x)
    (f (f x)))
  do-twice)

((double (lambda (x) (+ x 1))) 2)

; Exercise 1.42

(define (compose f g)
  (define (f-then-g x)
    (f (g x)))
  f-then-g)

((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6)

(define (square x) (* x x))
((compose square square) 5)

; Exercise 1.43

(define (repeated f n)
  (define (repeat-iter count)
    (cond ((= count n) (lambda (x) x))
          (else (compose f (repeat-iter (+ count 1))))))
  (repeat-iter 0))

((repeated square 3) 5)