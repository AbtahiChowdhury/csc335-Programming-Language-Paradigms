(define (mysqrt x)
 (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
 (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (myabs (- (square guess) x)) .001))

(define (improve guess x)
 (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (square x) (* x x))

(define (myabs x)
 (if (>= x 0)  
     x
     (- x)))

(mysqrt 4)
(mysqrt 0.000000000000000001)
(mysqrt 99999999999999999999)
