; Exercise 1.7
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

;(mysqrt 4)



; Exercise 1.8

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root-iter (newton-improve guess x) x)))

(define (cube-good-enough? guess x)
  (< (myabs (- (cube guess) x)) .001))

(define (newton-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube x)
  (* (square x) x))

;(cube-root 27)


; Exercise 1.9

;(+ 4 5)
;(inc (+ (dec 4) 5))
;(inc (+ 3 5))
;(inc (inc (+ (dec 3) 5)))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ (dec 2) 5))))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9


;(+ 4 5)
;(+ (dec 4) (inc 5))
;(+ 3 6)
;(+ (dec 3) (inc 6))
;(+ 2 7)
;(+ (dec 2) (inc 7))
;(+ 1 8)
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9
