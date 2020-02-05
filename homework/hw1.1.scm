; Exercise 1.1
10                                       ;10
(+ 5 3 4)                                ;12
(- 9 1)                                  ;8
(/ 6 2)                                  ;3
(+ (* 2 4) (- 4 6))                      ;6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))                          ;19
(= a b)                                  ;#f
(if (and (> b a) (< b (* a b)))          ;4
    b
    a)
(cond ((= a 4) 6)                        ;16
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))                   ;6
(* (cond ((> a b) a)                     ;16
         ((< a b) b)
         (else -1))
   (+ a 1))

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(define (sum-of-squares-of-larger-two x y z)
  (cond ((and (< x y) (< x z)) (+ (* y y) (* z z)))
        ((< y z) (+ (* x x) (* z z)))
        (else (+ (* x x) (* y y)))))
(sum-of-squares-of-larger-two 2 3 4)

; Exercise 1.4
(define (a-plus-abs-b a b)             ;If b is positive it does (+ a b)
  ((if (> b 0) + -) a b))              ;else is does (- a b) and since b
(a-plus-abs-b 3 6)                     ;is negative, it does a-(-b)=a+b
(a-plus-abs-b 3 -6)

; Exercise 1.5
(define (p)                            ;In applicative, it will be stuck in
  (p))                                 ;an infinite loop when evaluating (p). 
(define (test x y)                     ;In normal the output will be 0 b/c
  (if (= x 0) 0 y))                    ;(p) is never evaluated.
(test 0 (p))                           ;This interperter is applicative.