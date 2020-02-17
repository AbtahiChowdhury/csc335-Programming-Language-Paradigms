; Exercise 1.11

; f(n) recursive
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

; f(n) iterative
(define (f n)
  (if (< n 3)
      n
      (f-iter n 3 2 1 0 4)))

(define (f-iter n count last second-last third-last result)
  (if (= n count)
      result
      (f-iter n (+ count 1) result last second-last (+ result (* 2 last) (* 3 second-last)))))

; Test
(f-rec 6)
(f 6)

; Exercise 1.12

(define (pascal-tri level)
  (pt-rec-inc level 1 ""))

(define (pt-rec-inc level count result)
  (if (= level count)
      (pt-rec-dec level (- count 1) result)
      (pt-rec-inc level (+ count 1) (merge-numbers result (combination level count)))))

(define (pt-rec-dec level count result)
  (if (= 0 count)
      result
      (pt-rec-dec level (- count 1) (merge-numbers result (combination level count)))))

(define (merge-numbers first second)
   (string-append first (number->string second)))

(define (combination n k)
  (/ (new-fact n) (* (new-fact k) (new-fact (- n k)))))

(define (new-fact x)
  (fact-iter x 0 1))

(define (fact-iter x count result)
  (if (= count x)
      result
      (fact-iter x (+ count 1) (* (+ count 1) result))))

(pascal-tri 2)