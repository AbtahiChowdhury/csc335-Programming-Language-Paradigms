(define (nth-row-pascle-triangel n)
  (define (fact i)
    (define (fact-iter i total count)
      (cond ((> count i) total)
            (else (fact-iter i (* total count) (+ count 1)))))
    (fact-iter i 1 1))

  (define (choose n k)
    (/ (fact n) (* (fact k) (fact (- n k)))))

  (define (tmp ret n k)
    (cond ((= n k) (append ret (list (choose n k))))
          (else (tmp (append ret (list (choose n k))) n (+ k 1)))))

  (tmp '() n 0))

(nth-row-pascle-triangel 6)