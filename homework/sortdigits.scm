(define (sorted? n)
  (sorted-iter n 0))

(define (sorted-iter number prev-digit)
  (cond ((= number 0) #t)
        ((< (modulo number 10) prev-digit) #f)
        (else (sorted-iter (quotient number 10) (modulo number 10)))))

