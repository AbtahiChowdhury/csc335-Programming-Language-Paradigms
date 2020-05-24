(define (list-less-than? lst1 lst2)
  (define (list-max lst max)
    (cond ((null? lst) max)
          ((> (car lst) max) (list-max (cdr lst) (car lst)))
          (else (list-max (cdr lst) max))))
  (cond ((> (list-max lst1 (car lst1)) (list-max lst2 (car lst2))) #t)
        (else #f)))

(list-less-than? '(6 5 4 2 9) '(1 5 3 1 8 1 6 1))