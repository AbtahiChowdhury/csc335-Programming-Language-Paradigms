; Excercise 1

;guess idea, run cdr until it returns null and inc count
(define (my-length-rec x)
  (cond ((null? x) 0)
        (else (+ 1 (my-length-rec (cdr x))))))

(define (my-length-iter x)
  (define (tmp-f x count)
    (cond ((null? x) count)
          (else (tmp-f (cdr x) (+ count 1)))))
  (tmp-f x 0))

(my-length-rec '(a b c d))
(my-length-iter '(a b c d))

; Excercise 2
(define (my-list-ref-rec x i)
  (cond ((= i 0) (car x))
        (else (my-list-ref-rec (cdr x) (- i 1)))))

(define (my-list-ref-iter x i)
  (define (tmp-f x i count)
    (cond ((= i count) (car x))
          (else (tmp-f (cdr x) i (+ count 1)))))
  (tmp-f x i 0))

(my-list-ref-rec '(a b c d) 2)
(my-list-ref-iter '(a b c d) 2)

; Excercise 3
(define (first-elems-rec lst num)
  (cond ((= num 0) '())
        (else (cons (car lst) (first-elems-rec (cdr lst) (- num 1))))))
         
(define (first-elems-iter lst num)
  (define (tmp-f lst num new-lst count)
    (cond ((= count num) new-lst)
          (else (tmp-f (cdr lst) num (cons (car lst) new-lst) (+ count 1)))))
  (tmp-f lst num '() 0))

(first-elems-rec '(a b c d e f) 3)
(first-elems-iter '(a b c d e f) 3)

; Excercise 4
(define (but-last-iter lst num)
  (define (my-length x count)
    (cond ((null? x) count)
          (else (my-length (cdr x) (+ count 1)))))
  (define (tmp-f lst num new-lst count)
    (cond ((= count num) new-lst)
          (else (tmp-f (cdr lst) num (cons (car lst) new-lst) (+ count 1)))))
  (tmp-f lst (- (my-length lst 0) num) '() 0))

(but-last-iter '(a b c d e f g h i) 3)
  

; Excercise 5

