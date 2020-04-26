; A simple recursive idea for finding all prime factors of a positive integer n can be described as follows:

;;     prime-factors(1) = {} (the empty set)
;;     prime-factors(p) = {p} if p is prime
;;     prime-factors(n) = prime-factors(a) U prime-factors(b), where n = ab

; Using this approach, develop a complete R5RS program prime-factorization which inputs an
; integer n >= 1 and returns a list of pairs of the form <prime, power>,
; where prime occurs power times in the prime factorization of n.  Thus

;;    (prime-factorization 1) = ()
;;    (prime-factorization 7) = ((7 1))
;;    (prime-factorization 30) = ((2 1) (3 1) (5 1))
;;    (prime-factorization 100) = ((2 2) (5 2))
;;    (prime-factorization 1024) = ((2 10))

; The output list must be sorted by primes, with smaller prime numbers appearing before
; larger prime numbers.

; In addition,
;
;   the factors a and b of n must be chosen as close in size to each other as possible (for 200, for example, a = 10 and b = 20)
;
;   your code must be purely functional.
;
;   complete proofs of your major functions must be included.


;IH: assuming all previous calls are correct, prime-factors is called on a smaller value of n
;Basis: n is 1
;IS: by the induction assumption, we can see that we have a prime factor of n, which is i, and a
;    list of the rest of the prime factors of n which are greater than i. so we would then need
;    to glue then together by simply calling the scheme append function on i and the list.
;(define (prime-factors n i)
;  (cond ((= n 1) '())
;        ((and (= (modulo n i) 0) (is-prime i)) (append (list i) (prime-factors (/ n i) i)))
;        (else (prime-factors n (+ i 1)))))


;gI: sum of the first elements of the sublists of ret + the length of lst + (count - 1) = the length of L
;
;init: initially ret is empty as as not values of lst have been checked so length lf l + 1 - 1 = length og L
;      this can be seen in the code [ (count-numbers (prime-factors n 2) '() (car (prime-factors n 2)) 1) ]
;termination: terminates when lst is empty as all the elements of lst have been counted and appended to ret and
;             returned. this can be seen in the code [ ((null? lst) (append ret (list (list count curr)))) ]
;preservation: assuming all cases up to now are correct, there are 2 cases
;              case 1: if car of lst is equal to the previous element then on the next iteration count is incremented
;                      by 1. This can be seen in the code [ ((equal? (car lst) curr) (count-numbers (cdr lst) ret curr (+ count 1))) ]
;              case 2: if car of lst is not equal to the previous element then on the next iteration the list (count curr)
;                      appended to ret, curr is set to the car of lst and count is 1. This can be seen in the code
;                      [ (else (count-numbers (cdr lst) (append ret (list (list count curr))) (car lst) 1)) ]
;(define (count-numbers lst ret curr count)
;  (cond ((null? lst) (append ret (list (list count curr))))
;        ((equal? (car lst) curr) (count-numbers (cdr lst) ret curr (+ count 1)))
;        (else (count-numbers (cdr lst) (append ret (list (list count curr))) (car lst) 1))))

;now that we know that prime-factors and count-numbers work, we simply provide the result of prime-factors and it's car to lst and curr
;of count-numbers respectivly

(define (prime-factorization n)
  (define (is-prime n)
    (define (tmp n c)
      (cond ((= c 1) #t)
            ((= (modulo n c) 0) #f)
            (else (tmp n (- c 1)))))
    (cond ((= n 1) #f)
          (else (tmp n (- n 1)))))

  (define (prime-factors n i)
    (cond ((= n 1) '())
          ((and (= (modulo n i) 0) (is-prime i)) (append (list i) (prime-factors (/ n i) i)))
          (else (prime-factors n (+ i 1)))))

  (define (count-numbers lst ret curr count)
    (cond ((null? lst) (append ret (list (list count curr))))
          ((equal? (car lst) curr) (count-numbers (cdr lst) ret curr (+ count 1)))
          (else (count-numbers (cdr lst) (append ret (list (list count curr))) (car lst) 1))))

  (cond ((= n 1) '())
        ((is-prime n) (list n))
        (else (count-numbers (prime-factors n 2) '() (car (prime-factors n 2)) 1))))

(prime-factorization 1)
(prime-factorization 7)
(prime-factorization 30)
(prime-factorization 100)
(prime-factorization 1024)