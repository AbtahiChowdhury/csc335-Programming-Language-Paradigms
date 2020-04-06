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

;guess idea: 