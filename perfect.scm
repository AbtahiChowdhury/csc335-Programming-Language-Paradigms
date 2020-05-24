;preCond: n is integer > 1
;postCond: returns true if n is a "perfect" integer
(define (perfect? n)
  ;pre: n is non-negative integer, count is 1, sum is 0
  ;post: (sum of n's factors) - n 
  (define (sum-factors n count sum)
    (cond ((= count n ) sum)
          ((= (modulo n count) 0) (sum-factors n (+ count 1) (+ sum count)))
          (else (sum-factors n (+ count 1) sum))))

  (cond ((= n (sum-factors n 1 0)) #t)
        (else #f)))

(perfect? 28)

;Design Idea: iterate through integers from 0->N and test if it is a factor of N using modulo
;             if it is a factor, add it to the sum
;             at the end, check if sum equals N
;gI: sum = (sum of divisors of N seen so far) + (remaining divisors of N)
;
;Case 1: init
;initially sum is 0 because no divisors of N have been seen yet
;
;Case 2: termination
;on each iteration, the difference between N and count decreases by 1 due to count starting at 0 and
;being incremented by 1 every iteration. As the code shows, it will terminate once count becomes N [(= count n )]
;and return the value of sum with the assumption that the function worked correctly up to this point.
;
;Case 3: preservation
;we assume that the calls up to some number k were done correctly and sum = sum of the divisors of N that are less then
;k. on the k+1 call there are two possibilities of what can happen:
;1. as shown in the code, on the k+1 call, if count is divisibale by N [(= (modulo n count) 0)], it is added to the sum [(sum-factors n (+ count 1) (+ sum count))]
;2. as shown in the code, on the k+1 call, if count is not divisibale by N [else], it is not added to the sum [(sum-factors n (+ count 1) sum)]
;
;since we know that sum-factors is correct, we simply compare it to n to fulfill the post condition