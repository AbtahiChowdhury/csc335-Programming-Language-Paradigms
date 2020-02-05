

; Second Homework Set
; CSc 335
; Fall 2019


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a long problem set - you will want to set aside some hours.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Review lectures 3 and 4 before starting ...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs must be given for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Abelson and Sussman, Problems 1.11 and 1.12

; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

;(/ 37 7) ; the procedure / does actual division. if two integers are dividing it can become a rational number
;(quotient 7 7) ; quotient returns the value of how many time a number goes into another. 7 quotient 7 is 1, 14 quotient 7 is 2.. etc.
;(modulo 37 -7) ; modulo returns the remainder: 37 modulo 7 returns 2 while 37 quotient 7 returns 5.
;(remainder 347 10)
;(quotient 347 10)
;(remainder 34 10)
;(quotient 34 10)
;(remainder 3 10)
;(quotient 3 10)
;(remainder 0 10)

;Design Idea for recursive scheme function: the idea is to take in an integer number n and determine the remainder of n and 10.
;The remainder will give us the right most digit of a number that we will add the other numbers. we will use quotient to reduce the number
;n by removing the right most digit, this result will be added to the net call of the recursive function. The function will terminate
;when remainder is equal to zero and return an integer of the sum of the digits in the original n.

;(define (sum-of-digits n )
;  (cond((zero? (remainder n 10))n)
;       (else (+ (remainder n 10) (sum-of-digits (quotient n 10))))))
;(sum-of-digits 345)
;(sum-of-digits 0)
;wE see that the function works for the basis case of 0 by returning zero. Assumin that the function works, calling the functioin on a
;number n the function will return the correct sum.  


;Design idea iterative function: The preconditions are that the integer that the function is operating on must be an integer. The
;function will have parameters n for the number and sum to store the sum. At each call of the function the number n will be reduced by one
;right most digit while being added to the sum. The function will return the sum when the remainder of the number n is 0;

;(define (sum-of-digits n)
;  (define (iter number sum)
;    (cond ((zero? (/ number 10)) sum)
;          (else(iter (quotient number 10) (+ sum (modulo number 10))))))
;
;  (iter n 0))
;  
;(sum-of-digits 999999999)

;This program work for numbers that do not have a 0 digit in it. Our stopping condition must be improved
; so i change the remainder to division.



; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

;Design idea for recursive program: We will be testing if the number is in non-decreasing order by checking if the number is in
;decreasing order from right to left. At each call of the function we will test if the current value is greater then the one on its
;left. When the number is not it will return a false. When every number has been tested then we will return false.
;
;The pre condition is that the number must be a positive integer.
;
;(define (test-inc-order n)
;  (cond ((zero? (/ n 10)) #t)
;        ((< (remainder n 10) (remainder (quotient n 10) 10))#f)
;        (else ( test-inc-order (quotient n 10)))))
;
;  (test-inc-order 1230)

;Design idea for the iterative version: We will keep track of the current number and the previus number. As soon as the previus number
;is smaller than the next number, from righ to left, then we will return falls at that call of iter.

;(define (test-inc-order n)
;  (define (iter number right-digit left-digit)
;    (cond ((zero? (/ number 10)) #t)
;          ((< right-digit left-digit)#f)
;          (else (iter (quotient number 10)(remainder number 10)(remainder(quotient number 10) 10)))))
;  (iter n (remainder n 10)(remainder(quotient n 10) 10))
;  )
;(test-inc-order 1234)






; You may find the built-in functions quotient, remainder, truncate, zero? -- and perhaps others --
; helpful as you design your solutions for problems 2 and 3.  Have a look at the Scheme
; manual.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
