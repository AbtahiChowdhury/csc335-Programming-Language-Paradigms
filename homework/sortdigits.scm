;iterative solution

;design idea:use aux function iterate through initial number and find highest digit, subtract from it
;           (found-digit * place-of-digit) (ex: 232 - (3*10)) and return the digit
;            multiply result-so-far by 10 and add the found digit to it and iterate until initial number is 0
;            use another aux function to get number of zeros and add them to the end of the final number


;pre  => number is a non-negative integer and digit is 0 and count is 1 and place-of-digit is 1
;post => digit is the largest digit in number
;Termination: number of digits in number becomes 0
;gI: number of unchected digits + number of checked digits = total number of digits
(define (find-largest-digit number digit place place-of-digit)
  (cond ((= number 0) (* digit place-of-digit))
        ((< digit (modulo number 10)) (find-largest-digit (quotient number 10) (modulo number 10) (* place 10) place))
        (else (find-largest-digit (quotient number 10) digit (* place 10) place-of-digit))))

;pre  => number is a non-negative integer and count is 0
;post => count is the number of zeros in number
;Termination: number of digits in number becomes 0
;gI: number of unchected digits + number of checked digits = total number of digits
(define (number-of-zeros number count)
  (cond ((= number 0) count)
        ((= 0 (modulo number 10)) (number-of-zeros (quotient number 10) (+ count 1)))
        (else (number-of-zeros (quotient number 10) count))))

;pre  => number has a non-zero leading digit followed by zeros and is positive
;post => leading digit of number
;Termination: last digit of number is no longer 0
;gI: number of unchected digits + number of checked digits = total number of digits
(define (get-number-from-place number)
  (cond ((> (modulo number 10) 0) number)
        (else (get-number-from-place (quotient number 10)))))
; N = total digits
; r = processed digits
; n = unprocessed digits
; initially number of processed digits is 0 and number of unprocessed digits is N making their sum N
; N = n+r = (n-1)+(r+1) = n-1+r+1 = n+r
; given the precondition of n having a leading non-zero number and the rest of the digits being 0, at each iteration the number is divided by 10
; removing the zeros from the number until the remainder of the division by 10 ;eades to a non-zero number, which will happen given the precondition
; is true



;pre  => number is a non-negative integer and result is 0
;post => number with its non-zero digits sorted
;Termination: number is 0
;gI: number of sorted digits + number of unsorted digits + number of zeros = total number of digits
(define (sort-iter number result)
  (cond ((= number 0) result)
        (else (sort-iter (- number (find-largest-digit number 0 1 1)) (+ (* result 10) (get-number-from-place (find-largest-digit number 0 1 1)))))))
; N = total digits
; r = processed digits
; n = unprocessed digits
; initially number of processed digits is 0 and number of unprocessed digits is N making their sum N
; N = n+r = (n-1)+(r+1) = n-1+r+1 = n+r
; given the precondition of n being a non-negative number and that at each iteration it is decrmented by 1, it will reach 0

;pre  => number is a non-negative integer
;post => number with its digits sorted
(define (sort number)
  (* (sort-iter number 0) (expt 10 (number-of-zeros number 0))))

;(get-number-from-place 5000000)
;(number-of-zeros 15632984900613100 0)
;(find-largest-digit 15632984900613100 0 1 1)
;(sort-iter 15632984900613100 0)

15632984900613100
(sort 15632984900613100)





;recursive solution

;same precondition and post condition as sort
;IH: assume (insertion-insert (sort-rec (quotient number 10)) (modulo number 10) 1 0) = n with its digits sorted
;IS: if n has k remaining digits to sort, then (quotient n 10) will have k-1 digits to sort because assuming scheme works,
;    (quotient n 10) will get the floor of n/10, removing the last digit of the number
;BS: when n is less then 10, we have reached the last digit of N

;pre: number is non-negative, insert is a single digit positive number, place is 1, checked is 0
(define (insertion-insert number insert place checked)
  (cond ((= number 0)(+ (* insert place) checked))
        ((> (modulo number 10) insert) (+ (* number place 10) (* insert place) checked))
        (else (insertion-insert (quotient number 10) insert (* place 10) (+ (* (modulo number 10) place) checked)))))

(define (sort-rec number)
  (cond ((< number 10) number)
        (else (insertion-insert (sort-rec (quotient number 10)) (modulo number 10) 1 0))))

;(insertion-insert 421 3 1 0)
15632984900613100
(sort-rec 15632984900613100)