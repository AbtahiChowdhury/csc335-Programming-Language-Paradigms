(define (sorted? n)
  (sorted-iter n 0))

(define (sorted-iter number prev-digit)
  (cond ((= number 0) #t)
        ((< (modulo number 10) prev-digit) #f)
        (else (sorted-iter (quotient number 10) (modulo number 10)))))



;Termination: number of digits in number is 0
;gI: number of sorted digits + number of unsorted digits = total number of digits
;Invarience?: 


;pre  => insert is a single digit non-negative integer
;        count is 0
;        result is a sorted string of numbers
;post => result has insert inserted into it in its sorted position
(define (insertion-insert-iter insert count result)
  (cond ((= count (string-length result)) (string-append result (number->string insert)))
        ((string>=? (substring result count (+ count 1)) (number->string insert)) (insertion-insert-iter insert (+ count 1) result))
        (else  (string-append (substring result 0 count) (number->string insert) (substring result count)))))

;pre  => number is a non-negative integer
;        result is an empty string
;post => number with it's digits sorted in decreasing order
(define (sort-digits-iter number result)
  (cond ((= number 0) (string->number result))
        (else (sort-digits-iter (quotient number 10) (insertion-insert-iter (modulo number 10) 0 result)))))

;pre  => number is a non-negative number
;post => number with it's digits sorted in decreasing order
(define (sort-iter number)
  (sort-digits-iter number ""))

(sort-iter 451021856200)




;pre  => number is a non-negative number
;post => number with it's digits sorted in decreasing order
(define (sort-rec number)
  (cond ((= number 0) "")
        (else (insertion-insert-iter (modulo number 10) 0 (sort-rec (quotient number 10))))))

(sort-rec 451021856200)