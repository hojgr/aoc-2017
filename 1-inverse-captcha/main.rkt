#lang racket

(require racket/match)

(define (eval-comparison sum items)
  (define nextSum (if (= (first items) (second items))
                   (+ sum (first items))
                   sum)
    )
  
    (captcha nextSum (rest items))
  )

(define (captcha sum items)
  (if (= (length items) 1)
      sum
      (eval-comparison sum items)
  )
  )
  
  

(captcha 0 (string->list "1122"))