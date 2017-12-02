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
  
(define input (read-line))
(define convertedInput (map (lambda (x) (- (char->integer x) 48)) (string->list input)))
(define circularConvertedInput  (list* (last convertedInput) convertedInput))

(captcha 0 circularConvertedInput)