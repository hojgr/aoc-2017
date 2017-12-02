#lang racket

(require racket/match)

(define (fn-halfway-eq? items cur)
  (define curNum (list-ref items cur))
  (define steps (/ (length items) 2))
  (define matchingPos (modulo (+ cur steps) (length items)))
  (define toMatchNum (list-ref items matchingPos))

  (= curNum toMatchNum)
  )

(define (eval-comparison items cur sum)
  (define newSum
    (if (fn-halfway-eq? items cur)
      (+ sum (list-ref items cur))
      sum
      )
    )
  (captcha items (+ cur 1) newSum)
  )

(define (captcha items cur sum)
  (if (= (length items) cur)
      sum
      (eval-comparison items cur sum)
  )
  )
  
(define input (read-line))
(define convertedInput (map (lambda (x) (- (char->integer x) 48)) (string->list input)))

(captcha convertedInput 0 0)