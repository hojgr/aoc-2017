#lang racket

(define (parse-line line)
  (map string->number (string-split line)))

(define (find-diff input [arg-min +inf.0] [arg-max -inf.0])
  (match input
    [(cons head tail) (find-diff tail (min arg-min head) (max arg-max head))]
    [empty? (exact-floor (- arg-max arg-min))]))

(define (parse-input [sum 0])
  (define line (read-line))
  (if (> (string-length line) 0)
      (parse-input (+ sum (find-diff (parse-line line))))
      sum))

(parse-input)