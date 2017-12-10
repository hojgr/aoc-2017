#lang racket

(define (process-char rest last-char cur-char level in-garbage sum)
  (cond
    [(equal? last-char #\!) (list rest "x" level in-garbage sum)]
    [(equal? cur-char #\<) (list rest cur-char level 1 (+ sum (if (= in-garbage 1) 1 0)))]
    [(equal? cur-char #\>) (list rest cur-char level 0 sum)]
    [(= in-garbage 1) (list rest cur-char level in-garbage (+ sum (if (equal? cur-char #\!) 0 1)))]
    [(equal? cur-char #\{) (list rest cur-char (+ level 1) in-garbage sum)]
    [(equal? cur-char #\}) (list rest cur-char (- level 1) in-garbage sum)]
    [else (list rest cur-char level in-garbage sum)]))
    

(define (process rest [last-char "x"] [level 0] [in-garbage 0] [sum 0])
  (match rest
    [(cons first-char rest) (apply process (process-char rest last-char first-char level in-garbage sum))]
    ['() sum]))

(process (string->list (read-line)))