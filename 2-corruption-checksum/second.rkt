#lang racket

(define (parse-line line)
  (map string->number (string-split line)))

(define (devide-both-ways x y)
  (define xy (/ x y))
  (define yx (/ y x))

  (cond
    [(integer? xy) xy]
    [(integer? yx) yx]
    [else 'none]))

(define (process-devision-logic x input-rest)
  (cond
    [(empty? input-rest) 'none]
    [else
     (define y (first input-rest))
     (define tail (rest input-rest))
  
     (match (devide-both-ways x y)
       ['none (process-devision-logic x tail)]
       [n n])]))

(define (resolve-line input)
  (cond
    [(empty? input) 0]
    [else
     (define head (first input))
     (define tail (rest input))
  
     (match (process-devision-logic head tail)
       ['none (resolve-line tail)]
       [n n])]))


(define (parse-input [sum 0])
  (define line (read-line))
  (if (> (string-length line) 0)
      (parse-input (+ sum (resolve-line (parse-line line))))
      sum))


(parse-input)