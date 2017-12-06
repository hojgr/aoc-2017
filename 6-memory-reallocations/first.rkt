#lang racket

(define (find-highest lst)
  (let* ([highest-number (apply max lst)]
         [highest-number-index (index-of lst highest-number =)])
    (cons highest-number-index highest-number)))

(define (list-redistribute lst last-idx redistribute-budget)
  (if (= redistribute-budget 0)
      lst
      (let* ([idx (modulo (+ last-idx 1) (length lst))]
             [new-budget (- redistribute-budget 1)]
             [idx-num (list-ref lst idx)]
             [idx-num-incremented (+ idx-num 1)]
             [new-lst (list-set lst idx idx-num-incremented)])
        (list-redistribute new-lst idx new-budget))))



(define (solve lst [history '()] [iterations 0])
  (if (not (eq? (member lst history) #f))
      iterations
      (match-let ([(cons h-index h-val) (find-highest lst)])
                 (solve (list-redistribute (list-set lst h-index 0) h-index h-val)
                        (cons lst history)
                        (+ iterations 1)))))
 
(solve '(10 3 15 10 5 15 5 15 9 2 5 8 5 2 3 6))