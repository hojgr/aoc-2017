#lang racket

(require racket/struct)

(struct point (level x y number history) #:transparent)

(define (get-next-step current-point)
  (match current-point
    [(point 0 0 0 _ h) (point 1 1 0 1 (list (cons (cons 0 0) current-point)))] ; The `1` doesnt algoritmically fit. Needs to be predefined
    [p (get-next-step-non-origin p)])) ;(get-next-step-normalized level x y)]))

(define (get-next-step-non-origin current-point)
  (match-let ([(point level x y number history) current-point])

    (define half-side-floored (/ (- (get-side-length level) 1) 2))

    (define right? (= x half-side-floored))
    (define left? (= x (- half-side-floored)))
    (define top? (= y half-side-floored))
    (define bottom? (= y (- half-side-floored)))

    (define recalculated-level (if (and right? bottom?) (+ level 1) level))
    
    (match-let ([(cons new-x new-y) (cond
                                        [(and right? top?)   (cons (- x 1) y)]
                                        [(and left? top?)    (cons x (- y 1))]
                                        [(and left? bottom?) (cons (+ x 1) y)]
                                        [(and right? bottom? (cons (+ x 1) y))] ; same as (and left? bottom?) but for good measure
                                        [right?  (cons x (+ y 1))]
                                        [top?    (cons (- x 1) y)]
                                        [left?   (cons x (- y 1))]
                                        [bottom? (cons (+ x 1) y)])])

      (define new-history (cons (cons (cons x y) current-point) history))
      (define next-number (get-next-number-from-history new-history new-x new-y))

      (point recalculated-level new-x new-y next-number new-history))))
      
(define (get-next-number-from-history history x y)
  (define neighbors (list (cons -1 1)
                          (cons 0 1)
                          (cons 1 1)
                          (cons 1 0)
                          (cons 1 -1)
                          (cons 0 -1)
                          (cons -1 -1)
                          (cons -1 0)))

  (sum-neighbors history x y neighbors))

(define (sum-neighbors history x y neighbors [sum 0])
  ; n-diff -> neighbor difference
  (if (empty? neighbors)
      sum
      (match-let ([(cons (cons n-diff-x n-diff-y) neighbors-rest) neighbors])
        (define n-x (+ x n-diff-x))
        (define n-y (+ y n-diff-y))
    
        (if (dict-has-key? history (cons n-x n-y))
            (match-let ([(point _ _ _ val _) (dict-ref history (cons n-x n-y))])
              (sum-neighbors history x y neighbors-rest (+ sum val)))
            (sum-neighbors history x y neighbors-rest sum)))))
          

(define (get-side-length level)
  (match level
    [0 1]
    [n (+ 2 (get-side-length (- level 1)))]))

(define (find-point lastPoint num)
  (match-let ([(point level x y curNumber history) lastPoint])
              (if (> curNumber num)
                  lastPoint
                  (find-point (get-next-step lastPoint) num))))

(match-let ([(point _ _ _ val _) (find-point (point 0 0 0 1 '()) 368078)])
  val)

