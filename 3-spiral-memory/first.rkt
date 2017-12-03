#lang racket

(require racket/struct)

(struct point (level x y number) #:transparent)

(define (get-next-step current-point)
  (match current-point
    [(point 0 0 0 _) (point 1 1 0 2)] ; The `1` doesnt algoritmically fit. Needs to be predefined
    [p (get-next-step-non-origin p)])) ;(get-next-step-normalized level x y)]))

(define (get-next-step-non-origin current-point)
  (match-let ([(point level x y number) current-point])

    (define half-side-floored (/ (- (get-side-length level) 1) 2))

    (define right? (= x half-side-floored))
    (define left? (= x (- half-side-floored)))
    (define top? (= y half-side-floored))
    (define bottom? (= y (- half-side-floored)))

    (define next-number (+ number 1))
    
    (define recalculated-level (if (and right? bottom?) (+ level 1) level))
    
    (match-let ([(cons new-x new-y) (cond
                                        [(and right? top?)   (cons (- x 1) y)]
                                        [(and left? top?)  (cons x (- y 1))]
                                        [(and left? bottom?) (cons (+ x 1) y)]
                                        [(and right? bottom? (cons (+ x 1) y))] ; same as left? bottom? but for good measure
                                        [right?  (cons x (+ y 1))]
                                        [top?    (cons (- x 1) y)]
                                        [left?   (cons x (- y 1))]
                                        [bottom? (cons (+ x 1) y)])])

      (point recalculated-level new-x new-y next-number))))
    

(define (get-side-length level)
  (match level
    [0 1]
    [n (+ 2 (get-side-length (- level 1)))]))

(define (find-point lastPoint num)
  (match-let ([(point level x y curNumber) lastPoint])
              (if (= num curNumber)
                  lastPoint
                  (find-point (get-next-step lastPoint) num))))

(match-let ([(point _ x y _) (find-point (point 0 0 0 1) 368078)])
  (+ (abs x) (abs y)))

