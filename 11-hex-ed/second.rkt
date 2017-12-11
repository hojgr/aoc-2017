#lang racket

(define (direction->vector dir)
  (match dir
    ["nw" (vector -1/2 1/2)]
    ["n" (vector 0 1)]
    ["ne" (vector 1/2 1/2)]
    ["se" (vector 1/2 -1/2)]
    ["s" (vector 0 -1)]
    ["sw" (vector -1/2 -1/2)]))

(define (advance pos change-vec)
  (match-let* ([(vector x y furthest) pos]
         [(vector plus-x plus-y) change-vec]
         [new-x (+ x plus-x)]
         [new-y (+ y plus-y)])
    (vector new-x new-y (max furthest (hex-distance (vector new-x new-y))))))

(define (solve moves [pos (vector 0 0 0)])
  (match moves
    [(cons move rest-moves) (solve rest-moves (advance pos (direction->vector move)))]
    [_ pos]))

(define (hex-distance pos)
  (match-let* ([(vector x y) pos]
               [x-steps (abs x)]
               [y-steps (abs y)])
    (+ x-steps y-steps)))

(solve (string-split (read-line) ","))