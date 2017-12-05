#lang racket

(define (solve-maze maze [position 0] [steps 0])
  (let ([new-steps (+ steps 1)]
        [maze-length (length maze)])
                   
    (if (<= maze-length position)
        steps
        (let* ([cur-instruction (list-ref maze position)]
               [instruction-step (if (>= cur-instruction 3) -1 1)]
               [inc-instruction (+ cur-instruction instruction-step)]
               [new-position (+ position cur-instruction)]
               [new-maze (list-set maze position inc-instruction)])

          (solve-maze new-maze new-position new-steps)))))

(define (read-input [lst '()])
  (let ([line (read-line)])
    (if (eof-object? line)
        lst
        (read-input (append lst (list (string->number line)))))))

(solve-maze (read-input))