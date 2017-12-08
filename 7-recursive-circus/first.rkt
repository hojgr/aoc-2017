#lang racket

(require racket/struct)

(struct tower (name weight holds) #:transparent)

(define (string->tower str)
  (match (regexp-match #rx"([a-z]+) \\(([0-9]+)\\)(?: \\-> )?([a-z, ]+)*?$" str)
    [(list _ cur-tower weight #f) (tower cur-tower weight '())]
    [(list _ cur-tower weight holds-towers) (tower
                                             cur-tower
                                             weight
                                             (string-split holds-towers ", "))]))

(define (find-root flatmap)
  (match flatmap
    [(cons (tower name weight '()) flatmap-rest) (find-root flatmap-rest)]
    [(cons (tower name weight holds) flatmap-rest) (if (belongs-to-any? name flatmap-rest)
                                                       ; append first at the end so that we can look at it in the
                                                       ; following lookups. distorting the array doesnt matter
                                                       ; since it should always be solvable and we
                                                       ; discard the entire array afterwards
                                                   (find-root (append flatmap-rest (list (tower name weight holds))))
                                                   (tower name weight holds))]))

(define (belongs-to-any? name flatmap)
  (match flatmap
    [(cons (tower _ _ holds) flatmap-rest)
     (if (equal? #f (member name holds)) ; `name` is not in `holds`
         (belongs-to-any? name flatmap-rest)
         #t)]
    ['() #f]))


(define (read-input [lst '()])
  (let ([line (read-line)])
    (if (eof-object? line)
        lst
        (read-input (cons line lst)))))



(let* ([input (read-input)]
       [flatmap (map string->tower input)]
       [root (find-root flatmap)])
 root)
