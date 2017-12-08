#lang racket

(require racket/struct)

(struct tower (name weight holds) #:transparent)

(define (string->tower str)
  (match (regexp-match #rx"([a-z]+) \\(([0-9]+)\\)(?: \\-> )?([a-z, ]+)*?$" str)
    [(list _ cur-tower weight #f) (tower cur-tower (string->number weight) '())]
    [(list _ cur-tower weight holds-towers) (tower
                                             cur-tower
                                             (string->number weight)
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

(define (find-tower name all-towers)
  (match-let ([(cons (tower tname weight holds) all-towers-rest) all-towers])
    (if (equal? name tname)
        (tower tname weight holds)
        (find-tower name all-towers-rest))))

(define (build-tree all-towers root)
  (match-let* ([(tower name weight holds) (find-tower root all-towers)]
               [holds-tree (map (λ (twr-name) (build-tree all-towers twr-name)) holds)]
               [children-weights (map (λ (twr)
                                        (match-let ([(tower _ weight _) twr])
                                          weight)) holds-tree)]
               [children-weights-fixed (if (empty? children-weights) '(1) children-weights)] ; hotfix for applying empty array
               [cw-min (apply min children-weights-fixed)]
               [cw-max (apply max children-weights-fixed)]
               [cw-diff (- cw-max cw-min)])

    (if (= cw-diff 0)
        (tower name (+ weight (foldl + 0 children-weights)) holds-tree)
        (list name children-weights-fixed))))
  

  (define xinput (string-split "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)" "\n"))

(define (tower->name twr)
  (match-let ([(tower name _ _) twr])
    name))

(let* ([input xinput]
       [flatmap (map string->tower (read-input))]
       [root (find-root flatmap)]
       [root-name (tower->name root)]
       [result (build-tree flatmap root-name)])
 result)
