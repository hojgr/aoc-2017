#lang racket

(struct instruction (man-reg man-fn man-num  cond-reg cond-fn cond-num) #:transparent)

(define (recognize-fn str-fn)
  (match str-fn
    [">" >]
    ["<" <]
    ["==" =]
    ["!=" (λ (a b) (not (= a b)))]
    [">=" >=]
    ["<=" <=]
    ["inc" +]
    ["dec" -]))

(define (string->instruction ins)
  (match-let ([(list _ a b c d e f) (regexp-match #rx"^([a-z]+) (inc|dec) ([0-9\\-]+) if ([a-z]+) ([<>=!]+) ([0-9\\-]+)$" ins)])
    (instruction (string->symbol a)
                 (recognize-fn b)
                 (string->number c)
                 (string->symbol d)
                 (recognize-fn e)
                 (string->number f))))

(define (interpret instructions [registers '()])
  (match instructions
    ['() registers]
    [(cons ins instructions-rest) (interpret instructions-rest (eval registers ins))]))

(define (reg-find idx registers)
  (if (dict-has-key? registers idx)
    (dict-ref registers idx)
    0
    ))

(define (reg-set idx val registers)
  (dict-set registers idx val))

(define (eval registers ins)
  (match-let ([(instruction man-reg man-fn man-num  cond-reg cond-fn cond-num) ins])
             (if (cond-fn (reg-find cond-reg registers) cond-num)
                 (reg-set man-reg (man-fn (reg-find man-reg registers) man-num) registers)
                 registers)))

(define (find-highest registers [highest -inf.0])
  (match registers
    ['() highest]
    [(cons (cons _ num) registers-rest) (find-highest registers-rest (max num highest))]))

(define (read-input [lst '()])
  (let ([line (read-line)])
    (if (eof-object? line)
        lst
        (read-input (append lst (list line))))))

(let ([result (interpret (map string->instruction (read-input)))])
  (find-highest result))