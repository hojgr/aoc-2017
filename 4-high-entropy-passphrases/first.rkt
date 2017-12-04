#lang racket

(define (is-valid input)
  (let ([split-input (string-split input)])
    (is-valid-list split-input)))

(define (is-valid-list passphrases [uniq-passphrases '()])
  (match passphrases
    [(cons phrase rest-passphrases)
     (if (eq? (member phrase uniq-passphrases) #f)
         (is-valid-list rest-passphrases (cons phrase uniq-passphrases))
         #f)]
    [_ #t]))


(define (process-line [valid-count 0])
  (let ([line (read-line)])
    (if (eof-object? line)
        valid-count
        (if (is-valid line)
            (process-line (+ valid-count 1))
            (process-line valid-count)))))

(process-line)

