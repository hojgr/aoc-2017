#lang racket

(define (is-valid input)
  (let ([split-input (string-split input)])
    (is-valid-list split-input)))

(define (is-valid-list passphrases [uniq-passphrases '()])
  (match passphrases
    [(cons phrase rest-passphrases)
     (if (not (list-contains-anagram? phrase uniq-passphrases))
         (is-valid-list rest-passphrases (cons phrase uniq-passphrases))
         (not (not #f)))]
    [_ #t]))


(define (process-line [valid-count 0])
  (let ([line (read-line)])
    (if (eof-object? line)
        valid-count
        (if (is-valid line)
            (process-line (+ valid-count 1))
            (process-line valid-count)))))

(define (is-anagram a b)
  (is-anagram-list (string->list a) (string->list b)))

(define (is-anagram-list a b)
  (if (and (empty? b))
      (empty? a)
      (match a
        [(cons char tail) (if (eq? #f (member char b))
                              #f
                              (is-anagram-list tail (remq char b)))]
        [_ #f])))

(define (list-contains-anagram? word lst)
  (match lst
    [(cons lst-word lst-rest) (if (is-anagram word lst-word)
                                  #t
                                  (list-contains-anagram? word lst-rest))]
    [_ #f]))

(process-line)

;(is-anagram "xwosiah" "ihw")