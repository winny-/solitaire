#lang racket

(require "util.rkt")

(provide (all-defined-out) (struct-out card))

(define suites '(diamonds hearts spades clubs))
(define ranks '(1 2 3 4 5 6 7 8 9 10 11 12 13))
(define rank-names '("ace" "2" "3" "4" "5" "6" "7" "8" "9" "10" "jack" "queen" "king"))
(define colors (hash 'red '(diamonds hearts)
                     'black '(spades clubs)))

(define/contract (suite? suite)
  (any/c . -> . boolean?)
  (true? (member suite suites)))

(define/contract (rank? rank)
  (any/c . -> . boolean?)
  (true? (member rank ranks)))

(define/contract (rank->name rank)
  (rank? . -> . non-empty-string?)
  (list-ref rank-names (index-of ranks rank)))

(struct card [suite rank]
  #:transparent
  #:guard
  (λ (suite rank name)
    (unless (suite? suite)
      (error name "Suite ~a not in ~a" suite suites))
    (unless (rank? rank)
      (error name "Rank ~a not in ~a" rank ranks))
    (values suite rank)))

(define/contract (card->string c)
  (card? . -> . non-empty-string?)
  (string-append
   (substring (symbol->string (card-suite c)) 0 1)
   (number->string (card-rank c))))

(define/contract (card-even? c)
  (card? . -> . boolean?)
  (even? (card-rank c)))

(define/contract (card-odd? c)
  (card? . -> . boolean?)
  (not (card-even? c)))

(define/contract (card-red? c)
  (card? . -> . boolean?)
  (true? (member (card-suite c) (hash-ref colors 'red))))

(define/contract (card-black? c)
  (card? . -> . boolean?)
  (true? (member (card-suite c) (hash-ref colors 'black))))

(define/contract (make-suite suite)
  (suite? . -> . (listof card?))
  (for/list ([rank ranks])
    (card suite rank)))

(define/contract (make-deck)
  (-> (listof card?))
  (foldl (λ (suite acc)
           (append acc (make-suite suite)))
         '()
         suites))

(module+ test
  (require rackunit)
  (define a (card 'diamonds 13))
  (define b (card 'hearts 2))
  (define c (card 'spades 3))
  (define d (card 'clubs 6))
  (test-case "card->string"
    (check-equal? (card->string a) "d13")
    (check-equal? (card->string b) "h2")
    (check-equal? (card->string c) "s3")
    (check-equal? (card->string d) "c6"))
  (test-case "card-even?/card-odd?"
    (check-false (card-even? a))
    (check-true (card-odd? a))
    (check-true (card-even? b))
    (check-false (card-odd? b)))
  (test-case "card-red?/card-black?"
    (check-true (card-red? a))
    (check-false (card-black? a))
    (check-true (card-red? b))
    (check-true (card-black? c))
    (check-true (card-black? d))))
