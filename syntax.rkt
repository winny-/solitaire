#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "card.rkt")
         "card.rkt")

(provide (all-defined-out))

(define-for-syntax (suite+rank->identifier stx suite rank)
  (datum->syntax
   stx
   (string->symbol
    (card->string (card (syntax->datum suite)
                        (syntax->datum rank))))))

(define-syntax (define-card stx)
  (syntax-parse stx
    [(self suite:id rank:exact-positive-integer)
     #`(begin
         (define #,(suite+rank->identifier #'self #'suite #'rank) (card 'suite rank)))]))

(module+ test
  (require rackunit
           "card.rkt")
  (test-case "define-card"
    (define-card spades 1)
    (define-card hearts 13)
    (define-card diamonds 12)
    (check-equal? s1 (card 'spades 1))
    (check-equal? h13 (card 'hearts 13))
    (check-equal? d12 (card 'diamonds 12)))
  #;(test-case "define-cards"
    (define-cards (make-deck))
    (check-equal? c2 (card 'clubs 2))))
