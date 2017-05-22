#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "card.rkt")
         racket/function
         rackunit
         "card.rkt")

(provide (all-defined-out))

(define-for-syntax (suite+rank->identifier ctx suite rank)
  (datum->syntax
   ctx
   (string->symbol
    (card->string (card (syntax->datum suite)
                        (syntax->datum rank))))))

(define-syntax (define-card stx)
  (syntax-parse stx
    [(self suite:id rank:nat)
     #`(begin
         (define #,(suite+rank->identifier #'self #'suite #'rank) (card 'suite rank)))]))

(define-syntax (define-deck stx)
  (syntax-parse stx
    [(self)
     #`(begin
         #,@(map (λ (c)
                   (with-syntax ([s (datum->syntax #'self (card-suite c))]
                                 [r (datum->syntax #'self (card-rank c))])
                     #`(define #,(suite+rank->identifier #'self #'s #'r) (card 's r))))
                 (make-deck)))]))

(define-syntax-rule (check-values-equal? expr (expected ...))
  (let ([res (call-with-values (thunk expr) list)])
    (check-equal? res (list expected ...))))

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
  (test-case "define-deck"
    (define-deck)
    (check-equal? c1 (card 'clubs 1))
    (check-equal? c13 (card 'clubs 13))
    (check-equal? h1 (card 'hearts 1))
    (check-equal? h13 (card 'hearts 13)))
  (check-values-equal? (values 'a 'b 'c) ('a 'b 'c)))
