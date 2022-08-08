#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     "card.rkt")
         syntax/parse/define
         racket/class
         racket/function
         racket/match
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

;; Borrowed shamelessly from
;; https://github.com/deeglaze/slideshow-helpers/blob/0074e661f1112d74e45afa196beb2bb1d748133a/picts.rkt#L232-L258
;; brush/pen not parameters, unfortunately.
;; Imperative save-restore to the "rescue."
(begin-for-syntax
  (define-syntax-class (gsr dc-stx)
    #:attributes (g s do)
    (pattern [g:id s:id (~optional (~seq (~or (~and #:unless (~bind [guarder #'unless]))
                                              (~and #:when (~bind [guarder #'when])))
                                         guard:expr))
                   r:expr ...]
             #:with do (cond
                         [(attribute guard) #`(guarder guard (send #,dc-stx s r ...))]
                         [else #`(send #,dc-stx s r ...)]))))

(define-syntax-parse-rule (with-save dc (~var p (gsr #'dc)) body ...)
  (let* ([dcv dc]
         [v (send dcv p.g)])
    p.do
    body ...
    (send dcv p.s v)))

(define-syntax (with-save* stx)
  (syntax-parse stx
    [(_ dc () body ...) (syntax/loc stx (let () body ...))]
    [(_ dc (~and (give gives ...)
                 ((~var p (gsr #'dcv)) (~var ps (gsr #'dcv)) ...))
        body ...)
     (syntax/loc stx (let ([dcv dc])
                       (with-save dcv give
                         (with-save* dcv (gives ...) body ...))))]))

(define-match-expander obj
  (λ (stx)
    (syntax-parse stx
      [(_ cls%:expr)
       #'(obj cls% _)]
      [(_ cls%:expr binding:id)
       #'(? (λ (thing) (is-a? thing cls%)) binding)]
      [(_ cls%:expr binding:id ([method:id value:expr] ...))
       #'(and (obj cls% binding)
              (app (λ (o) (send o method)) value) ...)]
      [(_ cls%:expr ([method:id value:expr] ...))
       #'(obj cls% _ ([method value] ...))])))
