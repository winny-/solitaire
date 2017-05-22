#lang racket

#|
Some notes:
lists of cards are ascending from the bottom.

For example:

You have a foundation with s1 s2 s3. It is represented as (foundation (list s1 s2 s3) 'spades)
|#

(require "card.rkt")

(module+ test
  (require rackunit
           "syntax.rkt")
  (define-deck))

(struct tableu [hidden visible]
  #:guard (λ (hidden visible name)
            (unless ((listof card?) hidden)
              (error name "hidden not a listof card ~v" hidden))
            (unless ((listof card?) visible)
              (error name "visible not a listof card ~v" visible))
            (values hidden visible))
  #:transparent)
(struct reserve [deck visible]
  #:guard (λ (deck visible name)
            (unless ((listof card?) deck)
              (error name "deck not a listof card ~v" deck))
            (unless ((listof card?) visible)
              (error name "visible not a listof card ~v" visible))
            (values deck visible))
  #:transparent)
(struct foundation [stack suite]
  #:guard (λ (stack suite name)
            (unless ((listof card?) stack)
              (error name "stack not listof card ~v" stack))
            (unless ((or/c false? suite?) suite)
              (error name "suite not #f or a suite ~v" suite))
            (when (and suite (ormap (λ (c) (not (equal? (card-suite c) suite))) stack))
              (error name "stack has cards other than its given suite ~v ~v" stack suite))
            (values stack suite))
  #:transparent)
(struct board [piles foundations reserve]
  #:guard (λ (piles foundations reserve name)
            (unless ((list/c tableu? tableu? tableu? tableu? tableu? tableu? tableu?) piles)
              (error name "piles not of 7 tableu ~v" piles))
            (unless ((list/c foundation? foundation? foundation? foundation?) foundations)
              (error name "foundations not of 4 foundation ~v" foundations))
            (unless (reserve? reserve)
              (error name "reserve not a reserve ~v" reserve))
            (values piles foundations reserve))
  #:transparent)

(define stack? (or/c tableu? reserve? foundation?))

(define/contract (get-movable stack)
  (stack? . -> . (listof card?))
  (match stack
      [(tableu _ v) v]
      [(reserve _ v) v]
      [(foundation v _) v]))

(define/contract (make-board)
  (-> board?)
  (define deck (shuffle (make-deck)))
  (define piles
    (for/list ([i (in-range 1 8)])
      (define-values (taken rst) (split-at deck i))
      (set! deck rst)
      (tableu (drop taken 1) (take taken 1))))
  (define r (reserve (drop deck 1) (take deck 1)))
  (board piles (make-list 4 (foundation '() #f)) r))

(define/contract (can-move? stack stack-count)
  (stack? exact-positive-integer? . -> . boolean?)
  (define actual-stack (get-movable stack))
  (and
   (not (empty? actual-stack))
   (>= (length actual-stack) stack-count)
   (implies (or (reserve? stack) (foundation? stack))
            (= stack-count 1))
   (implies (tableu? stack)
            (let* ([cards (take actual-stack stack-count)]
                   [fst (car cards)]
                   [rst (cdr cards)]
                   [lst (last cards)])
              ;; Neither of these conditions should ever happen in an actual game, but it
              ;; does not hurt to check for that variant.
              (and
               (foldl (λ (c acc)
                        (xor (card-black? c) acc))
                      (card-black? fst)
                      rst)
               (andmap (λ (c n)
                         (= (card-rank c) n))
                       (reverse cards)
                       (range (card-rank lst) (+ stack-count (card-rank lst)))))))))

(define/contract (can-place? source source-count destination)
  (stack? exact-positive-integer? stack? . -> . boolean?)
  (define dest-ls (get-movable destination))
  (define dest-card (and (not (empty? dest-ls)) (last dest-ls)))
  (define src-ls (get-movable source))
  (and
   ; movable? ensures there is actually cards in the src-ls.
   (can-move? source source-count)
   (or ; Sum of products.
    (and
     (foundation? destination)
     (let ([suite (foundation-suite destination)]
           [c-src (last src-ls)])
       (if suite
           (and (equal? suite (card-suite c-src)) (= (add1 (card-rank dest-card)) (card-rank c-src)))
           (= (card-rank c-src) 1))))
    ; otherwise it's a tableu destination
    (and
     (tableu? destination)
     (let* ([cards (take-right src-ls source-count)]
            [top (car cards)])
       (and
        (xor (card-black? top) (card-black? dest-card))
        (= (add1 (card-rank top)) (card-rank dest-card))))))))

(module+ test
  (test-case "can-move? no cards"
    (check-false (can-move? (tableu '() '()) 1)) ; No cards in tableu
    (check-false (can-move? (foundation '() #f) 1)) ; No cards in foundation
    (check-false (can-move? (reserve '() '()) 1)) ; No cards in reserve

    (check-false (can-move? (tableu (list c2) '()) 1)) ; No visible cards in tableu
    (check-false (can-move? (reserve (list h5) '()) 1))) ; No visible cards in reserve

  (test-case "can-move? not enough cards"
    (check-false (can-move? (tableu '() (list c2)) 2)) ; Not enough visible cards in tableu
    (check-false (can-move? (foundation (list d1) 'diamonds) 2)) ; Not enough cards in foundation - doesn't matter anyway.
    (check-false (can-move? (reserve '() (list d1)) 2)) ; Not enough cards in reserve - doesn't matter anyway.

    (check-false (can-move? (foundation (list d1 d2) 'diamonds) 2)) ; Cannot move more than one card at a time from foundation.
    (check-false (can-move? (reserve '() (list c2 h5)) 2))) ; Cannot move more than one card at a time from reserve.

  (test-case "can-move? valid"
    (check-true (can-move? (tableu '() (list c2)) 1))
    (check-true (can-move? (foundation (list d1) 'diamonds) 1))
    (check-true (can-move? (reserve '() (list h5)) 1))
    (check-true (can-move? (tableu '() (list c2 d1)) 2))
    (check-true (can-move? (tableu '() (list c2 s1)) 1)))

  (test-case "can-move? improper tableu"
    (check-false (can-move? (tableu '() (list s1 c2)) 2))
    (check-false (can-move? (tableu '() (list d1 c2)) 2)))

  (test-case "can-place?"
    (check-true (can-place? (tableu '() (list s1)) 1 (tableu '() (list d2))))
    (check-true (can-place? (tableu '() (list s1)) 1 (foundation '() #f)))
    (check-true (can-place? (tableu '() (list s2)) 1 (foundation (list s1) 'spades)))))

(define/contract (take-from-tableu the-tableu n)
  (tableu? exact-positive-integer? . -> . (values tableu? (listof card?)))
  (match-define (tableu hidden visible) the-tableu)
  (define-values (left-over moved) (split-at-right visible n))
  (values (if (or (not (empty? left-over)) (empty? hidden))
              (struct-copy tableu the-tableu [visible left-over])
              (let-values ([(new-hidden new-visible) (split-at-right hidden 1)])
                (struct-copy tableu the-tableu [visible new-visible] [hidden new-hidden])))
          moved))

(module+ test
  (test-case "take-from-tableu"
    (check-values-equal? (take-from-tableu (tableu '() (list s1)) 1)
                         ((tableu '() '()) (list s1)))
    (check-values-equal? (take-from-tableu (tableu (list h9) (list d12)) 1)
                         ((tableu '() (list h9)) (list d12)))
    (check-values-equal? (take-from-tableu (tableu (list s3 d1 h3) (list d2 d9)) 2)
                         ((tableu (list s3 d1) (list h3)) (list d2 d9)))
    (check-values-equal? (take-from-tableu (tableu (list s5) (list c3 h2 s1)) 2)
                         ((tableu (list s5) (list c3)) (list h2 s1)))))

(define/contract (take-from-foundation the-foundation n)
  (foundation? exact-positive-integer? . -> . (values foundation? (listof card?)))
  (unless (= n 1)
    (error 'take-from-foundation "Can only move one card at a time from foundation"))
  (match-define (foundation cards suite) the-foundation)
  (define-values (left-over moved) (split-at-right cards 1))
  (values (struct-copy foundation the-foundation
                       [stack left-over]
                       [suite (and (not (empty? left-over)) suite)])
          moved))

(module+ test
  (test-case "take-from-foundation"
    (check-values-equal? (take-from-foundation (foundation (list s3 s2 s1) 'spades) 1)
                         ((foundation (list s3 s2) 'spades) (list s1)))
    (check-values-equal? (take-from-foundation (foundation (list h1) 'hearts) 1)
                         ((foundation '() #f) (list h1)))
    (check-exn exn:fail? (thunk (take-from-foundation (foundation (list d2 d1) 'diamonds) 2)))))

(define/contract (take-from-reserve the-reserve n)
  (reserve? exact-positive-integer? . -> . (values reserve? (listof card?)))
  (unless (= n 1)
    (error 'take-from-reserve "Can only move one card at a time from reserve"))
  (match-define (reserve deck visible) the-reserve)
  (define-values (left-over moved) (split-at-right visible 1))
  (values (struct-copy reserve the-reserve
                       [visible left-over])
          moved))

(module+ test
  (test-case "take-from-reserve"
    (check-values-equal? (take-from-reserve (reserve '() (list s5)) 1)
                         ((reserve '() '()) (list s5)))
    (check-values-equal? (take-from-reserve (reserve (list h12 h5) (list s1 s5)) 1)
                         ((reserve (list h12 h5) (list s1)) (list s5)))
    (check-exn exn:fail? (thunk (take-from-reserve (reserve (list s1) (list c5)) 2)))))
