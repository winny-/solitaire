#lang racket

(require racket/draw
         "board.rkt"
         "card.rkt"
         "syntax.rkt")

(provide (all-defined-out))

(define card-height 70)
(define card-width 50)
(define card-horizontal-space (quotient card-width 3))
(define card-vertical-space (quotient card-height 2))
(define card-tableu-space (quotient card-height 4))

(define board-width (+ (* card-horizontal-space 8) (* card-width 7)))
(define board-height (+ (* card-height 2) (* card-vertical-space 3) (* card-tableu-space 7)))

(define/contract (column col)
  (exact-nonnegative-integer? . -> . real?)
  (* (+ card-width card-horizontal-space) col))

(define/contract (draw-placeholder dc x y)
  ((is-a?/c dc<%>) exact-nonnegative-integer? exact-nonnegative-integer? . -> . void?)
  (with-save* dc ([get-brush set-brush (make-color #xe #xe #xe .2) 'solid]
                  [get-pen set-pen "white" 1 'transparent])
    (send dc draw-rounded-rectangle x y card-width card-height -.10)))

(define/contract (draw-card dc the-card visible? x y)
  ((is-a?/c dc<%>) (or/c card? false?) boolean? exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (set! visible? (and visible? the-card))
  (with-save* dc ([get-brush set-brush (if visible? "white" "yellow") 'solid]
                  [get-pen set-pen "black" 1 'solid])
    (send dc draw-rounded-rectangle x y card-width  card-height -.10)
    (when visible?
      (match-define (card suite rank) the-card)
      (with-save* dc ([get-text-foreground set-text-foreground (if (card-red? the-card) "red" "black")]
                      [get-font set-font (make-font #:size 7 #:face "Droid Sans")])
        (define s (card->pretty-string the-card))
        (send dc draw-text s (+ x 2) y)
        (send dc draw-text s (+ x (- card-width 2)) (+ y card-height) #f 0 pi)))))

(define/contract (draw-tableu dc the-tableu x y)
  ((is-a?/c dc<%>) tableu? exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void)
  (if (empty-stack? the-tableu)
      (draw-placeholder dc x y)
      (match-let ([(tableu hidden visible) the-tableu])
        (for ([c hidden]
              [n (length hidden)])
          (draw-card dc c #f x (+ (* n card-tableu-space) y)))
        (for ([c visible]
              [n (in-range (length hidden) (+ (length hidden) (length visible)))])
          (draw-card dc c #t x (+ (* n card-tableu-space) y))))))

(define/contract (draw-reserve dc the-reserve x y)
  ((is-a?/c dc<%>) reserve? exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (match-define (reserve hidden visible) the-reserve)
  (if (empty? hidden)
      (draw-placeholder dc x y)
      (draw-card dc #f #f x y))
  (define vis-x (+ (column 1) x))
  (if (empty? visible)
      (draw-placeholder dc vis-x y)
      (draw-card dc (last visible) #t vis-x y)))

(define/contract (draw-foundation dc the-foundation x y)
  ((is-a?/c dc<%>) foundation? exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (if (empty-stack? the-foundation)
      (draw-placeholder dc x y)
      (draw-card dc (last (foundation-stack the-foundation)) #t x y)))

(define/contract (draw-foundations dc foundations x y)
  ((is-a?/c dc<%>) (listof foundation?) exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (for ([f foundations]
        [n (length foundations)])
    (draw-foundation dc f (+ (column n) x) y)))

(define/contract (draw-tableus dc tableus x y)
  ((is-a?/c dc<%>) (listof tableu?) exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (for ([t tableus]
        [n (length tableus)])
    (draw-tableu dc t (+ (column n) x) y)))

(define/contract (draw-board dc the-board x y)
  ((is-a?/c dc<%>) board? exact-nonnegative-integer? exact-nonnegative-integer?
   . -> . void?)
  (match-define (board piles foundations the-reserve) the-board)
  (define-values (spaced-x spaced-y) (values(+ x card-horizontal-space) (+ y card-vertical-space)))
  (draw-reserve dc the-reserve spaced-x spaced-y)
  (draw-foundations dc foundations (+ (column 3) spaced-x) spaced-y)
  (draw-tableus dc piles spaced-x (+ spaced-y card-height card-vertical-space)))
