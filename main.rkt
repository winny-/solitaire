#lang racket/gui

(require "board.rkt"
         "draw.rkt"
         "syntax.rkt")

(define (main)
  (define b (make-board))

  (define f (new frame% [label "Klondike Solitaire"]))
  (send f create-status-line)
  (define m (new menu-bar% [parent f]))
  (define ms (new menu%
                  [label "&Solitaire"]
                  [parent m]))
  (void
   (new menu-item%
        [label "&New Game"]
        [parent ms]
        [callback (λ (mi ce)
                    (set! b (make-board))
                    (send c refresh))])
   (new separator-menu-item% [parent ms])
   (new menu-item%
        [label "&Quit"]
        [parent ms]
        [callback (λ (mi ce)
                    (exit))]))

  (define my-canvas%
    (class canvas%
      (super-new)
      (define bitmap (read-bitmap "/home/winston/pics/wp/cliff-animu-grill.jpg" #:backing-scale 3))
      (define/override (on-paint)
        (define dc (send this get-dc))
        (send dc set-smoothing 'smoothed)
        (send dc draw-bitmap bitmap 0 0)
        (define-values (w h) (send this get-scaled-client-size))
        (define scale (min (/ w board-width)
                           (/ h board-height)))
        (send dc set-scale scale scale)
        (draw-board dc b 0 0))
      (define/override (on-event evt)
        (match evt
          [(obj mouse-event%
                ([get-event-type 'left-down]
                 [get-x x] [get-y y]))
           (printf "M:Click ~a,~a\n" x y)]
          [(obj mouse-event%
                ([get-event-type 'left-up]
                 [get-x x] [get-y y]))
           (printf "M:Release ~a,~a\n" x y)]
          [_ (void)]))))

  (define c (new my-canvas%
                 [parent f]))

  (send f show #t))

(module+ main
  (main))
