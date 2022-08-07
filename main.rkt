#lang racket/gui

(require "board.rkt"
         "draw.rkt")

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
        [callback (λ (mi ce) (set! b (make-board)) (send c refresh))])
   (new separator-menu-item% [parent ms])
   (new menu-item%
        [label "&Quit"]
        [parent ms]
        [callback (λ (mi ce) (exit))]))

  (define c (new canvas%
                 [parent f]
                 [paint-callback
                  (λ (canvas dc)
                    (send dc set-smoothing 'smoothed)
                    (send dc draw-bitmap (read-bitmap "/home/winston/pics/wp/cliff-animu-grill.jpg" #:backing-scale 3) 0 0)
                    (define-values (w h) (send canvas get-scaled-client-size))
                    (define scale (min (/ w board-width) (/ h board-height)))
                    (send dc set-scale scale scale)
                    (draw-board dc b 0 0))]))

  (send f show #t))

(module+ main
  (main))
