#lang racket/base

(require racket/contract/base
         racket/contract/region)

(provide true?)

(define/contract (true? a)
  (any/c . -> . boolean?)
  (and a #t))
