;; Module for chapter 6 of the reasoned schemer
#lang racket

(require "lib.rkt" "5.rkt")
(module+ test (require "lib.rkt"))

(provide anyo
         nevero
         salo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define anyo
  (lambda (g)
    (conde
     [g succeed]
     [succeed (anyo g)])))

(define nevero (anyo fail))
(define alwayso (anyo succeed))

(define salo ;; succeeds at least once
  (lambda (g)
    (conde
     [succeed succeed]
     [succeed g])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check 6 (run 1 (q)
              fail
              nevero)
       '())

(check 7 (run 1 (q)
              alwayso
              (== #t q))
       '(#t))

(check 10 (run 5 (q)
               alwayso
               (== #t q))
       '(#t #t #t #t #t))
