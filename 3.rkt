;; Module for chapter 3 of the reasoned schemer
#lang racket

(require "mk.rkt" "utils.rkt" "2.rkt")
(module+ test (require "mk.rkt" "2.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listo
  (lambda (l)
    (conde
     [(nullo l) succeed]
     [(pairo l)
      (fresh (d)
             (cdro l d)
             (listo d))]
     [succeed fail])))


(check 7 (run* (x) (listo `('a 'b ,x 'd))) '(_.0))
(check 10 (run 1 (x) (listo `('a 'b 'c . ,x))) '(()))
(explicate 14 (run 5 (x) (listo `('a 'b 'c . ,x))) "we are asking for 5 results")
