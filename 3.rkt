;; Module for chapter 3 of the reasoned schemer
#lang racket

(require "mk.rkt" "utils.rkt" "2.rkt")

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
