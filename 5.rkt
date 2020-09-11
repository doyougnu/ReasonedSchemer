;; Module for chapter 5 of the reasoned schemer
#lang racket

(require "lib.rkt" "4.rkt")
(module+ test (require "lib.rkt"))

(provide appendo
         swappendo
         unwrap
         flatteno
         flattenrevo
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define appendo
  (λ (l s out)
    (conde
     [(nullo l) (== s out)]
     [succeed
      (fresh (a d res)
             (conso a d l)
             (conso a res out)
             (appendo d s res))])))

(define swappendo
  (λ (l s out)
    (conde
     [succeed
      (fresh (a d res)
             (conso a d l)
             (conso a res out)
             (swappendo d s res))]
     [succeed (nullo l) (== s out)])))

(define unwrap
  (λ (x)
    (cond
      [(pair? x) (unwrap (car x))]
      [else x])))

(define unwrapo
  (λ (x out)
    (conde
     [succeed (== x out)]
     [(pairo x) (fresh (a)
                       (caro x a)
                       (unwrapo a out))])))

(define flatteno ;; this definition doesn't seem to work
  (λ (s out)
    (conde
     [(nullo s) (== '() out)]
     [(pairo s)
      (fresh (a d res-a res-d)
             (conso a d s)
             (flatteno a res-a)
             (flatteno d res-d)
             (appendo res-a res-d out))]
     [succeed (conso s '() out)])))

(define flattenrevo
  (λ (s out)
    (conde
     [succeed (conso s '() out)]
     [(nullo s) (== '() out)]
     [succeed (fresh (a d res-a res-d)
                     (conso a d s)
                     (flattenrevo a res-a)
                     (flattenrevo d res-d)
                     (appendo res-a res-d out))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(explicate 16
           (run 5 (x)
                (fresh (y)
                       (appendo `(cake with ice . ,y)
                                `(d t)
                                x)))
           "testing appendo")

(explicate 46
           (run* (x)
                 (unwrapo `(((pizza))) x))
           "wrapping pizza")

(explicate 53
           (run 5 (x)
                (unwrapo `((,x)) 'pizza))
           "more pizza")

;; (check 60 (run 1 (x) (flatteno '('('a 'b) 'c) x)) '(a b c))
(explicate 68 (run* (x) (flatteno `((a b) c) x)) "??")
(explicate 75 (run* (x) (flattenrevo `((a b) c) x)) "??")
