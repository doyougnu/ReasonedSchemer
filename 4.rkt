;; Module for chapter 4 of the reasoned schemer
#lang racket

(require "lib.rkt")
(module+ test (require "lib.rkt"))

(provide memo
         rembero)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define eq-car?
  (λ (l x)
    (eq? x (car l))))

(define mem
  (λ (x l)
    (cond
      [(null? l) #f]
      [(eq-car? l x) l]
      [else (mem x (cdr l))])))

(define memo
  (λ (x l out)
    (conde
     [(eq-caro l x) (== l out)]
     [succeed (fresh (d)
                     (cdro l d)
                     (memo x d out))])))

(define rember
  (λ (x l)
    (cond
      [(null? l) '()]
      [(eq-car? l x) (cdr l)]
      [else (cons (car l)
                  (rember x (cdr l)))])))

(define rembero
  (λ (x l out)
    (conde
     [(nullo l) (== '() out)]
     [(eq-caro l x) (cdro l out)]
     [succeed
      (fresh (a d res)
             (cdro l d)
             (rembero x d res)
             (caro l a)
             (conso a res out))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check 10
       (run 1 (out) (memo 'tofu `(a b tofu d tofu e) out))
       '((tofu d tofu e))
       "unit test for memo")
(explicate 31
           (run* (out)
                 (fresh (y z)
                        (rembero y `(a b ,y d ,z e) out)))
           "rembero")
