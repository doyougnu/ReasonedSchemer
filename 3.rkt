;; Module for chapter 3 of the reasoned schemer
#lang racket

(require "mk.rkt" "utils.rkt" "2.rkt")
(module+ test (require "mk.rkt" "2.rkt"))

(provide listo
         lolo
         twinso
         loto
         listofo
         eq-caro
         membero
         pmembero
         memberrevo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define listo
  (lambda (l)
    (conde
     [(nullo l) succeed]
     [(pairo l)
      (fresh (d)
             (cdro l d)
             (listo d))]
     [succeed fail]))) ;; else fail

(define lolo
;; list of lists - o
  (λ (l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (listo a))
      (fresh (d)
             (cdro l d)
             (lolo d))]
     [succeed fail])))

;; 31
;; (define twinso
;;   (λ (s)
;;     (fresh (x y)
;;            (conso x y s)
;;            (conso x '() y))))

(define twinso
  (λ (y)
    (fresh (x)
           (== `(,x ,x) y))))

;; (define loto
;;   (λ (l)
;;     (conde
;;      [(nullo l) succeed]
;;      [(fresh (a)
;;              (caro l a)
;;              (twinso a))
;;       (fresh (d)
;;              (cdro l d)
;;              (loto d))]
;;      [succeed fail])))
(define loto
  (λ (l)
    (listofo twinso l)))

(define listofo
  (λ (predo l)
    (conde
     [(nullo l) succeed]
     [(fresh (a)
             (caro l a)
             (predo a))
      (fresh (d)
             (cdro l d)
             (listofo predo d))]
     [succeed fail])))

(define eq-caro
  (λ (l x)
    (caro l x)))

(define membero
  (λ (x l)
    (conde
     ;; [(nullo l) fail]
     [(eq-caro l x) succeed]
     [succeed (fresh (d)
                     (cdro l d)
                     (membero x d))])))

(define identity
  (λ (l)
    (run* (y)
          (membero y l))))

(define pmembero
  (λ (x l)
    (conde
     [(eq-caro l x) (fresh (a d)
                           (cdro l `(,a . ,d)))]
     [(eq-caro l x) (cdro l '())]
     [succeed (fresh (d)
                     (cdro l d)
                     (pmembero x d))])))

(define first-value
  (λ (l)
    (run 1 (y)
         (membero y l))))

(define memberrevo
  (λ (x l)
    (conde
     [(nullo l ) fail]
     [succeed (fresh (d)
                     (cdro l d)
                     (memberrevo x d))]
     [succeed
      (eq-caro l x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(check 7 (run* (x) (listo `('a 'b ,x 'd))) '(_.0))
(check 10 (run 1 (x) (listo `('a 'b 'c . ,x))) '(()))
(explicate 14 (run 5 (x) (listo `('a 'b 'c . ,x))) "we are asking for 5 results")
(check 20 (run 1 (l) (lolo l)) '(()))
(check 32 (run* (q) (twinso `('tofu 'tofu)) (== #t q)) '(#t))
(check 32 (run* (q) (twinso `(,q 'tofu))) '('tofu))
(check 38 (run 1 (z) (loto `('('g 'g) . ,z))) '())
(explicate 42 (run 5 (z) (loto `(('g 'g) . ,z))) "we asked for 5 results from loto")
(explicate 47 (run 3 (out) (fresh (w x y z)
                                  (== `((g g) (e ,w) (,x ,y) . ,z) out)
                                  (loto out))) "we are putting it through its paces")
(explicate 49 (run 3 (out) (fresh (w x y z)
                                  (== `((g g) (e ,w) (,x ,y) . ,z) out)
                                  (listofo twinso out)))
           "we are testing out listofo")
(check 54 (run 1 (y) (membero y `('hummus 'with 'pita))) '('hummus))
(explicate 76 (run 5 (l) (membero 'tofu l)) "we building lists bby")
(explicate 80 (run 5 (l) (pmembero 'tofu l)) "we building lists smart bby")
(explicate 89 (run 12 (l) (pmembero 'tofu l)) "we building lists excessively bby")
