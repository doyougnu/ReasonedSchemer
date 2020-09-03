#lang racket

(require "mk.rkt" "utils.rkt")
(provide caro cdro nullo conso pairo)
;; (test-case "7"
;;   (check-match
;;    (run* (r)
;;          (caro `(a c o r n) 'a)
;;          (== #t r))
;;    '(#t)))



;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;
(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

;; (run* (r)
;;       (fresh (y x)
;;              (== `(,x ,y) r)))

;; (run* (r)
;;       (caro '(a c o r n) 'a)
;;       (== #t r))

(define nullo
  (lambda (x)
    (== '() x)))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define pairo
  (lambda (r)
    (fresh (x y)
     (== x (conso x y r)))))

;;;;;;;;;;;;;; checks ;;;;;;;;;;;;;;;;
(check 6 (run* (r) (caro `(a c o r n) r)) '(a))
(check 7 (run* (r) (caro `(a c o r n) 'a) (== #t r)) '(#t))
