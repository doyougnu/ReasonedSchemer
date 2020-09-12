;; Module for chapter 6 of the reasoned schemer
#lang racket

(require "lib.rkt" "6.rkt")
(module+ test (require "lib.rkt"))

(provide
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bit-xoro
  (λ (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 1 x) (== 0 y) (== 1 r)]
     [(== 0 x) (== 1 y) (== 1 r)]
     [(== 1 x) (== 1 y) (== 0 r)]
     [succeed fail])))

(define bit-ando
  (λ (x y r)
    (conde
     [(== 0 x) (== 0 y) (== 0 r)]
     [(== 1 x) (== 0 y) (== 0 r)]
     [(== 0 x) (== 1 y) (== 0 r)]
     [(== 1 x) (== 1 y) (== 1 r)]
     [succeed fail])))

(define half-addero
  (λ (x y r c)
    (conde
     [(== 0 x) (== 0 y) (== 0 r) (== 0 c)]
     [(== 1 x) (== 0 y) (== 1 r) (== 0 c)]
     [(== 0 x) (== 1 y) (== 1 r) (== 0 c)]
     [(== 1 x) (== 1 y) (== 0 r) (== 1 c)]
     [succeed fail])))

(define full-addero
  (λ (b x y r c)
    (fresh (w xy wz)
           (half-addero x y w xy)
           (half-addero w b r wz)
           (bit-xoro xy wz c))))

(define build-num
  (λ (n)
    (cond
      [(odd? n)
       (cons 1 (build-num (/ (- n 1) 2)))]
      [(and (not (zero? n)) (even? n))
       (cons 0 (build-num (/ n 2)))]
      [(zero? n) '()])))

(define poso
  (λ (n)
    (fresh (a d)
           (== `(,a . ,d) n))))

(define >1o
  (λ (n)
    (fresh (a ad dd)
           (== `(,a ,ad . ,dd) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(explicate 6 (run* (s) (fresh (x y)
                              (bit-xoro x y 0)
                              (== `(,x ,y) s))))

(explicate 9 (run* (s) (fresh (x y r )
                              (bit-xoro x y r)
                              (== `(,x ,y ,r) s))))
(check 11 (run* (s)
                (fresh (x y)
                       (bit-ando x y 1)
                       (== `(,x ,y) s)))
       '((1 1)))
(explicate 13 (run* (s)
                    (fresh (x y r c)
                           (half-addero x y r c)
                           (== `(,x ,y ,r ,c) s))))

(explicate 15 (run* (s)
                    (fresh (r c)
                           (full-addero 0 1 1 r c)
                           (== `(,r ,c) s))))

(check 80 (run* (q)
                (poso '(0 1 1))
                (== #t q))
       '(#t))

(check 87 (run* (q)
                (>1o '(0 1))
                (== #t q))
       '(#t))
