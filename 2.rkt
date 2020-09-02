#lang racket

(require "mk.rkt")

                                        ; Define #s.
(current-readtable
 (make-readtable (current-readtable)
                 #\s
                 'dispatch-macro
                 (lambda (a b c d e f) succeed)))

                                        ; Define #u.
(current-readtable
 (make-readtable (current-readtable)
                 #\u
                 'dispatch-macro
                 (lambda (a b c d e f) fail)))

;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;
(define car0
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define cdr0
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

;; (run* (r)
;;       (fresh (y x)
;;              (== `(,x ,y) r)))

;; (run* (r)
;;       (car0 '(a c o r n) 'a)
;;       (== #t r))

(define null0
  (lambda (x)
    (== '() x)))

(define cons0
  (lambda (a d p)
    (== (cons a d) p)))

(define pair0
  (lambda (r)
    (fresh (x y)
     (== x (cons0 x y r)))))

(define list0
  (lambda (l)
    (conde
     [(null0 l) succeed]
     [(pair0 l)
      (fresh (d)
             (cdr0 l d)
             (list0 d))]
     [succeed fail])))

;; (run 5 (x)
;;      (list0 `('a 'b 'c . ,x)))

;; (run* (r)
;;       (list0 `('a 'b ,r 'c)))

(module+ test
  (run* (r)
        (list0 `('a 'b ,r 'c))))
