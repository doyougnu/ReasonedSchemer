#lang racket

(require minikanren)

(define succeed (== #t #t))
(define fail (== #t #f))
(define else succeed)

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


;; other stuff
(run* (q)
      #s
      (== 'corn q))

(run* (x)
 (let ((x #f))
   (== #t x)))

(run* (x) #s)

;; fresh a variable is gensym
(run* (x)
      (let ((x #f))
        (fresh (x)
               (== #t x))))

(run* (r)
      (fresh (x y)
             (== (cons x (cons y '())) r)))

(run* (r)
      (fresh (x)
             (let ((y x))
               (fresh (x)
                      (== (cons y (cons x (cons y '()))) r)))))

;; run1 produces at most one value
(run 1 (x)
      (conde
       ((== 'olive x) #s)
       ((== 'Oil x) #s)
       (else #u)))

(run* (r)
      (fresh (x y)
             (conde ((== 'split x) (== 'pea y))
                    ((== 'navy x) (== 'bean y)))
             (== (cons x (cons y '())) r)))

;; this is cool
(define teacup
  (lambda (x)
    (conde
     ((== 'tea x) #s)
     ((== 'cup x) #s)
     (else #u))))

(run* (x)
      (teacup x))

;; more answers from a single question, apparently they must all succeed
(run* (r)
      (fresh (x y)
             (conde ((teacup x) (== #t y) #s)
                    ((== #f x) (== #t y))
                    (else #u))
             (== (cons x (cons y '())) r)))
