(load "miniKanren-with-symbolic-constraints/mk.scm")

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

(define listo
  (lambda (l)
    (conde
     [(nullo l) succeed]
     [(pairo l)
      (fresh (d)
             (cdro l d)
             (listo d))]
     [succeed fail])))

(run* (r) (caro `('a 'c 'o 'r 'n) r))


(run* (r) (caro `('a 'c 'o 'r 'n) 'a) (== #t r))
