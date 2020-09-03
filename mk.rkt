#lang racket

(require racket/trace)

(provide run run*
         == =/=
         fresh eigen
         conde conda condu
         symbolo numbero ;; not-pairo
         absento
         project
         succeed
         fail
         else)

;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;
;; extra stuff for racket
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



;; due mostly to samth
(define (list-sort f l) (sort l f))
(define (remp f l) (filter-not f l))
(define (call-with-string-output-port f)
  (define p (open-output-string))
  (f p)
  (get-output-string p))
(define (exists f l) (ormap f l))
(define for-all andmap)
(define (find f l)
  (cond [(memf f l) => car] [else #f]))
(define memp memf)
(define (var*? v) (var? (car v)))

;; actual code

(include "mk.scm")

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
