#lang racket/base

(require "mk.rkt" "2.rkt" "3.rkt" "utils.rkt")
(provide ; mk
         run run*
         == =/=
         fresh eigen
         conde conda condu
         symbolo numbero ;; not-pairo
         absento
         project
         succeed
         fail
         else
         ; utils
         (all-from-out "utils.rkt")
         ; 2
         caro
         cdro
         nullo
         conso
         pairo
         ; 3
         listo
         lolo
         twinso
         loto
         listofo
         eq-caro
         membero
         pmembero
         memberrevo)
