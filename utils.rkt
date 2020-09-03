#lang racket

(provide check)

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ id question answer)
     (with-syntax ([id (syntax->datum #'id)])
       #'(module+ test
           (let ([test-passed (equal? question answer)])
             (when test-passed
               (fprintf (current-output-port) "~a   ===>   ~s~n" id "Passed")
               ))))]
    [(_ id question answer message)
     (with-syntax ([id (syntax->datum #'id)])
       #'(module+ test
           (let ([test-passed (equal? question answer)])
             (when test-passed
               (fprintf (current-output-port) "~a   ===>   ~s with: ~s~n" id "Passed" message)))))]))
