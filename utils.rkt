#lang racket

(require (for-syntax racket/format))
(provide check)

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ id question answer)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (let* ([test-passed (equal? question answer)]
                  [result-message (if test-passed "Passed" "Failed")])
             (fprintf (current-output-port) "~a   ===>   ~s~n" id result-message))))]
    [(_ id question answer message)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (let* ([test-passed (equal? question answer)]
                  [result-message (if test-passed "Passed" "Failed")])
             (fprintf (current-output-port) "~a ===> ~s with: ~s~n" id message))))]))
