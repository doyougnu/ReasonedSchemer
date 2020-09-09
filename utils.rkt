#lang racket

(require (for-syntax racket/format))
(provide check explicate)

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ id question answer)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (let* ([test-passed (equal? question answer)]
                  [result-message (if test-passed "Passed" "Failed")])
             (printf  "~a   ===>   ~s~n~n" id result-message))))]
    [(_ id question answer message)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (let* ([test-passed (equal? question answer)]
                  [result-message (if test-passed "Passed" "Failed")])
             (printf  "~a ===> ~s with message: ~s~n~n" id result-message message))))]))

(define-syntax (explicate stx)
  (syntax-case stx ()
    [(_ id question)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (printf  "~a becomes ~s~n~n" id question)))]
    [(_ id question message)
     (with-syntax ([id (~r (syntax->datum #'id) #:min-width 2 #:pad-string " ")])
       #'(module+ test
           (printf  "~a becomes ~s because ~s~n~n" id question message)))]))
