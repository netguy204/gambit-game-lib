(define-macro (when test . body)
  `(if ,test
       (begin . ,body)
       '()))

(define-macro (comment . body)
  ''())

