
(define-syntax test
  (syntax-rules ()
    [(_ e v)
     (let ([v1 e])
       (unless (equal? v1 v)
         (error 'test "test failed" 'e v v1)))]))


;; pseudo-random number generator for testing
(define random #f)

(define (init-random! seed)
  (set! random
        (lambda (n)
          (set! seed (remainder (+ 13849 (* 2053 seed)) 65536))
          (remainder seed n))))
