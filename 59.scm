(load "prelude.scm")

;;; P59

(define (hbal-trees h)
  (define (combine l* r*)
    (apply append
           (map
            (lambda (l)
              (map
               (lambda (r)
                 (list 'x l r))
               r*))
            l*)))
  (cond
   [(zero? h) '(())]
   [(= 1 h)
    (list (list 'x '() '()))]
   [else
    (let ([h-1 (hbal-trees (sub1 h))]
          [h-2 (hbal-trees (- h 2))])
      (append (combine h-1 h-1)
              (combine h-1 h-2)
              (combine h-2 h-1)))]))

(test (hbal-trees 0)
      '(()))
(test (hbal-trees 1)
      '((x () ())))
(test (hbal-trees 2)
      '((x (x () ()) (x () ()))
        (x (x () ()) ())
        (x () (x () ()))))
