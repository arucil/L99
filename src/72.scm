;;; P72

(load "prelude.scm")


(define (bottom-up t)
  (if (symbol? t)
      (list (list t))
      (append (list (list (car t)))
              (map
               (lambda (x)
                 (append x (list (car t))))
               (apply append (map bottom-up (cdr t)))))))

(test (bottom-up 'x)
      '((x)))
(test (bottom-up '(f g))
      '((f) (g f)))
(test (bottom-up '(x (f g)))
      '((x) (f x) (g f x)))
(test (bottom-up '(a (f g) c (b d e)))
      '((a) (f a) (g f a) (c a) (b a) (d b a) (e b a)))
