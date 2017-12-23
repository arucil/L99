;;; P89

(load "prelude.scm")
(load "86.scm")


(define (bipartite? g)
  (for-all
   (lambda (node)
     (or (= 2 (cadr node))
         (= 1 (cadr node))))
   (car (color g))))

(test (bipartite? '((a b c d) ((a b) (c d))))
      #t)
(test (bipartite? '((a b c d) ((a b) (a c) (b d) (c d))))
      #t)
(test (bipartite? '((a b c d) ((a b) (a d) (a c) (b d) (c d))))
      #f)
