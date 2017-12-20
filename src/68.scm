;;; P68

(load "prelude.scm")


;; a

(define (preorder t)
  (if (null? t)
      '()
      (cons (car t)
            (append (preorder (cadr t))
                    (preorder (caddr t))))))

(test (preorder '())
      '())
(test (preorder '(x () ()))
      '(x))
(test (preorder '(a (b (d () ())
                       (e () ()))
                    (c ()
                       (f (g () ())
                          ()))))
      '(a b d e c f g))


(define (inorder t)
  (if (null? t)
      '()
      (append (inorder (cadr t))
              (list (car t))
              (inorder (caddr t)))))

(test (inorder '())
      '())
(test (inorder '(x () ()))
      '(x))
(test (inorder '(a (b (d () ())
                      (e () ()))
                   (c ()
                      (f (g () ())
                         ()))))
      '(d b e a c g f))


;; c

(define (partial-length a b)
  (let f ([a a] [len 0])
    (if (eq? a b)
        len
        (f (cdr a) (add1 len)))))

(define (pre-in-tree pre in)
  (let f ([pre pre] [in-beg in] [in-end '()])
    (if (eq? in-beg in-end)
        '()
        (let ([in-mid (memq (car pre) in-beg)])
          (list (car pre)
                (f (cdr pre) in-beg in-mid)
                (f (list-tail (cdr pre) (partial-length in-beg in-mid))
                   (cdr in-mid)
                   in-end))))))

(test (pre-in-tree '(a) '(a))
      '(a () ()))
(test (pre-in-tree '(a c b d) '(c a d b))
      '(a (c () ())
          (b (d () ())
             ())))
(test (pre-in-tree '(a b d e c f g) '(d b e a c g f))
      '(a (b (d () ())
             (e () ()))
          (c ()
             (f (g () ())
                ()))))
