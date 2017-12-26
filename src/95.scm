;;; P95

(load "prelude.scm")


(define (full-words n)
  (if (< n 10)
      (list-ref '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "night") n)
      (string-append
       (full-words (quotient n 10))
       "-"
       (full-words (remainder n 10)))))

(test (full-words 3)
      "three")
(test (full-words 0)
      "zero")
(test (full-words 10)
      "one-zero")
(test (full-words 175)
      "one-seven-five")
