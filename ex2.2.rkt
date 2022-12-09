#lang pl
;;2
(: square : Number -> Number)
(define (square x) (* x x))

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares list)
  (foldl + 0 (map square list)))


(test (sum-of-squares '(1 2 4 3)) => 30)
(test (sum-of-squares '(2 2 2 2)) => 16)
(test (sum-of-squares '(0 0 0 0 )) => 0)