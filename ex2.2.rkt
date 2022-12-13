#lang pl
;;2

;;helper function to calculate square numbers
(: square : Number -> Number)
(define (square x) (* x x))
#|
the map make new list with square values,
and the foldl sum all the square numbers
|#
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares list)
  (foldl + 0 (map square list)))


(test (sum-of-squares '(1 2 4 3)) => 30)
(test (sum-of-squares '(2 2 2 2)) => 16)
(test (sum-of-squares '(0 0 0 0 )) => 0)
(test (sum-of-squares '(1 2 3 4 5 6)) => 91)
(test (sum-of-squares '(10 20 30 40)) => 3000)
(test (sum-of-squares '(-1 -1 1 -1)) => 4)
(test (sum-of-squares '(-10 -2 1 5)) => 130)
(test (sum-of-squares '(-1 -1 1 -1 1 -1 1 -1 1 -1)) => 10)
(test (sum-of-squares '(0.2 1/2 1 2)) => 5.29)