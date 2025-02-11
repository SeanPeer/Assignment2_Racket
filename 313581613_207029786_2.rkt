#lang pl 02


#| BNF for SE 

<SE> ::= <string> | <char> | <num
        |  {string <char> <SE_char} (1)                      **Only Chars -> string
        | {string-length <SE_str> } (2)                     **Only string -> Number
        | {string-append <string> <SE_str>}(3)              **only strings ->string
        | {string-insert <SE_str> <char> <SE_num>} (4)      **string ^ char ^ number -> string 
        | {number->string <SE_num>)(5)                      **Natural Number -> string


<SE_char> ::= <char> (6)| {string <char> <SE_char> } (7)
<char> ::= #\0 | #\1 | #\2 | #\3 | #\4 | #\5 | #\6 | #\7 | #\8 | #\9 (8)

<SE_num> ::= <num> (9) | (string-length <string> } (10)

<SE_Str> ::= <string> (11)| {string-append <string> <SE_Str>} | {number->string <SE_num>} (12)
            |{string-insert <SE_str> <char> <SE_num>}(13)
1b.
First - Derivating "123" using string-append > number->string > string-length
SE =3> 
{string-append <string> <SE_str>} =12>
{string-append "12" {number->string <SE_num>}} =10>
{string-append "12" {number->string {string-length <SE_str> }}} =11>
{string-append "12" {number->string {string-length <string> }}} =>
{string-append "12" {number->string {string-length "123" }}} =>
"123"

Second - Derivation of "1234" using string-insert :
SE =4>
{string-insert <SE_str> <char> <SE_num>} =11,8,9>
{string-insert <string> '3' <num>} =>
{string-insert "12" #\3 4} =>
"1234"

Third - Derivation of "1234567" using string-append > string-insert :
SE=3>
{string-append <string> <SE_str>} =13>
{string-append "12" {string-insert <SE_str> <char> <SE_num>}} =11,8,9>
{string-append "12" {string-insert <string> #\5 <num>} =>
{string-append "12" {string-insert "34 #\5 67} =>
"1234567"

|#





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




#|
3A
create Polynomial is a function that returns a function .
poly is calctulating recursively the value of the polynom.
polyX send to poly the X to use the power and the list of k numbers we've got, in another way of saying
thist polyX start the action.
|#


(: createPolynomial : (Listof Number) -> (Number -> Number)) 
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number) 
  (define (poly argsL x power accum) 
     (if (null? argsL) accum
         (poly (rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))))) 
  (: polyX : Number -> Number) 
  (define (polyX x)
    (poly coeffs x 0 0))
    polyX)
    
  

(define p2345 (createPolynomial '(2 3 4 5))) 
(test (p2345 0) =>  (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3)))) 
(test (p2345 4) =>  (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3)))) 
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3)))) 
 
 
(define p536 (createPolynomial '(5 3 6))) 
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 
(expt 11 2)))) 
 
(define p_0 (createPolynomial '())) 
(test (p_0 4) => 0) 


#|

<PLANG> := {poly <AEs>} {<AEs>}}

<AEs> := <AE> <AEs> | <AE>

<AE> := <NUM> 
        |{+ <AE> <AE>}
        |{- <AE> <AE>}
        |{* <AE> <AE>}
        |{/ <AE> <AE>}


|#

(define-type PLANG 
    [Poly (Listof AE) (Listof AE)]) 
 
  (define-type AE 
    [Num  Number] 
    [Add  AE AE] 
    [Sub  AE AE] 
    [Mul  AE AE] 
    [Div  AE AE]) 
 
  (: parse-sexpr : Sexpr -> AE) 
  ;; to convert s-expressions into AEs 
  (define (parse-sexpr sexpr) 
    (match sexpr 
      [(number: n)    (Num n)] 
      [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))] 
      [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
      
 [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

#|
  3B
  The parse function is taking the given string and switching it to a s-expression with string->sexpr
  after that we are looking for a match with the given list of s-expressions first list should start with
  'poly and have at least one S-expression with it.
  second list should have also at least on S-expression.
  otherwise the given string is incorrect and we threw an error for it.
  |#
  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code
       ;;[(list lhs rhs) (Poly (parse-list(list lhs))(parse-list(list rhs)))]
       ;;[(list (first(list code))  (second code)) (Poly  (parse-list(list(first code)))
       ;;                                           (parse-list (list (list(second code)))))]
        [(list (list 'poly ) (list r ...)) (error 'parse "at least one coefficient is required in ~s" code)]
        [(list (list 'poly l ...) (list)) (error 'parse "at least one point is required in ~s" code)]
        [(list (list 'poly l ...) (list r ...)) (Poly (parse-list l)
                                                      (parse-list r))]
        [else (error 'parse "Another unknown error is occured in ~s" code)])))


#|
using parse-list to send each s-expression alone from the list to parse it to AE using map 
|#

(: parse-list : (Listof Sexpr) -> (Listof AE))
(define (parse-list listxpr)
  (map parse-sexpr listxpr))
      
      





(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly 1 2 3 4} {1 2 3 4}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3) (Num 4))  
              (list (Num 1) (Num 2) (Num 3) (Num 4)))) 
(test (parse "{{poly} {1 2} }")   =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2 } {} }")       =error> "parse: at least one point is required in ((poly 1 2) ())") 

(test (parse "{{poly {+ 1 2} 2 } {1 2 }}")  
     =>(Poly (list (Add (Num 1) (Num 2)) (Num 2))
               (list (Num 1) (Num 2))))
(test (parse "{{poly 1 2 3 4 } {1 2 3 5 }}")  
     => (Poly (list (Num 1) (Num 2) (Num 3) (Num 4))  
              (list (Num 1) (Num 2) (Num 3) (Num 5))))
(test (parse "{{poly 1 2 } {1 2 }}")  
     => (Poly (list (Num 1) (Num 2))  
              (list (Num 1) (Num 2)))) 

;; 3C
;; evaluates AE expressions to numbers
(: eval : AE -> Number)
  (define (eval expr) 
    (cases expr 
      [(Num n)  n] 
      [(Add l r) (+ (eval l) (eval r))] 
      [(Sub l r) (- (eval l) (eval r))] 
      [(Mul l r) (* (eval l) (eval r))] 
      [(Div l r) (/ (eval l) (eval r))]))


  (: eval-poly : PLANG -> (Listof Number) ) 
  (define (eval-poly p-expr) 
    (cases p-expr
      [(Poly f s)(map(createPolynomial(map eval f)) (map eval s))])) 
 
  (: run : String -> (Listof Number)) 
  ;; evaluate a FLANG program contained in a string 
  (define (run str) 
    (eval-poly (parse str)))


(test (run "{{poly 1 2 3} {1 2 3}}")  
=> '(6 17 34)) 
(test (run "{{poly 4 2 7} {1 4 9}}")  => '(13 124 589)) 
(test (run "{{poly 4/5 } {1/2 2/3 3}}")  
=> '(4/5 4/5 4/5)) 
(test (run "{{poly 2 3} {4}}")  => '(14)) 
(test (run "{{poly 1 1 0} {-1 3 3}}")  => '(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") 
=> '(14)) 
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 
{/ 27 9}}}") 
=> '(0 4 4))



(test (run "{{poly {- 9 {/ {+ 5 3} 4}} 1 {* 0 9}} {{- 4 5} 3 
{/ 27 9}}}") 
=> '(6 10 10)) 
(test (run "{{poly 3 2 5} {1 2 3}}")  
=> '(10 27 54))
(test (run "{{poly 2 1 3} {2 2 2}}")  
=> '(16 16 16))
(test (run "{{poly 1 1 1} {2}}")  
=> '(7))
(test (run "{{poly 20 20 3} {2 3}}")  
=> '(72 107))
(test (run "{{poly 1 2 3 4 } {2 3 4}}")  
=> '(49 142 313))
(test (run "{{poly a 2 5} {1 2 3}}")  
=error> "parse-sexpr: bad syntax in a")
(test (run "{{poly 5 2 5} {b 2 3}}")  
=error> "parse-sexpr: bad syntax in b")
(test (run "{{poly 5 2 5} {}}")  
=error> "parse: at least one point is required in ((poly 5 2 5) ())")
(test (run "{{poly } {22 33 44}}")  
=error> "parse: at least one coefficient is required in ((poly) (22 33 44))")
