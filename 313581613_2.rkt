#lang pl 02


#| BNF for SE 

<SE> ::= {string <char> <SE_char} (1) **Only Chars -> string
        | {string-length <SE_str> } (2) **Only string -> Number
        | {string-append <string> <SE_str>}(3) **only strings ->string
        | {string-insert <SE_str> <char> <SE_num>} (4) string ^ char ^ number -> string 
        | {number->string <SE_num>)(5) **Natural Number -> string


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




#|
TODO ADD COMMENTS!!!
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
   
  (: parse : String -> PLANG) 
  ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str) 
    (let ([code (string->sexpr str)]) 
      (match code)
      [(list (rest(list 'poly x)))(not(null?(list )))) (error 'parse "bad parse at ~s" code)]
      [(list (not(null?(rest(list 'poly))))(null?(list ))) (error 'parse "bad parse at ~s" code)]
      [(list (not(null?(rest(list 'poly))))(not(null?(list )))) (poly )]
      





(test (parse "{{poly 1 2 3} {1 2 3}}")  
     => (Poly (list (Num 1) (Num 2) (Num 3))  
              (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }")  
     =error> "parse: at least one coefficient is                        required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }")       =error> "parse: at least one point is  
                       required in ((poly 1 2) ())") 

