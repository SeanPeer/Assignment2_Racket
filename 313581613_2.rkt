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

