#lang racket
#|Dibuja Calculadora|#
(displayln "Hola elige alguna función")
(display "Raiz, suma, cuadrado, potencia o resta")
(define(raiz X)(sqrt X))
(define(cuadrado X)(* X X))
(define(suma X Y)(+ X Y))
(define(potencia X Y)(expt X Y))
(define(resta X Y)(- X Y))