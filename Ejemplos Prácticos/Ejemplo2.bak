;De esta forma se comentarean una lineas

#|De esta forma se
docuemntas varias
lineas del código|#
#lang racket
;Definición de numeros
(define numero 42)
;Definición de caracteres o textos
(define texto "Hola Mundo")
;media : a,b = (a+b)/2
;Esto se representa como (media 3 5) esto es = 4)
(define (media a b)
  (/ (+ a b) 2))
;Otra forma de definir es
(define (sumar a b c)
  (+ a b c))

; ---FACTORIAL----
(define (factorial n)

  (define (i_factorial n acc)

    (if (= 0 n)

      acc

      (i_factorial (- n 1) (* n acc))))

  (i_factorial n 1))

(map (lambda (n)(printf "~a~n" (factorial n))) (list 1 12 123 1234 12345))