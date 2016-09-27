#lang racket
#|Calculadora|#
(println "Calculadora básica")
(println "Para sumar escriba : (suma # #)")
(println "Para restar escriba : (resta # #)")
(println "Para dividir escriba : (div # #)")
(println "Para multiplicación escriba : (mult # #)")
;(= resul (read))

(define (t) ;lo que hacemos con esta función es la figura con la que se ;llenara el triángulo
(display "*"))

(define (dibuje cant) ;esta funcion me dibuja los "*" pasando un espacio :entre ellos
   (do ((i 0 (+ i 1)))
       ((= i cant) )
     (t)
     (display " ")))


(define (triangulo cant) ;y esta es la que me dibuja el triangulo aquí defino ;que el tamaño sea de 10 *'s
   (do ((i 0 (+ i 1)));observa como se hace el ciclo :D
         ((= i cant) )
     (dibuje i);llamada a la funcion
     (newline)));pasa renglon

(define suma (lambda(a b) (+ a b)))
(define resta (lambda(a b) (- a b)))
(define div (lambda(a b) (/ a b)))
(define mult (lambda(a b) (* a b)))
