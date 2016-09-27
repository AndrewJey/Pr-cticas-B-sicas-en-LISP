;De esta forma se comentarean una lineas

#|De esta forma se
docuemntas varias
lineas del código|#
#lang racket
; ---FACTORIAL----
(define (factorial n)

  (define (i_factorial n dcc)

    (if (= 0 n)

      dcc

      (i_factorial (- n 1) (* n dcc))))

  (i_factorial n 1))

(map (lambda (n)(printf "~a~n" (factorial n))) (list 1 12 123 1234 12345))