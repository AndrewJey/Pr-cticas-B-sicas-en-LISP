#lang racket
#|Condicional 1|#


(define (respuesta r)
  (if (equal? "hola" (substring r 0 4))
      "hola, gusto de verte!" "perdón?"))