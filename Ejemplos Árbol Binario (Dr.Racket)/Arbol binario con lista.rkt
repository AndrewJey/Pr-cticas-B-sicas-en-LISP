#lang racket

;;Creamos un procedimiento para pasar de arbol binario a lista
;definimos variable bt para el dato y
;los nodos izq der (nodos de los lados)
;metodo constructor
(define (make-bt dato izq der)
  ;declaramos una lista para que se ordenen en forma de lista
  ;en el panel (con ambos nodos)
  (list dato izq der))
;definimos el arbol vacio para mostrar en los campos el vacio
;se define la constante de los campos para el arbol vacio
(define vacio-bt '())

;Se definen los tres selectores
;Primer elemento de la lista
(define (dato-bt btree)
  (car btree))
;segundo elemento que devuelve el nodo izquierdo del arbol
(define (izq-bt btree)
  (car (cdr btree)))
;define el nodo derecho de la lista (tercer elemento de la lista)
(define (der-bt btree)
  (car (cdr (cdr btree))))
;metodo que nos devuelve el elemento vacio de la lista
;selector que nos dice si el arbol esta vacio
(define (vacio-bt? btree)
  (null? btree));aqui me consulta si lo que me enviar es una lista vacia

;Consulta si elos nodos son vacios
(define (hoja-bt? btree)
  ;y si es vacio el izq de la lista
  (and (vacio-bt (izq-bt btree))
       ; o el derecho de la lista
       (vacio-bt (der-bt btree))))


;;OPERADORES o metodos
(define (to-list-bt btree)
  (if (vacio-bt? btree)
      '()
  (append (to-list-bt (izq-bt btree))
  (list (dato-bt btree))
  (to-list-bt (der-bt btree)))))

;;Se realizan las condiciones para busrcar el orden del arbol
;(define(member-bt? x btree)
 ; (cond
  ;  ((vacio-bt? btree) #f)
   ; ((= x (dato-bt btree)) #t)
    ;((< x (dato-bt btree)) (member-bt? x (izq-bt btree)))
    ;((> x (dato-bt btree)) (member-bt? x(der-bt btree)))))