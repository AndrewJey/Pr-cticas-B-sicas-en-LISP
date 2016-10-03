#lang racket
(require racket/generator)

(define (safe? x y sln)
    (if (null? sln)
        #t
        (let ((px (caar sln)) (py (cadar sln)))
            (if (or (= y py) (= (- py y) (- px x)) (= (- py y) (- x px)))
                #f 
                (safe? x y (cdr sln))))))

; input-> n, output-> a generator that yields 
;            every possible list of n queens' coordinates 
(define (nqueen n)
    (let recur ((x (- n 1)))
        (generator ()
            (if (= x -1)
                (yield '())
                (for* ([sln (in-producer (recur (- x 1)) (void))]
                       [y   (range n)])
                    (and (safe? x y sln)
                         (yield (cons (list x y) sln)))))
        (void))))

(for ([e (in-producer (nqueen 6) (void))])
  (displayln e))