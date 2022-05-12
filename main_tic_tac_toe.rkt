#lang racket/gui
(require "list_manipulation.rkt")
(provide playTurn)
;;;Funcion llamada por los Botones, funciona como si de un fachada se tratase
;;playTurn 
;;Parametros, (Matrix sobre la que trabaja)(columna donde se jugo)(fila donde se jugo)(caracter de jugador)
;;comprueba sí se gano el juego al poner la ficha, sino ejecute 
(define (playTurn matrix N M char)
  (cond ((equal? (checkWin? (car matrix) N M char)#t) (send (caddr matrix) set-label (string-append  char  " win")))
        (else (voraz matrix N M char))
   ))

(define (voraz matrix N M char)(jugarIA matrix (minmax(car matrix) (conjunto_de_candidatos (car matrix))))) ;implementar algoritmo aqui y llamar sobre la matrix resultante a checkWin

(define (minmax matrix candidatos [depth 1])(cond ((null? (cdr candidatos))  (minmaxAux (matrixRemplace matrix (caar candidatos) (cadar candidatos) "X") (list) depth (caar candidatos) (cadar candidatos)"X"))
                                                  ((zero? depth)(solucion matrix candidatos "O" (list) -1))
                                                  ((zero?(remainder depth 2))((seleccion (cons (minmaxAux (matrixRemplace matrix (caar candidatos) (cadar candidatos) "O") (list) depth (caar candidatos) (cadar candidatos) "O")(minmax matrix (cdr candidatos) depth)))))
                                                  (else (seleccion (append (list(minmaxAux (matrixRemplace matrix (caar candidatos) (cadar candidatos) "X") (list) depth (caar candidatos) (cadar candidatos)"X"))(list(minmax matrix (cdr candidatos) depth)))))))


(define (minmaxAux matrix candidatos  depth N M char)(cond ((checkWin? matrix N M char) (list N M 1))
                                                      (else( minmax matrix (conjunto_de_candidatos matrix) (- depth 1)))))

;;jugarIA
;;Parametros (conjunto de matrices de intecambio de informacion GUI-Main)(solucion)
;; ejecuta la solucion y devuelve la matrix modificada
(define (jugarIA matrix solucion)(send (caddr matrix) set-label "X win")(aplicateTo (cadr matrix) (car solucion)(cadr solucion) (lambda(x)(send x enable #f)(send x set-label "X")))(cons(matrixRemplace (car matrix) (car solucion) (cadr solucion) "X")(cdr matrix)))


;;;Algoritmo de comprobacion de fin de juego
;;checkWin 
;;Parametros, (Matrix sobre la que trabaja)(columna donde se jugo)(fila donde se jugo)(caracter de jugador)
;;empezara a recorrer la matrix en las 8 posibles direcciones desde donde se jugar hasta llegar al limite o encontrar una ficha diferente a la jugada,
;;se comprobara si gano con las funciones auxiliares
;;retorna #t sí gana el caracter y #f si no
(define (checkWin? matrix N M char)(checkExtremes matrix N M (list '(-1 -1) '(0 -1)'( 1 -1)'(-1 0)'(1 0)'(-1 1)'(0 1)'(1 1)) N M char (matrixGet matrix N M ) 0 '()))
;;comprueba si se gano jugando en el extremo de una solucion, si no llama a comprobar si se jugo desde el centro de una solucion
(define (checkExtremes matrix N M vectors X Y char check count walk)
  (cond ((null? vectors) (checkCenter (splitList walk 4)))
        ((>= count 3)  #t)
        ((equal? check #f)(checkExtremes matrix N M (cdr vectors) N M char (matrixGet matrix N M ) 0 (cons count walk)))
        ((equal? check char)(checkExtremes matrix N M vectors (+ X (caar vectors)) (+ Y (cadar vectors)) char (matrixGet matrix (+ X (caar vectors)) (+ Y (cadar vectors)) ) (+ count 1) walk))
        (else (checkExtremes matrix N M (cdr vectors) N M char (matrixGet matrix N M ) 0 (cons count walk)))
   ))
;;comprueba si se gano enmedio de una solucion comprobando los valores obtenidos de las dirreciones contrarias, salta a la primera solucion
(define (checkCenter walk)(checkCenterAux (car walk)(reverse (cadr walk))))
(define (checkCenterAux listA listB)
  (cond ((null? listA) #f)
        ((>= (-(+(car listA)(car listB))1) 3)#t)
        (else (checkCenterAux (cdr listA)(cdr listB)))
  
  ))


;;;Algortimo voraz


;;Funcion objetivo
;;Parametros(Matrix de juego) (columna donde se jugo)(fila donde se jugo)(caracter de jugador)(multiplicador de valor)
;;aplicara checkWin? sobre la matrix de juego
;;retornara un 1*multiplicador sí gano el caracter otorgado o un 0 sí no se llega a una solucion
(define (objetivo matrix N M char [multi 1])(cond ((checkWin? matrix N M char) (* 1 multi)) (else 0)))

;;Funcion viabilidad? 
;;Parametros(Matrix de juego)(columna a jugar)(fila a jugar)
;;Comprobara sí el valor de la matrix en una posicion es una lista vacia/0
;;retorna #t sí esta vacia y #f sí esta ocupada
(define (viabilidad? matrix [N 0] [M 0])(cond((equal? (matrixGet matrix N M) 0) #t)(else #f)))

;;Funcion conjunto_de_candidatos
;;Parametros (matrix de juego)
;;comprobara toda la matrix y y seleccionara los campos libres
;;retorna una lista con pares de indices de la matrix
(define (conjunto_de_candidatos matrix) (aplicateAll matrix (lambda (N M X)(cond((viabilidad? (list(list X)))(println matrix)(println (list N M X))(list(list N M)))
                                                                                 (else '())))))
;;Funcion seleccion
;;Parametros (matrix de juego) (candidatos (N M X))
;;comprobara cual candidato tiene mayor puntaje y lo seleccionara
;;retorna una lista con pares de indices de la matrix y el valor del candidato
(define (seleccion candidatos [mayor (list )])(cond((null? candidatos) mayor)
                                                 ((null? mayor)(seleccion (cdr candidatos)(car candidatos)))
                                                 ((> (caddar candidatos)0)(seleccion (cdr candidatos)(car candidatos)))
                                                 ((and (equal? (caddar candidatos) -1)(equal? (caddr mayor) 0) #t)(seleccion (cdr candidatos)(car candidatos)))
                                                 (else (seleccion  (cdr candidatos) mayor))))

;;Funcion solución
;;Parametros (matrix de juego)(conjunto de candidatos)(caracter de jugador)(conjunto de soluciones posibles)
;;buscara el mejor candidato para una matrix de juego y un conjunto de candidatos.
;;Para ello obtendra la solucion de cada candidato aplicando la funcion objetivo y agregando su valor al candidato
;; una vez halla creado todas las soluciones selecciona la mejor
;;retorna una lista con la columna, fila y valor de la solucion
(define (solucion matrix candidatos char [soluciones (list)] [multi 1])(cond ((null?  candidatos)(seleccion soluciones))
                                                          (else (solucion matrix (cdr candidatos) char (cons (append (car candidatos)(list(objetivo (matrixRemplace matrix (caar candidatos) (cadar candidatos) char)(caar candidatos)(cadar candidatos)char multi))) soluciones)multi))
                                                          ))


