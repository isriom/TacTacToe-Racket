#lang racket/gui
(require "list_manipulation.rkt")
(provide playTurn)
;;playTurn 
;;Parametros, (Matrix sobre la que trabaja)(columna donde se jugo)(fila donde se jugo)(caracter de jugador)
;;ejecuta la logica para poner una ficha en el juego
(define (playTurn matrix N M char)
  (cond ((equal? (checkWin (car matrix) N M char)#t) (send (caddr matrix) set-label (string-append  char  " win")))
        (else (PlayTurnAux matrix N M char))
   ))

(define (PlayTurnAux matrix N M char) matrix) ;implementar algoritmo aqui y llamar sobre la matrix resultante  a checkWin

;;checkWin 
;;Parametros, (Matrix sobre la que trabaja)(columna donde se jugo)(fila donde se jugo)(caracter de jugador)
;;empezara a recorrer la matrix en las 8 posibles direcciones desde donde se jugar hasta llegar al limite o encontrar una ficha diferente a la jugada, se comprobara si gano con las funciones auxiliares
(define (checkWin matrix N M char)(checkExtremes matrix N M (list '(-1 -1) '(0 -1)'( 1 -1)'(-1 0)'(1 0)'(-1 1)'(0 1)'(1 1)) N M char (matrixGet matrix N M ) 0 '()))
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
  (cond ((null? listA) "Tie")
        ((>= (-(+(car listA)(car listB))1) 3)#t)
        (else (checkCenterAux (cdr listA)(cdr listB)))
  
  ))