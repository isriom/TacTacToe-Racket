#lang racket
(provide reverse)

;;Reverse
;;Parametros, lista a invertir
;;invierte una lista
(define (reverse lista)
  (reverseAux lista '()))

; aux function to save the value of the reverserd list
(define (reverseAux lista inversa)
  (cond
    ((null? (cdr lista))
     (cons
      (car lista) inversa))
    (else
     (reverseAux
      (cdr lista) (cons (car lista) inversa)))))

;;Reverse test
;(define listaPrueba '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10))
;(print listaPrueba)
;(reverse listaPrueba)

(provide matrixRemplace)

;;MAtrixRemplace 
;;Parametros, (Matrix sobre la que trabaja) (columna) (fila) (valor por el que remplazar)
;;remplaza el valor en la columna N y en la linea M
(define (matrixRemplace matrix N M value)
  (cond ((and (>= N 0)(>= M 0))(aplicateTo matrix N M (lambda(old) value)))
        (else #f)))

(provide matrixGet)

;;matrixGet 
;;Parametros, (Matrix sobre la que trabaja) (columna) (fila)
;;remplaza el valor en la columna N y en la linea M, retorna falso si se le da un numero de columna o linea invalido
(define (matrixGet matrix N M)
  (cond ((and (>= N 0)(>= M 0))(iter (iter matrix M) N))
        (else #f)))

;;Iter
;;Parametros, (matrix sobre la que trabaja) (posicion a la que llegar)
;;recorre una lista hasta llegar al N posicion y devuelve su valor, retorna falso si la lista o el numero a obtener es invalida
(define (iter matrix N)
  (cond ((equal? matrix #f) #f)
        ((null? matrix) #f)
        ((zero? N) (car matrix))
        (else (iter (cdr matrix) (- N 1)))))


(provide aplicateTo)
;;aplicateTo 
;;Parametros, (Matrix sobre la que trabaja) (columna) (fila) (funcion a ejecutar, debe considerar el formato del valor de la celda)
;;aplica la funcion sobre el valor en la columna N y en la linea M retorna falso si se le da un numero de columna o linea invalido
(define (aplicateTo matrix N M function)
  (cond ( (and (>= N 0)(>= M 0))(aplicateToX '() matrix N M 0 function))
        (else #f)))


;Move to N colum of the matrix an aplicate the function to the N value
(define (aplicateToY left rigth N counter function)
  (cond((and(< counter N )(null? rigth))(#f))
       ((>= counter N)
        (cond((null? left)(cons (function (car rigth)) (cdr rigth)))
             ((null? (cdr rigth))(list (function (car rigth))))
             (else (cons (function (car rigth)) (cdr rigth)))))
  (else (cons (car rigth)(aplicateToY (car rigth) (cdr rigth) N (+ counter 1) function))
  )))

;Move to M line of the matrix an aplicate the function to the N value of the M Line
(define (aplicateToX up down N M counter function)
  (cond ((and(< counter M )(null? down))(#f))
    (
     (>= counter M)
      (cond
        (
         (null? up)
          (cons  (aplicateToY '() (car down) N 0 function)(cdr down))  )
        (else
           (cons  (aplicateToY '() (car down) N 0 function)(cdr down))  )))
      
     (else
      (cons (car down)(aplicateToX (car down) (cdr down) N M (+ counter 1) function))
      )))
;;Test remplace y aplicateTo
(define MatrixPrueba '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)))
(print MatrixPrueba)

;(matrixRemplace MatrixPrueba 2 2 '())
(set! MatrixPrueba(matrixRemplace MatrixPrueba 0 0 'x))
;(matrixRemplace MatrixPrueba 1 0 'd)
;(matrixRemplace MatrixPrueba 2 2 'x)
;(aplicateTo MatrixPrueba 2 2 (lambda(x)(+ x 1)))

;;matrixGet test
(matrixGet MatrixPrueba 2 2)

(provide aplicateAll)

;;aplicateAll 
;;Parametros, (Matrix sobre la que trabaja)(funcion a aplicar)
;;aplica una funcion para cada celda de una matrix
;; la funcion debe tener la Fomra (lambda N M X) Donde N y M son los indices y X el valor de la casilla
(define (aplicateAll matrix function) (aplicateAllX matrix function))


;aplicate the function to all the elments of a line
(define (aplicateAllY line function M [N 0])
  (cond
    (
     (null?(cdr line))
           (function N M (car line)))
     (else
       (append (function N M (car line))(aplicateAllY (cdr line) function M (+ N 1)))
       )))

;aplicate the function to all the lines of a matrix
(define (aplicateAllX matrix function [M 0])
  (cond
    (
     (null?(cdr matrix))
           (append  (aplicateAllY (car matrix) function M)(cdr matrix)))
     (else
      (append (aplicateAllY (car matrix) function M)(aplicateAllX (cdr matrix) function (+ M 1)))
      )))

;;Test aplicateAll
;(define MatrixPrueba '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)))
;(print MatrixPrueba)

;(aplicateAll MatrixPrueba(lambda(x)(list)))


;;sume2vectors
;;Parametros, (vector origen)(vector destino)
;;obtiene el vector resultante de sumar los componentes de los dos vectores
(define (sume2vectors a b)(list (+ (car a)(car b))(+(cadr a)(cadr b))))


;;Test sume2vector
;(sume2vectors '( 1 1) '(-1 -1))

(provide splitList)
;;splitList
;;Parametros, (lista original)(length de la division)
;;retorna una lista que contiene las dos lista en las que se dividio
;;Divide una lista en dos
(define (splitList originalList divider)(splitListAux originalList divider '()))

(define (splitListAux originalList divider newList)
  (cond ((zero? divider)(list newList originalList))
        (else (splitListAux (cdr originalList) (- divider 1)(append newList (list (car originalList)))))))
;;Test splitList
;(splitList '( 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17) 11)