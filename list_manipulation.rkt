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

(provide MatrixRemplace)

;;MAtrixRemplace 
;;Parametros, (Matrix sobre la que trabaja) (columna) (fila) (valor por el que remplazar)
;;remplaza el valor en la columna N y en la linea M
(define (MatrixRemplace matrix N M value)
  (aplicateTo matrix N M (lambda(old) value)))


;;aplicateTo 
;;Parametros, (Matrix sobre la que trabaja) (columna) (fila) (funcion a ejecutar, debe considerar el formato del valor de la celda)
;;aplica la funcion sobre el valor en la columna N y en la linea M
(define (aplicateTo matrix N M function) (aplicateToX '() matrix N M 0 function))


;Move to N colum of the matrix an aplicate the function to the N value
(define (aplicateToY left rigth N counter function)
  (cond
    (
     (>= counter N)
      (cond
        (
         (null? left)
          (cons (function (car rigth)) (cdr rigth)))
        (
         (null? (cdr rigth))
           (list (function (car rigth))))
         (else
          (cons (function (car rigth)) (cdr rigth)))))
     (else
       (cons (car rigth)(aplicateToY (car rigth) (cdr rigth) N (+ counter 1) function))

       )))

;Move to M line of the matrix an aplicate the function to the N value of the M Line
(define (aplicateToX up down N M counter function)
  (cond
    (
     (>= counter M)
      (cond
        (
         (null? up)(
          cons  (aplicateToY '() (car down) N 0 function)(cdr down))  )
        (else
           (cons  (aplicateToY '() (car down) N 0 function)(cdr down))  ))
      )
     (else
      (cons (car down)(aplicateToX (car down) (cdr down) N M (+ counter 1) function))

      )))
;;Test remplace y aplicateTo
;(define MatrixPrueba '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)))
;(print MatrixPrueba)

;(MatrixRemplace MatrixPrueba 2 2 '())
;(set! MatrixPrueba(MatrixRemplace MatrixPrueba 0 0 'x))
;(MatrixRemplace MatrixPrueba 1 0 'd)
;(MatrixRemplace MatrixPrueba 2 2 'x)
;(aplicateTo MatrixPrueba 2 2 (lambda(x)(+ x 1)))

;;aplicateAll 
;;Parametros, (Matrix sobre la que trabaja)(funcion a aplicar)
;;aplica una funcion para cada celda de una matrix
(define (aplicateAll matrix function) (aplicateAllX matrix function))
;aplicate the function to all the elments of a line
(define (aplicateAllY line function)
  (cond
    (
     (null?(cdr line))
           (list (function (car line))))
     (else
       (cons (function(car line))(aplicateAllY (cdr line) function))
       )))

;aplicate the function to all the lines of a matrix
(define (aplicateAllX matrix function)
  (cond
    (
     (null?(cdr matrix))
           (cons  (aplicateAllY (car matrix) function)(cdr matrix)))
     (else
      (cons (aplicateAllY (car matrix) function)(aplicateAllX (cdr matrix) function))

      )))

;;Test aplicateAll
(define MatrixPrueba '((0 0 0) (0 0 0) (0 0 0) (0 0 0) (0 0 0)))
(print MatrixPrueba)

(aplicateAll MatrixPrueba(lambda(x)(list)))
