#lang racket
(define listaPrueba '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10))
(provide reverse)

; reverse a list using cons and aux function
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

(print listaPrueba)
(reverse listaPrueba)

(provide MatrixRemplace)
;change the value of a matrix given N M and the value to remplace
(define (MatrixRemplace matrix N M value)
  (MatrixRemplaceX '() matrix N M 0 value))

;Move to N colum of the matrix an remplace the N value
(define (MatrixRemplaceY left rigth N counter value)
  (cond
    (
     (>= counter N)
      (cond
        (
         (null? left)
          (cons value (cdr rigth)))
        (
         (null? (cdr rigth))
           (list value))
         (else
          (cons value (cdr rigth)))))
     (else
       (cons (car rigth)(MatrixRemplaceY (car rigth) (cdr rigth) N (+ counter 1) value))

       )))

;Move to M line of the matrix an remplace the M value
(define (MatrixRemplaceX up down N M counter value)
  (cond
    (
     (>= counter M)
      (cond
        (
         (null? up)(
          cons  (MatrixRemplaceY '() (car down) N 0 value)(cdr down))  )
        (else
           (cons  (MatrixRemplaceY '() (car down) N 0 value)(cdr down))  ))
      )
     (else
      (cons (car down)(MatrixRemplaceX (car down) (cdr down) N M (+ counter 1) value))

      )))

(define MatrixPrueba '((() () ()) (() () ()) (() () ()) (() () ()) (() () ())))
(print MatrixPrueba)

(MatrixRemplace MatrixPrueba 2 2 '())
(set! MatrixPrueba(MatrixRemplace MatrixPrueba 0 0 'x))
(MatrixRemplace MatrixPrueba 1 0 'x)
(MatrixRemplace MatrixPrueba 2 2 'x)