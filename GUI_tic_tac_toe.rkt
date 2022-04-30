
#lang racket/gui

(require "list_manipulation.rkt")
(provide init)
(provide GameMatrix)

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Example"]))
(define MainColumn (new vertical-panel% [parent frame]
                                     [alignment '(center center)]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "You turn"]))

; Create a pointer to the buttons logic
(define GameMatrix (list))

; Make a button in the frame
(define (makebutton n m Ppanel)(new button% [parent Ppanel]
             [label (string-append (number->string n)"."(number->string m))]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label (string-append (number->string n)"."(number->string m)))(send button set-label "x")(send button enable #f)
                         (set! GameMatrix (cons (MatrixRemplace (car GameMatrix) (- n 1) (- m 1) 'x)(cdr GameMatrix)) )(print (car GameMatrix)))]))

; create a panel to aling buttons
(define (makeVPanel) (new horizontal-panel%  [parent MainColumn]
                                     [alignment '(center center)]))

; create a list that contains row, 
(define (CreateMatrix N M counter tmp matrix Bmatrix clean)
  (
   cond
    ((equal? clean #t)
     (CreateMatrix N M counter (list) (cons (caar tmp) matrix )  (cons Bmatrix (cdar tmp)  ) #f) )
    ((> counter M)
     (cons matrix Bmatrix))
    (else
     (CreateMatrix N M (+ counter 1) (cons (CreateRow N counter)tmp) matrix Bmatrix #t)))
    )

; create a list that represent a row, crate the button and add they to a panel
(define (CreateRow N M)(CreateRowAux N M 1 (makeVPanel) (list)(list)) )

; Aux function to save row and button row of the matrix
(define (CreateRowAux N M counter panel row Brow)
  (cond
    ((> counter N)
     (cons row Brow))
    (else
     (CreateRowAux N M (+ counter 1) panel (cons (list) row )(cons Brow (cons (makebutton counter M panel)(list counter M))   )))))


;init function create the button and matrix
(define (init N M)(set! GameMatrix (CreateMatrix N M 1 (list) (list)(list) #f)))

;test function
(define (foo matrix)(print matrix))
(init 3 5)
(send frame show #t)
(print GameMatrix)