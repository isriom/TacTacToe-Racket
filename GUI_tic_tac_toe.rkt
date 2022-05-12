
#lang racket/gui

(require "list_manipulation.rkt")
(require "main_tic_tac_toe.rkt")
(provide init)
(provide GameMatrix)
;;;Codigo de GUI basado en la GUI de la libreria por Matthew Flatt,Robert Bruce Findler and John Clements
;;https://docs.racket-lang.org/gui/windowing-overview.html
; Make a frame by instantiating the frame% class
(define frame (new frame% [label "TicTacToe"]))
; Make a panel to align the buttons
(define MainColumn (new vertical-panel% [parent frame]
                                     [alignment '(center center)]))

; Make a static text message in the frame
(define msg (new message% [parent frame]
                          [label "You turn"]))

; Create a pointer to the buttons logic
(define GameMatrix (list msg))

; Make a button in the frame
(define (makebutton n m Ppanel)(new button% [parent Ppanel]
             [label ""]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send button set-label "X")(send button enable #f)
                         (set! GameMatrix (cons (matrixRemplace (car GameMatrix) (- n 1) (- m 1) "X")(cdr GameMatrix)))
                         (set! GameMatrix (playTurn GameMatrix (- n 1) (- m 1) "X") )(println (car GameMatrix)))]))

; create a panel to aling buttons
(define (makeVPanel) (new horizontal-panel%  [parent MainColumn]
                                     [alignment '(center center)]))
;CreateMatrix
; create a list that contains M rows 
(define (CreateMatrix N M counter tmp matrix Bmatrix clean)
  (
   cond
    ((equal? clean #t)
     (CreateMatrix N M counter (list) (cons (caar tmp) matrix )  (append Bmatrix (list(cdar tmp))  ) #f) )
    ((> counter M)
     (cons matrix (list Bmatrix)))
    (else
     (CreateMatrix N M (+ counter 1) (cons (CreateRow N counter)tmp) matrix Bmatrix #t)))
    )

;CreateRow
; create a list that represent a row, crate the button and add they to a panel
; the list have N elements
(define (CreateRow N M)(CreateRowAux N M 1 (makeVPanel) (list)(list)) )
; Aux function to save row and button row of the matrix
(define (CreateRowAux N M counter panel row Brow)
  (cond
    ((> counter N)
     (cons row Brow))
    (else
     (CreateRowAux N M (+ counter 1) panel (cons 0 row )(append Brow (list(makebutton counter M panel))   )))))


;init function create the buttons and matrix to represented the game
(define (init N M)(set! GameMatrix (append (CreateMatrix N M 1 (list) (list)(list) #f) GameMatrix)))

;TTT
;encapsulation of init
(define (TTT N M)(init N M))

;test function
(define (foo matrix)(print matrix))
(TTT 5 5)
(send frame show #t)
(print GameMatrix)
;(list N M vectors X Y check count)