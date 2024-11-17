#lang racket
(require racket threading)


(define wires
  (~> "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i"
      (string-split "\n")))
      ;; (map (curryr string-split " -> ") _)))

#|

idea

sort lines
parse each segment
add operations

|#

(define-syntax wire
  (λ (stx)
    (syntax-case stx ()
      [(arg -> id) #'(define id arg)]
      [(op arg -> id) #'(wire (op arg) -> id)]
      [(arg1 op arg2 -> id) #'(wire (op arg1 arg2) -> id)])))

(~> (first wires)
    (string-split " ")
    (map syntax->datum _))



(for/list ([wire wires])
  )
