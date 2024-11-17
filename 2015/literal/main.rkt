#lang racket
(require racket syntax/strip-context (for-syntax racket/base))

(module+ reader
  (provide read-syntax read))

(define (read-syntax path port)
  (define datums
    (for/list ([str (in-lines port)]
               #:when (not (zero? (string-length str))))
      (format-datum str)))
  (strip-context
   #`(module anything literal/main
       #,@datums)))

(provide #%module-begin #%datum #%app #%top #%top-interaction wire format-datum)

(define-syntax (wire stx)
  (syntax-case stx ()
    [(wire arg -> var) #'(define/display var arg)]
    [(wire op arg -> var) #'(wire (op arg) -> var)]
    [(wire var1 op var2 -> res-var) #'(wire (op var1 var2) -> res-var)]))

(define-syntax-rule (define/display var arg)
  (begin
    (define var arg)
    (displayln (format "~a: ~a" 'var var))))

(define (format-datum line)
  (match (string-split line)
    [(list num -> var) `(wire ,(string->number num) -> ,(string->symbol var))]
    [(list op var1 -> var2) `(wire (,(string->symbol op) ,(string->symbol var1)) -> ,(string->symbol var2))]
    [(list var1 op var2 -> res-var)
     `(wire (,(string->symbol op) ,(string->symbol var1) ,(string->symbol var2)) -> ,(string->symbol res-var))])
  )

(define (mod-16bit x) (modulo x 65536))

(define AND (compose mod-16bit bitwise-and))
(define NOT (compose mod-16bit bitwise-not))

(provide AND NOT)
