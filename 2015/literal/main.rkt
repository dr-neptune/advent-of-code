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
    [(wire arg -> var) #'(begin
                           (define var arg)
                           (displayln (format "~a: ~a" 'var var)))]))

(define (format-datum line)
  (match-let ([(list num _ var) (string-split line)])
    `(wire ,(string->number num) -> ,(string->symbol var))))
