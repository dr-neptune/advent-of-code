#lang racket/base

(require (only-in racket string-trim string-split) (for-syntax racket/base))
(provide wire format-datum)

(define-syntax (wire stx)
  (syntax-case stx ()
    [(wire arg var)  #'(define var arg)]))

(define (format-datum line)
    (let ([parts (map string-trim (string-split line "->"))])
      (when (= (length parts) 2)
        (let ([arg (read (open-input-string (car parts)))] [id (read (open-input-string (cadr parts)))])
          `(wire ,arg ,id)))))
