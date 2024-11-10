#lang racket
(require racket threading advent-of-code math math/number-theory)

(define card-key 8335663)
(define door-key 8614349)
(define p 20201227)
(define g 7)

;; 7^l mod 20201227 = card-key

;; baby-step giant-step algorithm for
;; calculating discrete logarithms
(define (baby-step-giant-step g h p)
  (define m (add1 (integer-sqrt (- p 1))))
  (define baby-steps (make-hash))

  ;; Baby steps
  (for ([j (in-range m)])
    (define value (modular-expt g j p))
    (hash-set! baby-steps value j))

  ;; Compute g^{-m} mod p
  (define g-inv (modular-inverse g p))
  (define g-inv-m (modular-expt g-inv m p))

  ;; Giant steps
  (define gamma h)
  (let/ec exit
    (for ([i (in-range m)])
      (define j (hash-ref baby-steps gamma #f))
      (when j
        (exit (+ (* i m) j)))
      (set! gamma (modulo (* gamma g-inv-m) p)))
    #f))

;; Find card's loop size
(define card-loop-size (baby-step-giant-step g card-key p))

;; Calculate the encryption key
(define encryption-key (modular-expt door-key card-loop-size p))
