#lang racket
(require racket
         threading
         advent-of-code
         (only-in srfi/1 map))

(define bus
  (~>>
"939
7,13,x,x,59,x,31,19"
   ;; (fetch-aoc-input (find-session) 2020 13)
   (string-split _ "\n")
   ((λ (p)
     (list
      (string->number (first p))
      (for/list ([num (string-split (second p) ",")]
                 #:when (not (equal? "x" num)))
        (string->number num)))))))

;; part 1
(match-let ([(list dep-time buses) bus])
  (~>
   (map (λ (v)
          (cons v (+ v (- (remainder dep-time v)))))
        buses)
   (sort < #:key cdr)
   first
   ((λ (p) (* (car p) (cdr p))))))

;; part 2
(define bus
  (~>>
   (fetch-aoc-input (find-session) 2020 13)
   (string-split _ "\n")
   ((λ (p)
      (for/list ([num (string-split (second p) ",")]
                 [idx (in-naturals)]
                 #:when (not (equal? "x" num)))
        (list idx (string->number num)))))))


;; Extended Euclidean Algorithm to find gcd and coefficients
(define (extended-gcd a b)
  (if (= a 0)
      (values b 0 1) ; Return gcd, x, and y
      (let-values ([(gcd x1 y1) (extended-gcd (modulo b a) a)])
        (values gcd (- y1 (* (quotient b a) x1)) x1))))

;; Function to find the modular inverse of Ni modulo mod
(define (mod-inverse Ni mod)
  (let-values ([(gcd x y) (extended-gcd Ni mod)])
    (if (= gcd 1)
        (modulo x mod) ; The inverse is x % mod
        (error "No inverse exists for" Ni "modulo" mod))))

;; calculate product of all moduli
(define N (apply * (map second bus)))

;; calculate total product divided by modulus
(define Ni (map (λ (v) (/ N v)) (map second bus)))

(define modular-inverses (map (λ (a b) (mod-inverse a b)) Ni (map second bus)))

(define (solve-crt remainders Nis inverses N)
  (define total 0)  ; Initialize the sum
  ;; Sum up all r_i * N_i * M_i
  (for ([r remainders] [Ni Nis] [Mi inverses])
    (set! total (+ total (* r Ni Mi))))
  ;; Return the result modulo the product of all moduli (N)
  (modulo total N))

(solve-crt (map (compose - first) bus)
           Ni
           modular-inverses
           N)
