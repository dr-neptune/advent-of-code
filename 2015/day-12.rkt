#lang racket
(require racket threading advent-of-code json)

(define (str->json str)
  (with-input-from-string
    str
    (λ () (read-json))))

(define js
  (~> (fetch-aoc-input (find-session) 2015 12)
      str->json))

;; part 1
(define (get-numbers ls)
  (for/list ([ele ls])
    (cond [(number? ele) ele]
          [(hash? ele) (parse-hsh ele)]
          [(list? ele) (get-numbers ele)]
          [else '()])))

(define (parse-hsh hsh)
  (for/list ([(k v) (in-hash hsh)])
    (cond [(hash? v) (parse-hsh v)]
          [(number? v) v]
          [(list? v) (get-numbers v)]
          [else '()])))

(apply + (flatten (parse-hsh js)))

;; part 2
(define (parse-hsh hsh)
  (for/list ([(k v) (in-hash hsh)]
             #:when (false? (member "red" (hash-values hsh))))
    (cond [(hash? v) (parse-hsh v)]
          [(number? v) v]
          [(list? v) (get-numbers v)]
          [else '()])))

(apply + (flatten (parse-hsh js)))
