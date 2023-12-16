#lang racket
(require racket advent-of-code)

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (val-map fn ht)
  (for/hash ([(k v) (in-hash ht)]) (values k (fn v))))

(define init-seq (map (curryr string-replace "\n" "") (string-split (fetch-aoc-input (find-session) 2023 15) ",")))

(define (Holiday-ASCII-String-Helper-algorithm_appendix_1A str)
  (let ([current-value 0])
    (let loop ([ascii-values (map char->integer (string->list str))])
      (if (empty? ascii-values)
          current-value
          (begin
            (set! current-value (remainder (* 17 (+ current-value (first ascii-values))) 256))
            (loop (rest ascii-values)))))))

;; part 1
(apply + (map Holiday-ASCII-String-Helper-algorithm_appendix_1A init-seq))

;; part 2
(define (split-instruction instruction)
  (match (string->list instruction)
    [(list a ..1 #\= c)
     (let ([str (list->string a)])
       (list (Holiday-ASCII-String-Helper-algorithm_appendix_1A str)
             str
             (char->digit c)))]
    [(list a ..1 #\-)
     (let ([str (list->string a)])
       (list (Holiday-ASCII-String-Helper-algorithm_appendix_1A str) str))]))

(define (set-lenses instruction-lens-sets)
  (let ([hsh (make-hash)])
    (for ([instruction instruction-lens-sets])
      (match instruction
        [(list a b)
         (match-let ([(list HASH code) instruction])
           (hash-set! hsh HASH (filter (λ (v) (not (equal? code (car v)))) (hash-ref hsh HASH '()))))]
        [(list a b c)
         (match-let ([(list HASH code val) instruction])
           (let ([current-vals (hash-ref hsh HASH '())])
             (if ((compose not false? (curry member code)) (map car current-vals))
                 (hash-set! hsh HASH
                            (list-set current-vals (index-of (map car current-vals) code) (list code val)))
                 (hash-set! hsh HASH (cons (list code val) current-vals)))))]))
    (val-map reverse hsh)))

(apply +
       (hash-map
        (set-lenses (map split-instruction init-seq))
        (λ (k v)
          (for/sum ([idx (in-inclusive-range 1 (length v))]
                    [val v])
            (* (add1 k) idx (cadr val))))))
