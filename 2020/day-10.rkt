#lang racket
(require racket threading advent-of-code
         (only-in srfi/1 map))

(define jolts (~> (fetch-aoc-input (find-session) 2020 10)
                  (string-split "\n")
                  (map string->number _)
                  ((λ (v)
                     (cons (+ 3 (apply max v))
                           (append (sort v >) '(0)))) _)))

(define (vector-values-between idx1 idx2 vec)
  (for/list ([idx (in-range idx1 idx2)])
    (vector-ref vec idx)))

;; part 1
(let* ([counts (map - jolts (rest jolts))])
  (apply * (list (count (curry equal? 3) counts)
                 (count (curry equal? 1) counts))))

;; part 2
(let* ([jolts (list->vector (map cons (range 0 (length jolts)) (reverse jolts)))]
       [memo (make-hash)])
  (define (rec idx)
    (cond
      [(hash-has-key? memo idx) (hash-ref memo idx)]
      [(= idx (vector-length jolts)) 1]
      [else
       (let* ([curr-val (cdr (vector-ref jolts idx))]
              [possibilities
               (filter
                (λ (v) (<= (- (cdr v) curr-val) 3))
                (vector-values-between (add1 idx) (min (vector-length jolts) (+ idx 4)) jolts))]
              [result
               (match possibilities
                 ['() 1]
                 [_ (apply + (map (compose rec car) possibilities))])])
         (hash-set! memo idx result)
         result)]))
  (rec 0))
