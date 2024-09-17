#lang racket
(require racket threading advent-of-code)

(define (parse-op op)
  (match-let ([(list operation num) (string-split op)])
    (list operation (string->number num))))

(define boot (~> (fetch-aoc-input (find-session) 2020 8)
                 (string-split "\n")
                 (map parse-op _)))

;; part 1
(let loop ([seen '()] [acc 0] [idx 0])
  (match-let ([(list op amt) (list-ref boot idx)])
    (if (member idx seen)
        acc
        (match op
          ["acc" (loop (cons idx seen) (+ amt acc) (add1 idx))]
          ["jmp" (loop (cons idx seen) acc (+ amt idx))]
          [_ (loop (cons idx seen) acc (add1 idx))]))))

;; part 2

;; update part 1 to return #f if a cycle is found, acc if not
(define (try-boot-set boot)
  (let loop ([seen '()] [acc 0] [idx 0])
    (match idx
      [(== (length boot)) acc]
      [_
       (match-let ([(list op amt) (list-ref boot idx)])
         (if (member idx seen)
             #f
             (match op
               ["acc" (loop (cons idx seen) (+ amt acc) (add1 idx))]
               ["jmp" (loop (cons idx seen) acc (+ amt idx))]
               [_ (loop (cons idx seen) acc (add1 idx))])))])))


;; iterate through
;; if we find a nop or a jmp, swap and check if it has a cycle
(let ([swap-instr (λ (op idx)
                    (let ([new-op (match op ["nop" "jmp"] ["jmp" "nop"])])
                      (list-update boot idx (λ (p) (list new-op (second p))))))])
  (for/or ([instr (in-list boot)]
           [idx (in-naturals)]
           #:do [(define op (first instr))]
           #:when (member op (list "nop" "jmp")))
    (try-boot-set (swap-instr op idx))))
