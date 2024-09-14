#lang racket

(require racket threading advent-of-code)

(define init-pr
  (~>>
;;    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
;; mem[8] = 11
;; mem[7] = 101
;; mem[8] = 0"
   (fetch-aoc-input (find-session) 2020 14)
   (string-split _ "mask = ")
   (map (compose
         (λ (p)
           (list (string-trim (first p) "mask = ")
                 (map (λ (inst)
                        (match-let ([(list _ addr val) (regexp-match #rx"([0-9]+)] = ([0-9]+)" inst)])
                          (map string->number (list addr val))))
                      (rest p))))
         (curryr string-split "\n")))))

(define mask-len 36)

;; make a memory block as big as is needed
(define memory-block (make-vector (apply max (map (λ (inst-set)
                                                    (apply max (map second (cadr inst-set))))
                                                  init-pr))))

(vector-length memory-block)  ;; 101 / 918_112_464

(define (make-bit-str-ls num)
  (let ([num-str (number->string num 2)])
    ((compose list->vector string->list)
     (string-append (make-string (- mask-len (string-length num-str)) #\0) num-str))))

;; part 1
(for ([instr-block init-pr])
  (let* ([mask (string->list (car instr-block))]
         [mask-indices (indexes-where mask (curry (compose not equal?) #\X))])
    (for ([mem-setter (second instr-block)])
      (match-let* ([(list mem-loc mem-val) mem-setter]
                   [num-bit (make-bit-str-ls mem-val)])
        (for ([idx mask-indices])
          (vector-set! num-bit idx (list-ref mask idx)))
        (vector-set! memory-block mem-loc ((compose (curryr string->number 2) list->string vector->list) num-bit))))))

(apply + (vector->list memory-block))


;; part 2
#|

for each mem-loc, apply the mask. When an x is written, after the mask, we need to take all combinations at those locations

so we need a function result->combinations that returns a list of memory addresses to apply the values.
the values given aren't masked, so mem[8] = 11 means that each of the different memory areas spawned all get 11

this makes me think that we should avoid simulation altogether.
We should make the mask on memory addresses, calculate the new addresses and multiply it by the value.
Unfortunately, with this approach we can't guarantee that there isn't an overlap between multiple addresses
hitting the same slot, we need to either do the simulation or keep track of which values were hit. Likely easier
to just do the simulation

|#
