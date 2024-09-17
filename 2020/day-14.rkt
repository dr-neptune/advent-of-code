#lang racket
(require racket threading advent-of-code)

(define init-pr
  (~>>
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
(define memory-block (make-hash))

(define (make-bit-vec num)
  (let ([num-str (number->string num 2)])
    ((compose list->vector string->list)
     (string-append (make-string (- mask-len (string-length num-str)) #\0) num-str))))

;; part 1
(for ([instr-block init-pr])
  (let* ([mask (string->list (car instr-block))]
         [mask-indices (indexes-where mask (curry (compose not equal?) #\X))])
    (for ([mem-setter (second instr-block)])
      (match-let* ([(list mem-loc mem-val) mem-setter]
                   [num-bit (make-bit-vec mem-val)])
        (for ([idx mask-indices])
          (vector-set! num-bit idx (list-ref mask idx)))
        (hash-set! memory-block mem-loc ((compose (curryr string->number 2) list->string vector->list) num-bit))))))

(apply + (hash-values memory-block))

;; part 2
(define (apply-mask num-bit mask)
  (for/vector #:length 36 ([mele mask] [ele num-bit])
    (match mele
      [#\0 ele]
      [#\1 #\1]
      [#\X #\X])))

(define (get-binary-reps n)
  (for/list ([val (in-inclusive-range 0 (sub1 (expt 2 n)))])
    (let* ([bin-rep (number->string val 2)]
           [brsl (string-length bin-rep)])
      (if (> n brsl)
          (string->list (string-append (make-string (- n brsl) #\0) bin-rep))
          (string->list bin-rep)))))

(define (get-floating-addresses res)
  (let* ([floating-idxs (indexes-where (vector->list res) (curry equal? #\X))]
         [binary-reps (get-binary-reps (length floating-idxs))]
         [res/copy (vector-copy res)])
    (for/list ([bin binary-reps])
      (for ([bin-val bin]
            [idx floating-idxs])
        (vector-set! res/copy idx bin-val))
      (string->number (list->string (vector->list res/copy)) 2))))

(for ([instr-block init-pr])
  (let ([mask (string->list (car instr-block))])
    (for ([mem-setter (second instr-block)])
      (match-let* ([(list mem-loc mem-val) mem-setter])
        (define num-bit (make-bit-vec mem-loc))
        (define result (apply-mask num-bit mask))
        (define new-mem-locs (get-floating-addresses result))
        (for-each (λ (pos) (hash-set! memory-block pos mem-val)) new-mem-locs)))))

(apply + (hash-values memory-block))
