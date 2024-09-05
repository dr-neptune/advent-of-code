#lang racket
(require racket threading advent-of-code)

(define boot (~> (fetch-aoc-input (find-session) 2020 8) (string-split "\n")))

(define boot
  (~> "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
      (string-split "\n")))

(define (parse-op op)
  (match-let ([(list operation num) (string-split op)])
    (list operation (string->number num))))

(define (get-next line-number)
  (list (add1 line-number)
        (list-ref boot (add1 line-number))))

(define gen
  (generator ()
             (let ([acc 0])
               (let rec ([line-number 0]
                         [op (first boot)])
                 (define get-next (λ ()
                                    (let ([new-loc (add1 line-number)])
                                      (begin
                                        (yield (list acc line-number))
                                        (rec new-loc (list-ref boot new-loc))))))
                 (define goto (λ (loc)
                                (let ([new-loc (+ loc line-number)])
                                  (begin
                                    (yield (list acc line-number))
                                    (rec new-loc (list-ref boot new-loc))))))
                 (match-let ([(list operation amt) (parse-op op)])
                   ;; (displayln (format "~a\t~a\t~a" acc line-number op))
                   (match operation
                     ["acc" (begin
                              (set! acc (+ acc amt))
                              (get-next))]
                     ["nop" (get-next)]
                     ["jmp" (goto amt)]))))))

;; idea
;; turn the above into a generator that generates the next acc value?
(for/list ([acc (in-producer gen (void))]
           [idx (in-range 30)])
  acc)

;; idea
;; yield both acc and line number
;; check for a cycle in the line number
;; Test: run for 30 iterations or until a cycle is detected
;; this still needs a proper cycle detection
(define final-result (let ([visited-lines '()])
  (for/list ([result (in-producer gen (void))]
             [idx (in-range 30)]
             #:break (member (second result) visited-lines))
    (let ([acc (first result)]
          [line-number (second result)])
      (set! visited-lines (cons line-number visited-lines))
      result))))
