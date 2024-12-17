#lang racket
(require racket threading "../utils.rkt")

(match-define (list (list A B C) program)
  (~> ;; "Register A: 729
;; Register B: 0
;; Register C: 0

;; Program: 0,1,5,4,3,0"
      (get-aoc 2024 17)
      (string-split "\n\n")
      ((λ (pair)
         (list  (map (λ~>> (regexp-match #px"\\d+") first string->number)
                     (string-split (first pair) "\n"))
                (map string->number (regexp-match* #px"\\d+" (second pair))))))))

(define (run-program A B C program)
  (let ([A A] [B B] [C C] [OUTS '()])
    (define (adv x) (set! A (truncate (/ A (expt 2 x)))))
    (define (bxl x) (set! B (bitwise-xor B x)))
    (define (bst x) (set! B (remainder x 8)))
    (define (bxc _) (set! B (bitwise-xor B C)))
    (define (out x) (set! OUTS (cons (~> x (remainder 8) number->string) OUTS)))
    (define (bdv x) (set! B (truncate (/ A (expt 2 x)))))
    (define (cdv x) (set! C (truncate (/ A (expt 2 x)))))

    (define (combo-operand operand) (match operand [4 A] [5 B] [6 C] [_ operand]))

    (define (operator opcode)
      (match opcode
        [0 adv] [1 bxl] [2 bst] [4 bxc] [5 out] [6 bdv] [7 cdv] [_ 'nope]))

    (let ([orig-program program])
      (let loop ([instr-pointer 0])
        (let ([program (drop orig-program instr-pointer)])
          (displayln (format "A: ~a B: ~a C: ~a Instr: ~a" A B C instr-pointer))
          (if (empty? program)
              (list A B C)
              (begin
                (displayln (apply format "opcode: ~a operand: ~a" (take program 2)))
                (match-let* ([(list* opcode operand rest) program]
                             [opcode/fn (operator opcode)]
                             [operand (combo-operand operand)])
                  (if (equal? opcode 3)
                      (if (equal? A 0)
                          (loop (+ instr-pointer 2))
                          (loop operand))
                      (begin
                        (opcode/fn operand)
                        (loop (+ instr-pointer 2)))))))
          (displayln (format "Final State: A: ~a B: ~a C: ~a" A B C))
          (string-join (reverse OUTS) ","))))))

(run-program A B C program)
