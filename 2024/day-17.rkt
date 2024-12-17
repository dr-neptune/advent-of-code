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

    (define (decode-operand opcode operand)
      (if (member opcode (list 0 2 5 6 7)) (combo-operand operand) operand))

    (define (operator opcode)
      (match opcode
        [0 adv] [1 bxl] [2 bst] [4 bxc] [5 out] [6 bdv] [7 cdv] [_ 'nope]))

    (let ([orig-program program])
      (let loop ([instr-pointer 0])
        (let ([program (drop orig-program instr-pointer)])
          ;; (displayln (format "A: ~a B: ~a C: ~a Instr: ~a" A B C instr-pointer))
          (if (empty? program)
              (list A B C)
              (begin
                ;; (displayln (apply format "opcode: ~a operand: ~a" (take program 2)))
                (match-let* ([(list* opcode operand rest) program]
                             [opcode/fn (operator opcode)]
                             [operand (decode-operand opcode operand)])
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

;; part 1
(run-program 2024 0 0 (list 0 3 5 4 3 0))

;; part 2
(run-program 117440 0 0 (list 0 3 5 4 3 0))

#|

we need to figure out the effect that A has on the output
and then use that to determine what the input should be

operations that affect A:
adv : A -> (/ A x^2)

adv is only called if our opcode is 0

I don't think we can use math to reverse engineer this because we have the jump construct
in the program. Maybe that is why the author asked to provide the A check beforehand.

idea:

try out some increasing A values and see if there is a pattern that emerges?

|#


(pretty-display
 (for/list ([a-val (in-range 2024 200000 1000)])
   (run-program a-val 0 0 (list 0 3 5 4 3 0))))

#|

Each out instruction outputs a value that corresponds to the low 3
bits of some intermediate state of A.  Dividing A by 2^N before output
shifts its bits down, so each subsequent output reads a different
portion of A’s binary form.

By tracing backward from the final output back up to the original A,
you can "rebuild" the binary number. Essentially, if you know the full
sequence of outputs (the exact copy of the program’s instructions you
want), you can determine what bits A had to contain initially, working
backward through the inverse of those divisions and mod operations.

Operations that involve A:

adv (0): divides A by 2^N
:: inv: A * 2^N
out (5): takes mod 8
:: inv: A mod 8 = k -> A - 8b = k

'(2 4 1 3 7 5 1 5 0 3 4 2 5 5 3 0)

idea:

can we iterate upwards by multiplying A by n^2 and transforms the number by 8*b + k?

|#


(pretty-display
 (for/list ([a-val (in-range 117000 118000 1)]
            #:when (equal? (run-program a-val 0 0 (list 0 3 5 4 3 0)) (string-join (map number->string (list 0 3 5 4 3 0)) ",")))
   a-val))
