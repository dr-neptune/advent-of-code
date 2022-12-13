 #lang racket
(require racket threading advent-of-code)

(define cpu-codes (fetch-aoc-input (find-session) 2022 10))

(define (execute-cpu-commands commands [stop-iter (length commands)])
  (let iter ([clock 1]
             [value 1]
             [cmds commands]
             [carry? #f]
             [carry-val 0])
      (cond [(empty? cmds) value]
            [(equal? stop-iter clock) value]
            [(equal? (first cmds) "noop")
             (iter (add1 clock) value (rest cmds) #f 0)]
            [carry? (iter (add1 clock) (+ value carry-val) (rest cmds) #f 0)]
            [else
             (let* ([adder (string-split (first cmds))]
                    [num (string->number (second adder))])
               (iter (add1 clock)
                     value
                     cmds
                     #t
                     num))])))

(define (signal-strength? instructions clock-time)
  (* clock-time
     (execute-cpu-commands instructions clock-time)))

(define cmds (string-split cpu-codes "\n"))

;; pt 1
(foldl + 0 (map ((curry signal-strength?) cmds) '(20 60 100 140 180 220)))

;; pt 2
(define (execute-cpu-commands commands)
  (let iter ([clock 0]
             [value 1]
             [cmds commands]
             [carry? #f]
             [carry-val 0])
    (begin
      (begin
        (cond [(and (<= (modulo clock 40) (add1 value))
                    (>= (modulo clock 40) (sub1 value)))
               (display "#")]
              [else (display ".")]))
      (cond [(empty? cmds) ""]
            [(equal? (first cmds) "noop")
             (iter (add1 clock) value (rest cmds) #f 0)]
            [carry? (iter (add1 clock) (+ value carry-val) (rest cmds) #f 0)]
            [else
             (let* ([adder (string-split (first cmds))]
                    [num (string->number (second adder))])
               (iter (add1 clock) value cmds #t num))]))))

(execute-cpu-commands (string-split cpu-codes "\n"))
