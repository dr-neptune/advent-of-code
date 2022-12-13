 #lang racket
(require racket threading advent-of-code)

(define cpu-codes (fetch-aoc-input (find-session) 2022 10))

(define excodes
  #<<"
noop
addx 3
addx -5
"
)

(define excodes2
  #<<"
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"
)


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

;; idea

(define (execute-cpu-commands commands [stop-iter (length commands)])
  (let iter ([clock 1]
             [value 1]
             [cmds commands]
             [carry? #f]
             [carry-val 0])
    (begin
      (println (format "clock: ~a value: ~a cmd: ~a" clock value (first cmds)))
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
                     num))]))))


;; each cycle, add a '#
;; if the value is within +-1 of the cycle, then draw '#, else draw a .
(define (execute-cpu-commands commands [stop-iter (length commands)])
  (let ([graphics '()])
    (let iter ([clock 1]
               [value 1]
               [cmds commands]
               [carry? #f]
               [carry-val 0])
      (begin
        (if (and (<= (sub1 clock) value)
                 (>= (add1 clock) value))
            (if (equal? (modulo clock 40) 0)
                (pretty-print "\n#")
                (pretty-print "#"))
            (pretty-print "."))
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
                       num))])))))

(require racket/pretty)

(execute-cpu-commands cmds)
