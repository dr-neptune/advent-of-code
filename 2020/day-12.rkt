#lang racket
(require racket threading advent-of-code)

(define pa
  (~>>
   (fetch-aoc-input (find-session) 2020 12)
   (string-split _ "\n")
   (map (λ (instruction)
          (let ([rem (regexp-match #px"^([A-Z])([0-9]+)$" instruction)])
            (list (second rem) (string->number (third rem))))))))

(define (append-values . fns-and-extras)
  (define (collect-values fns)
    ;; Helper function to collect values from each function
    (apply append
           (map (lambda (fn) (call-with-values fn list)) fns)))

  ;; Separate functions from extra values
  (define fns (filter procedure? fns-and-extras))
  (define extras (filter (lambda (x) (not (procedure? x))) fns-and-extras))

  ;; Collect the values from all functions and append extras
  (apply values (append (collect-values fns) extras)))


;; part 1
(define (turn curr-dir turn-dir amt)
  (define directions '("N" "E" "S" "W"))  ; Order of cardinal directions
  (define curr-index (index-of directions curr-dir))  ; Get the current direction index
  (define steps (/ amt 90)) ; calc number of steps

  ;; Adjust the steps based on the turn direction (right = forward, left = backward)
  (define new-index
    (if (equal? turn-dir "R")
        (modulo (+ curr-index steps) 4)  ; "R"
        (modulo (- curr-index steps) 4)))  ; "L"

  ;; Return the new direction
  (list-ref directions new-index))

(define (move-in-direction dir x y amt)
  (match dir
    ["N" (values x (+ y amt))]
    ["S" (values x (- y amt))]
    ["E" (values (+ x amt) y)]
    ["W" (values (- x amt) y)]))

(for/fold ([x 0] [y 0] [facing-dir "E"]
           #:result (+ (abs x) (abs y)))
          ([instruction pa])
  (match-let ([(list dir amt) instruction])
    (match dir
      [(or "N" "S" "E" "W")
       (append-values (λ () (move-in-direction dir x y amt)) facing-dir)]
      [(or "R" "L") (values x y (turn facing-dir dir amt))]
      ["F" (append-values (λ () (move-in-direction facing-dir x y amt)) facing-dir)])))


;; part 2
(define (rotate-waypoint x y direction theta)
  (match (list direction theta)
    ;; Clockwise rotations ("R")
    [(list "R" 90)  (values y (- x))]
    [(list "R" 180) (values (- x) (- y))]
    [(list "R" 270) (values (- y) x)]
    ;; Counterclockwise rotations ("L")
    [(list "L" 90)  (values (- y) x)]
    [(list "L" 180) (values (- x) (- y))]
    [(list "L" 270) (values y (- x))]))

(for/fold ([wx 10] [wy 1]
           [sx 0] [sy 0]
           #:result (+ (abs sx) (abs sy)))
          ([instruction pa])
  (match-let ([(list dir amt) instruction])
    (match dir
      [(or "N" "S" "E" "W")
       (append-values (λ () (move-in-direction dir wx wy amt)) sx sy)]
      [(or "R" "L")
       (append-values (λ () (rotate-waypoint wx wy dir amt)) sx sy)]
      ["F"
       (values wx wy (+ sx (* amt wx)) (+ sy (* amt wy)))])))
