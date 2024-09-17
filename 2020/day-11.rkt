#lang racket
(require racket threading advent-of-code)

(define seats (~> (fetch-aoc-input (find-session) 2020 11)
                  (string-split "\n")
                  (map (compose list->vector string->list) _)
                  list->vector))

;; Define a structure to hold adjacent values in clockwise order starting from North
(struct adjacents (n ne e se s sw w nw) #:transparent)

;; Define the grid class
(define grid%
  (class object%
    (init-field [grid #f])
    (field [current-iteration 0]
           [previous-state #f]
           [num-rows (vector-length grid)]
           [num-cols (vector-length (vector-ref grid 0))])

    ;; Method to get the value at (x, y)
    (define/public (get-value x y)
      (if (and (<= 0 x (- num-cols 1))
               (<= 0 y (- num-rows 1)))
          (vector-ref (vector-ref grid y) x)
          #f))

    ;; Method to get adjacents at (x, y)
    (define/public (get-adjacents x y)
      (define (safe-get x y)
        (if (and (<= 0 x (- num-cols 1))
                 (<= 0 y (- num-rows 1)))
            (vector-ref (vector-ref grid y) x)
            #f))

      ;; Collect adjacent values in clockwise order starting from North
      (define n  (safe-get x       (- y 1)))    ; N
      (define ne (safe-get (+ x 1) (- y 1)))    ; NE
      (define e  (safe-get (+ x 1) y))          ; E
      (define se (safe-get (+ x 1) (+ y 1)))    ; SE
      (define s  (safe-get x       (+ y 1)))    ; S
      (define sw (safe-get (- x 1) (+ y 1)))    ; SW
      (define w  (safe-get (- x 1) y))          ; W
      (define nw (safe-get (- x 1) (- y 1)))    ; NW

      ;; Return the adjacents structure
      (adjacents n ne e se s sw w nw))

    (define/public (compute-next-step x y)
      (let* ([curr-val (get-value x y)]
             [curr-adj (get-adjacents x y)]
             [occ-adj (vector-count (curry equal? #\#) (struct->vector curr-adj))])
        (match curr-val
          [#\L (if (zero? occ-adj) #\# #\L)]
          [#\# (if (<= 4 occ-adj) #\L #\#)]
          [_ curr-val])))

    (define/public (compute-next-state)
      (define new-grid
        (for/vector ([y (in-range num-rows)])
          (for/vector ([x (in-range num-cols)])
            (compute-next-step x y))))
      ;; Update the current state and increase the iteration count
      (set! previous-state grid)
      (set! grid new-grid)
      (set! current-iteration (add1 current-iteration))
      grid)

    ;; Public accessors for the attributes
    (define/public (get-current-iteration) current-iteration)
    (define/public (get-current-state) grid)
    (define/public (get-previous-state) previous-state)

    (super-new)))

;; Create an instance of the grid class
(define (get-occupied-seats grid)
  (let loop ()
  (if (equal? (send my-grid get-previous-state) (send my-grid get-current-state))
      (apply + (vector->list (vector-map (curry vector-count (curry equal? #\#)) (send my-grid get-current-state))))
      (begin
        (send my-grid compute-next-state)
        (loop)))))

;; part 1
(define seat-grid (new grid% [grid seats]))
(get-occupied-seats seat-grid)

;; pt 2
(define visibility-grid%
  (class grid%
    (super-new)
    (inherit-field num-cols num-rows)

    ;; Override get-adjacents with get-visible-adjacents
    (define/override (get-adjacents x y)
      ;; Method to get the first seat visible in a direction (dx, dy)
      (define (get-first-seat x y dx dy)
        (let loop ([x (+ x dx)] [y (+ y dy)])
          (if (or (< x 0) (>= x num-cols) (< y 0) (>= y num-rows))
              #f ; Reached the edge
              (let ([val (send this get-value x y)])
                (if (equal? val #\.) ; If floor, continue in same direction
                    (loop (+ x dx) (+ y dy))
                    val)))))

      (define n  (get-first-seat x y  0 -1))   ; N
      (define ne (get-first-seat x y  1 -1))   ; NE
      (define e  (get-first-seat x y  1  0))   ; E
      (define se (get-first-seat x y  1  1))   ; SE
      (define s  (get-first-seat x y  0  1))   ; S
      (define sw (get-first-seat x y -1  1))   ; SW
      (define w  (get-first-seat x y -1  0))   ; W
      (define nw (get-first-seat x y -1 -1))   ; NW

      (adjacents n ne e se s sw w nw))

    ;; Override compute-next-step to adjust occupancy threshold
    (define/override (compute-next-step x y)
      (let* ([curr-val (send this get-value x y)]
             [curr-adj (get-adjacents x y)]
             [occ-adj (vector-count (curry equal? #\#) (struct->vector curr-adj))])
        (match curr-val
          [#\L (if (zero? occ-adj) #\# #\L)]
          [#\# (if (<= 5 occ-adj) #\L #\#)]  ; Threshold adjusted to 5
          [_ curr-val])))))

(define seat-grid (new visibility-grid% [grid seats]))
(get-occupied-seats seat-grid)
