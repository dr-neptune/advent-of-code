#lang racket
(require racket racket/hash memo threading advent-of-code)

(define guard-map
  (~> (fetch-aoc-input (find-session) 2024 6 #:cache #t)  ;; 130x130
      (string-split "\n")
      (map (λ~> string->list list->vector) _)
      list->vector))

(define (get-xy vec x y) (vector-ref (vector-ref vec y) x))

(define (get-locations grid char)
  (for*/list ([row-idx (in-range (vector-length grid))]
              [col-idx (in-range (vector-length (vector-ref grid 0)))]
              #:when (char=? (get-xy grid col-idx row-idx) char))
    (list col-idx row-idx)))

(define (go-right dir)
  (match dir
    ['(0 -1) '(1 0)] ;; n -> e
    ['(1 0) '(0 1)]  ;; e -> s
    ['(0 1) '(-1 0)] ;; s -> w
    [_ '(0 -1)]))    ;; w -> n

(define (in-bounds? x y)
  (let* ([x-len (vector-length (vector-ref guard-map 0))]
         [y-len (vector-length guard-map)])
    (and (< -1 x x-len)
         (< -1 y y-len))))

(define out-of-bounds? (compose not in-bounds?))

(define (get-guard-path starting-position [obstacles obstacles])
  (let loop ([guard starting-position] [seen (list starting-position)] [dir '(0 -1)])
    (let ([next-pos (map + guard dir)])
      (cond [(apply out-of-bounds? next-pos) (cons guard seen)]
            [(hash-has-key? obstacles next-pos) (loop guard seen (go-right dir))]
            [else (loop next-pos (cons guard seen) dir)]))))

(define obstacles
  (for/hash ([loc (get-locations guard-map #\#)])
    (values loc 0)))

(define starting-position (first (get-locations guard-map #\^)))

;; pt 1
(~> starting-position
    get-guard-path
    remove-duplicates
    length)

;; part 2
(define (get-guard-path/cycle-detection starting-position [obstacles obstacles])
  (let ([visited-states (make-hash)])
    (let loop ([guard starting-position] [dir '(0 -1)])
      (cond [(apply out-of-bounds? guard) 'out-of-bounds]
            [(hash-has-key? visited-states (list guard dir)) 'cycle-detected]
            [else
             (begin
               (hash-set! visited-states (list guard dir) #t)
               (let ([next-pos (map + guard dir)])
                 (if (hash-has-key? obstacles next-pos)
                     (loop guard (go-right dir))
                     (loop next-pos dir))))]))))

(define (make-new-obstacles x y)
  (if (hash-has-key? obstacles (list x y))
      obstacles
      (hash-union (make-immutable-hash `([(,x ,y) . 0])) obstacles)))

(for*/sum ([x (in-range (vector-length (vector-ref guard-map 0)))]
           [y (in-range (vector-length guard-map))]
           #:do [(define new-obs (make-new-obstacles x y))
                 (define path (get-guard-path/cycle-detection starting-position new-obs))]
           #:when (equal? path 'cycle-detected))
  1)
