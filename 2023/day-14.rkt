#lang racket
(require racket advent-of-code (only-in srfi/1 map))

(define (transpose lol)
  (apply map list lol))

(define (prep-input in)
  (map (compose (curry filter (λ (v) (not (zero? (string-length v)))))
                (curryr string-split ""))
       (string-split in "\n")))

(define platform (prep-input (fetch-aoc-input (find-session) 2023 14)))

;; get nearest obstacle
(define (get-obstacle col rock-idx)
  (- rock-idx (length (takef-right (take col rock-idx) (curry string=? ".")))))

;; rock update
(define (shift-rocks col)
  (let loop ([updated col]
             [idx 0])
    (if (= idx (length updated))
        updated
        (match (list-ref updated idx)
          ["O" (let ([obs (get-obstacle updated idx)])
                 (cond [(= obs idx) (loop updated (add1 idx))]
                       [else
                        (loop
                         (list-set (list-set updated obs "O") idx ".")
                         (add1 idx))]))]
          [_ (loop updated (add1 idx))]))))

(define (tilt-south lol)
  (transpose (map (compose reverse shift-rocks) (map reverse (transpose lol)))))

(define (tilt-north lol)
  (transpose (map shift-rocks (transpose lol))))

(define (tilt-east lol)
  (map (compose reverse shift-rocks reverse) lol))

(define (tilt-west lol)
  (map shift-rocks lol))

;; get-load
(define (north-beam-load tilted-rocks)
  (for/sum ([idx (in-range (length tilted-rocks) 0 -1)]
            [row tilted-rocks])
    (* idx (count (curry string=? "O") row))))

;; part 1
(north-beam-load (tilt-north platform))

;; part 2
;; cycle detection
(define (floyds vec)
  (let loop ([tort-pos 0]
             [hare-pos 1])
    (if (or (> hare-pos (sub1 (vector-length vec)))
            (> tort-pos (sub1 (vector-length vec))))
        #f
        (let ([tort (vector-ref vec tort-pos)]
              [hare (vector-ref vec hare-pos)])
          (if (equal? tort hare)
              #t
              (loop (add1 tort-pos)
                    ((compose add1 add1) hare-pos)))))))

;; get pre run out iteration count and cycle length
(define-values (pre cycle-length)
  (let ([iters 500])
    (let loop ([num-iters iters]
               [shifted-platform platform]
               [history '()]
               [cycles '()])
      (cond [(zero? num-iters) (match-let ([(list* pre cycle-ls) (map inexact->exact (reverse cycles))])
                                 (values pre (first (remove-duplicates (map (λ (a b) (/ (- b a) 2)) cycle-ls (rest cycle-ls))))))]
            [else
             (let ([round ((compose tilt-east tilt-south tilt-west tilt-north) shifted-platform)])
               (when (floyds (list->vector history))
                 (set! history '())
                 (set! cycles (cons (- iters num-iters) cycles)))
               (loop (sub1 num-iters) round (cons round history) cycles))]))))

;; calculate final value
(let ([cycle-count-offset (+ pre (modulo (- 1e9 pre) cycle-length))])
  (let loop ([num-iters cycle-count-offset] [shifted-platform platform])
    (match num-iters
      [0 (north-beam-load shifted-platform)]
      [_ (let ([round ((compose tilt-east tilt-south tilt-west tilt-north) shifted-platform)])
           (loop (sub1 num-iters) round))])))
