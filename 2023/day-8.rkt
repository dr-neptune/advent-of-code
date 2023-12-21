#lang racket
(require racket advent-of-code data/monad data/applicative megaparsack megaparsack/text)

(define (string-eval-parser str-in parser)
  (parse-result! (parse-string parser str-in)))

(define map/p
  (let ([space+ (many/p space/p)])
    (do [idx <- (many/p (char-between/p #\A #\Z))]
        space+
      (string/p "=")
      space+
      (string/p "(")
      [from <- (many/p (char-between/p #\A #\Z))]
      (string/p ", ")
      [to <- (many/p (char-between/p #\A #\Z))]
      (string/p ")")
      ;; [boat-ints <- (many/p integer/p #:sep (many/p space/p))]
      (pure (map list->string (list idx from to))))))

(define input (fetch-aoc-input (find-session) 2023 8))

(match-define (list instructions maps)
  (apply
   (λ (a b)
     (list a (map (curryr string-eval-parser map/p) (string-split b "\n"))))
   (string-split input "\n\n")))

;; part 1
(define map-graph
  (for/hash ([curr-map maps])
  (values (first curr-map) (make-hash `((#\L ,(second curr-map))
                                        (#\R ,(third curr-map)))))))

(define (get-node node-str dir)
  (hash-ref (hash-ref map-graph node-str) dir))

(define (get-node-trip starting-value [ending-value "ZZZ"] [end-xform identity])
  (let ([curr-node starting-value])
    (for/sum ([dir (in-cycle (string->list instructions))]
              #:break (equal? (end-xform curr-node) ending-value))
      (let ([nxt (first (get-node curr-node dir))])
        (set! curr-node nxt) 1))))

(get-node-trip "AAA")

;; part 2
(apply lcm
       (for/list ([node (filter (curryr string-suffix? "A") (hash-keys map-graph))])
         (get-node-trip node #\Z (λ (str) (last (string->list str))))))
