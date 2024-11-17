#lang racket
(require racket racket/hash threading advent-of-code)

(define directions (~> (fetch-aoc-input (find-session) 2015 3)
                       string->list))


(define (offset loc dir)
  (match dir
    [#\> (map + loc '(1 0))]
    [#\< (map + loc '(-1 0))]
    [#\^ (map + loc '(0 1))]
    [#\v (map + loc '(0 -1))]))


;; pt 1
(define (make-santa-map directions)
  (let ([hsh (make-hash '([(0 0) . 1]))])
    (let loop ([loc (list 0 0)] [dir directions])
      (if (empty? dir)
          hsh
          (let ([new-loc (offset loc (first dir))])
            (begin
              (hash-update! hsh new-loc add1 0)
              (loop new-loc (rest dir))))))))

(hash-count (make-santa-map directions))

;; pt 2
#|

idea
split these into even / odd
add the origin stop
then hash-union / add items
|#

;; split up
(define (make-split-map pred? directions)
  (for/list ([dir directions]
             [idx (in-naturals)]
             #:when (pred? idx))
    dir))

(define reg-santa-dirs (make-split-map odd? directions))
(define robo-santa-dirs (make-split-map even? directions))
(define reg-santa-map (make-santa-map reg-santa-dirs))
(define robo-santa-map (make-santa-map robo-santa-dirs))

(hash-union! reg-santa-map robo-santa-map #:combine +)

(hash-count reg-santa-map)
