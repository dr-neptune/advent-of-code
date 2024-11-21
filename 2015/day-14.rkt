#lang racket
(require racket threading advent-of-code)

(define re-pattern #px"^(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.$")

(define reindeer
  (~>
;; "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
;; Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
   (fetch-aoc-input (find-session) 2015 14)
   (string-split "\n")
   (map
    (λ (s) (match-let ([(list _ reindeer speed seconds rest-time)
                        (regexp-match re-pattern s)])
             (cons reindeer (map string->number (list speed seconds rest-time)))))
    _)))

(define (get-distance total-seconds movement-speed movement-duration rest-duration)
  (for/fold ([distance 0] [num-seconds 0] [phase 'move] #:result distance)
            ([sec (in-range total-seconds)])
    (match phase
      ['move
       (let ([new-dist (+ distance movement-speed)]
             [num-secs (add1 num-seconds)])
         (if (equal? num-secs movement-duration)
             (values new-dist 0 'rest)
             (values new-dist num-secs phase)))]
      ['rest
       (let ([num-secs (add1 num-seconds)])
         (if (equal? num-secs rest-duration)
             (values distance 0 'move)
             (values distance num-secs phase)))])))

(sort (map (λ (r)
             (list (first r) (apply (curry get-distance 2503) (rest r))))
           reindeer)
      >
      #:key cadr)


;; part 2
;; idea
;; lets just br00tally write to a higher level var
;; who gives a h00t
(define (get-distance/2 total-seconds movement-speed movement-duration rest-duration)
  (let ([ye-ole-intermediary '()])
    (for/fold ([distance 0] [num-seconds 0] [phase 'move] #:result distance)
              ([sec (in-range total-seconds)])
      (set! ye-ole-intermediary (cons (list sec distance) ye-ole-intermediary))
      (match phase
        ['move
         (let ([new-dist (+ distance movement-speed)]
               [num-secs (add1 num-seconds)])
           (if (equal? num-secs movement-duration)
               (values new-dist 0 'rest)
               (values new-dist num-secs phase)))]
        ['rest
         (let ([num-secs (add1 num-seconds)])
           (if (equal? num-secs rest-duration)
               (values distance 0 'move)
               (values distance num-secs phase)))]))
    (reverse ye-ole-intermediary)))


(define (compare-times . args)
  (let ([max-val (apply max args)])
    (let ([lead-reindeer (map (curry list-ref reindeer-names) (indexes-of args max-val))])
      (for-each (λ (lead-reindeer-name)
             (hash-update! reindeer-scores lead-reindeer-name add1))
           lead-reindeer))))

(define reindeer-distances
  (map (λ (r)
         (list (first r) (apply (curry get-distance/2 2504) (rest r)))) reindeer))
(define reindeer-names (map first reindeer))
(define reindeer-scores (make-hash (map (λ (r) (cons r 0)) reindeer-names)))

(apply
 (curry for-each compare-times)
 (map (λ (times) (flatten (map cdr times)))
           (map cdadr reindeer-distances)))

reindeer-scores
