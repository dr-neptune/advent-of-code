#lang racket
(require racket threading advent-of-code)

(define cups
  (~>> "389125467"
       (string-split _ "")
       (filter (compose not (curry equal? "")))
       (map string->number)))

#|

idea
seems like we need a lot of in-cycle

|#

#|
idea
start dumb and just nail the placement
eventually we may need to capture the circular nature of the placement
|#

;; ;; simulate a move
;; (define (simulate-move cups [move-number 1])
;;   (match-let ([(list circle o1 o2 o3 others ...) cups])
;;     (let ([pick-up (list o1 o2 o3)]
;;           [destination
;;            (let ([pool (cons circle others)])
;;              (let rec ([dest (sub1 circle)])
;;                (cond [(< (apply min pool) dest) (apply max pool)]
;;                      [(false? (member dest others)) (rec (sub1 dest))]
;;                      [else dest])))])
;;       (let* ([new-idx (index-of cups destination)]
;;              [placement-idx (if (>= new-idx (length others))
;;                                 0
;;                                 new-idx)])
;;         ;; note, the selected one is always the move number
;;         (append (take cups move-number)
;;                 (list destination)
;;                 pick-up
;;                 (drop (drop cups placement-idx) 1))))))


;; (simulate-move (simulate-move cups) 2)

;; (simulate-move '(3 2 8 9 1 5 4 6 7) 2)


;; let's try simulating with in-cycle
(define (stream-take-until st v)
  (let rec ([s st])
    (if (equal? (stream-first s) v)
        s
        (rec (stream-rest s)))))

(let ([input-cups cups])
  (let loop ([cups (sequence->stream (in-cycle input-cups))])
    (let* ([curr-cup-label (stream-first cups)]
           [pick-up (stream->list (stream-take (stream-rest cups) 3))]
           [not-pick-up? (λ (v) (false? (member v pick-up)))]
           [destination-set (filter not-pick-up? input-cups)]
           [destination (let loop ([seek (sub1 curr-cup-label)])
                          (cond [(< seek (apply min destination-set)) (apply max destination-set)]
                                [(member seek destination-set) seek]
                                [else (loop (sub1 seek))]))]
           [filtered-stream (stream-filter (λ (v) (false? (member v pick-up))) (stream-rest cups))]
           [new-stream (stream-rest (stream-take-until filtered-stream destination))])
      (begin
        (displayln (format "~a ~a ~a" curr-cup-label pick-up destination))
        (loop (stream-append (stream (in-list pick-up)) new-stream) (add1 curr-cup)))
      )))


;; try again!
#|

1. keep current cup
2. take next 3 cups
3. make a subset circle (in-cycle)
4. make an ordered subset circle including the current cup and take-right
e.g. if we have 3125467, we sort -> 1234567 and then find idx 3 and take to the left
idx 3 is 2, so we take idx 1 giving us 2. If we were idx 0, we would take 7
5. place the cups to the right of the destination in the non-ordered subset circle
6. take the new cup to the left
|#
(define (list-insert lst index val-or-vals [flatten? #t])
  (let* ((before (take lst index))
         (after (drop lst index))
         (to-insert (if (list? val-or-vals)
                        val-or-vals
                        (list val-or-vals)))
         (inserted (append before to-insert after)))
    (if flatten? (flatten inserted) inserted)))

(list-insert '(1 2 3 4 5) 5 '(6 7 8))

;; 0 move 1
(9 1 5 4 6 7 3 2 8)
-> (3 2 8 9 1 5 4 6 7)

;; 1 move 2

(3 2 5 8 4 6 7 9 1)
(9 2 5 8 4 1 3 6 7)
(7 2 5 8 4 1 9 3 6)

;; 0
(3 8 9 1 2 5 4 6 7)
(3 2 8 9 1 5 4 6 7)

;; 1
(3 2 5 4 6 7 8 9 1)

;; 2
(7 2 5 3 4 8 9 1 6)

;; idea
;; if the destination index - 3 > length then we must adjust?

(define (list-insert lst index val-or-vals [flatten? #t])
  (let* ((before (take lst index))
         (after (drop lst index))
         (to-insert (if (list? val-or-vals)
                        val-or-vals
                        (list val-or-vals)))
         (inserted (append before to-insert after)))
    (if flatten? (flatten inserted) inserted)))

(let (;; [cups cups] ;; add1
      ;; [cups '(3 2 8 9 1 5 4 6 7)]  ;; add1 works here
      [cups '(7 2 5 8 9 1 3 4 6)]  ;; things break down here
      ;; [cups '(3  2  5  8 4 6  7  9  1)]
      ;; [cups '(9  2  5  8  4 1 3  6  7)]
      ;; [cups '(5 7  4  1  8  3  9  2  6)]
      ;; [cups '(3  2  5  8 4 6  7  9  1)]
      )
  (let* ([current-cup-idx 2]
         [current-cup (list-ref cups current-cup-idx)]
         [pick-up (take (drop cups (add1 current-cup-idx)) 3)]
         [unordered-subset-circle (filter (λ (v) (not (member v pick-up))) cups)]
         [ordered-subset-circle (sort unordered-subset-circle <)]
         [destination (let ([curr-cup-sorted-idx (index-of ordered-subset-circle current-cup)])
                        (if (zero? curr-cup-sorted-idx)
                            (last ordered-subset-circle)
                            (list-ref ordered-subset-circle (sub1 curr-cup-sorted-idx))))]
         [new-order (list-insert unordered-subset-circle
                                 (add1 (index-of unordered-subset-circle destination)) pick-up)]
         [next-curr-idx (if (>= current-cup-idx (length cups)) 0 (add1 current-cup-idx))])
    (list current-cup
          pick-up
          unordered-subset-circle
          ordered-subset-circle
          destination
          new-order)))


(let ([cups cups])
  (for/fold ([curr-idx 0] [cups cups])
            ([idx (in-range 0 10)])
    (displayln (format "i: ~a cups: ~a" curr-idx cups))
    (let* ([current-cup-idx curr-idx]
           [current-cup (list-ref cups current-cup-idx)]
           [pick-up (take (drop cups (add1 current-cup-idx)) 3)]
           [unordered-subset-circle (filter (λ (v) (not (member v pick-up))) cups)]
           [ordered-subset-circle (sort unordered-subset-circle <)]
           [destination (let ([curr-cup-sorted-idx (index-of ordered-subset-circle current-cup)])
                          (if (zero? curr-cup-sorted-idx)
                              (last ordered-subset-circle)
                              (list-ref ordered-subset-circle (sub1 curr-cup-sorted-idx))))]
           [new-order (list-insert unordered-subset-circle
                                   (add1 (index-of unordered-subset-circle destination)) pick-up)]
           [next-curr-idx (if (>= current-cup-idx (length cups)) 0 (add1 current-cup-idx))])
      (values next-curr-idx new-order))))
