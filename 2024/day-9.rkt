#lang racket
(require racket threading advent-of-code
         (only-in srfi/1 unfold-right)
         "../utils.rkt")

(define disk-map (int->digit-list 2333133121414131402))
(define disk-map (~> (get-aoc 2024 9) (string-split "\n") first string->number int->digit-list))

(define (translate-map disk-map)
  (for/fold ([updated-space '()] [id-idx 0]
             #:result (reverse updated-space))
            ([ele disk-map] [idx (in-naturals)])
    (if (even? idx)
        (values (append (make-list ele id-idx) updated-space) id-idx)
        (values (append (make-list ele #\.) updated-space) (add1 id-idx)))))

(define (resort fdisk-ls)
  (let loop ([fptr 0] [bptr (sub1 (length fdisk-ls))] [ls fdisk-ls])
    (if (> fptr bptr) ls
        (let ([fval (list-ref ls fptr)] [bval (list-ref ls bptr)])
          (match fval
            [#\. (if (equal? bval #\.)
                     (loop fptr (sub1 bptr) ls)
                     (loop (add1 fptr) (sub1 bptr) (list-set (list-set ls fptr bval) bptr #\.)))]
            [_ (loop (add1 fptr) bptr ls)])))))

(for/sum ([ele (~> disk-map translate-map resort)]
          [idx (in-naturals)]
          #:when (number? ele))
  (* ele idx))

#|

idea

rewrite resort to do the following

starting at the highest index, blk
find a continious block of . s.t. the lengths are equal.
If found, move blk to that spot, else do nothing
loop with sub1 idx

|#