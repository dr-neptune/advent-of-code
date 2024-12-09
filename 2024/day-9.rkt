#lang racket
(require racket threading advent-of-code "../utils.rkt")

(define disk-map (~> (get-aoc 2024 9) (string-split "\n") first string->number int->digit-list))

(define disk-map (int->digit-list 2333133121414131402))

(define (translate-map disk-map)
  (for/fold ([updated-space '()] [id-idx 0]
             #:result (reverse updated-space))
            ([ele disk-map] [idx (in-naturals)])
    (if (even? idx)
        (values (append (make-list ele id-idx) updated-space) id-idx)
        (values (append (make-list ele #\.) updated-space) (add1 id-idx)))))

(define (resort fdisk-ls)
  (let loop ([fptr 0] [bptr (sub1 (length fdisk-ls))] [ls fdisk-ls])
    (if (>= fptr bptr) ls
        (let ([fval (list-ref ls fptr)] [bval (list-ref ls bptr)])
          (match fval
            [#\. (if (equal? bval #\.)
                     (loop fptr (sub1 bptr) ls)
                     (loop (add1 fptr) (sub1 bptr) (list-set (list-set ls fptr bval) bptr #\.)))]
            [_ (loop (add1 fptr) bptr ls)])))))

(define (indexed-dprod ls)
  (for/sum ([ele ls] [idx (in-naturals)]
            #:when (number? ele))
    (* ele idx)))

;; pt 1
(~> disk-map translate-map resort indexed-dprod)

;; part 2
(define (translate-map/2 disk-map)
  (for/fold ([files '()] [space '()] [id-idx 0]
             #:result (map reverse (list files space)))
            ([ele disk-map] [idx (in-naturals)])
    (if (even? idx)
        (values (cons (make-list ele id-idx) files) space id-idx)
        (values files (cons (make-list ele #\.) space) (add1 id-idx)))))

(define (enmesh lists)
  (define (_enmesh ls1 ls2)
    (if (empty? ls1)
        ls2
        (cons (first ls1) (_enmesh ls2 (rest ls1)))))
  (flatten (apply _enmesh lists)))

(define (move-file file-block space-block)
  (match-let
      ([(list closed-block open-block)
        (call-with-values (λ () (splitf-at space-block (λ (v) (not (equal? v #\.))))) list)])
    (append closed-block file-block (drop open-block (length file-block)))))

(define (file-fits? file-block space-block) (<= (length file-block) (length space-block)))

(define (resort/2 fdisk-ls)
  (let loop ([fptr (sub1 (length (first fdisk-ls)))] [sptr 0] [files (first fdisk-ls)] [space (second fdisk-ls)])
    (cond [(negative? fptr) (enmesh (list files space))]
          [(>= sptr fptr) (loop (sub1 fptr) 0 files space)]
          [else
           (let ([fval (list-ref files fptr)] [sval (list-ref space sptr)])
             (if (file-fits? fval (filter (curry equal? #\.) sval))
                 (loop (sub1 fptr) 0 (list-set files fptr (make-list (length fval) #\.)) (list-set space sptr (move-file fval sval)))
                 (loop fptr (add1 sptr) files space)))])))

(~> disk-map translate-map/2 resort/2 indexed-dprod)
