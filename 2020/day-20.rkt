#lang racket
(require racket threading advent-of-code racket/hash)

(define (parse-tile tile)
  (~> tile
      ((λ (tile)
         (list (string->number (substring (first tile) 5 9))
               (map string->list (rest tile)))))))

(define tiles
  (~> (fetch-aoc-input (find-session) 2020 20)
      (string-split "\n\n")
      (map (λ~> (string-split _ "\n") parse-tile) _)))

(define (get-edges data)
  (let* ([number (car data)]
         [matrix (cadr data)]
         [top (car matrix)]
         [bottom (car (reverse matrix))]
         [left (map car matrix)]
         [right (map last matrix)]
         [edges (list top right bottom left)]
         [edge-variants (apply append (map (λ (edge) (list edge (reverse edge))) edges))])
    (list number edge-variants)))

(define edge-counts
  (let ([hsh (make-hash)])
    (for* ([tile tiles]
           [edge (cadr (get-edges tile))])
      (hash-update! hsh edge add1 0))
    hsh))

(for/product ([tile tiles]
              #:do [(match-define (list tnum _) tile)
                    (define edges (cadr (get-edges tile)))
                    (define unique-edges (filter (λ~> (hash-ref edge-counts _) (equal? 1 _)) edges))]
              #:when (= 4 (length unique-edges)))
  tnum)

;; part 2

#|

idea

we just need to find how many sea monsters there are

and then subtract (num sea-monster hashes) from overall hashes

What if we remove the borders
make the overall picture
then a sea monster needs to fit the overall pictujre
starting from (0, (len sea monster)) search left, up, right, down
for a matching "signature". Each time a signature is found add to a counter

maybe easiest to do 4 iterations, 1 for each direction

we could even iterate over blocks the size of the sea monster
and just scan the whole image for them

|#

#|

fresh eyes

let's start by just trying to make the image?

|#

;; just lojic
(require threading)

(struct game (width n tiles dim sol vref) #:transparent)
(struct tile (id vec)      #:transparent)
(struct pos (tidx ori r c) #:transparent)

(define (get-vidx dim)
  (define (idx r c) (+ (* r dim) c))
  (define end (- dim 1))
  (list->vector (list (λ (r c) (idx r c))         (λ (r c) (idx r (- end c)))
                      (λ (r c) (idx (- end r) c)) (λ (r c) (idx (- end r) (- end c)))
                      (λ (r c) (idx c r))         (λ (r c) (idx c (- end r)))
                      (λ (r c) (idx (- end c) r)) (λ (r c) (idx (- end c) (- end r))))))
(define vidx (get-vidx 10))

(define (parse-game fname)
  (define (parse-tile s)
    (let* ([ lines (string-split s "\n")                    ]
           [ m     (regexp-match #px" (\\d+):" (car lines)) ]
           [ id    (second m)                               ]
           [ vec   (~> (string-join (cdr lines) "") (string->list _) (list->vector _)) ])
      (tile id vec)))
  (let* ([ tiles (for/vector ([ s (in-list (string-split (file->string fname) "\n\n")) ])
                   (parse-tile s)) ]
         [ dim   (sqrt (vector-length (tile-vec (vector-ref tiles 0)))) ]
         [ n     (vector-length tiles) ]
         [ width (sqrt n) ])
    (game width n tiles dim #f (get-vidx width))))

(define (solve g stack)
  (define (backtrack g stack)
    (match-define (pos tidx ori r c) (car stack))
    (if (< ori 7)
        (cons (pos tidx (add1 ori) r c) (cdr stack))
        (let ([ n (next-avail-idx stack (add1 tidx)) ])
          (if (< n (game-n g))
              (cons (pos n 0 r c) (cdr stack))
              (backtrack g (cdr stack))))))
  (define (next-pos g stack r c)
    (let ([ n (next-avail-idx stack 0) ])
      (if (>= n (game-n g))
          #f
          (let ([ new-c (add1 c) ])
            (if (< new-c (game-width g))
                (pos n 0 r new-c)
                (pos n 0 (add1 r) 0))))))
  (define (next-avail-idx stack beg)
    (let loop ([ n beg ])
      (if (findf (λ (p) (= n (pos-tidx p))) stack)
          (loop (add1 n)) n)))
  (let loop ([ stack stack ])
    (match-define (pos tidx ori r c) (car stack))
    (if (valid? g stack)
        (let ([ nextp (next-pos g stack r c) ])
          (if nextp (loop (cons nextp stack)) (reverse stack)))
        (loop (backtrack g stack)))))

(define (valid? g stack)
  (define (get vec ori row col) (vector-ref vec ((vector-ref vidx ori) row col)))
  (define (get-vecs g tidx o-tidx)
    (let ([ tiles (game-tiles g) ])
      (values (tile-vec (vector-ref tiles tidx)) (tile-vec (vector-ref tiles o-tidx)))))
  (define (above-matches? g tidx ori stack)
    (match-define (pos o-tidx o-ori r c) (list-ref stack (game-width g)))
    (let-values ([ (vec ovec) (get-vecs g tidx o-tidx)])
      (for/and ([ i (in-range (game-dim g)) ])
        (char=? (get vec ori 0 i) (get ovec o-ori (sub1 (game-dim g)) i)))))
  (define (left-matches? g tidx ori stack)
    (match-define (pos o-tidx o-ori r c) (list-ref stack 1))
    (let-values ([ (vec ovec) (get-vecs g tidx o-tidx)])
      (for/and ([ i (in-range (game-dim g)) ])
        (char=? (get vec ori i 0) (get ovec o-ori i (sub1 (game-dim g)))))))
  (match-define (pos tidx ori r c) (car stack))
  (and (or (< r 1) (above-matches? g tidx ori stack))
       (or (< c 1) (left-matches? g tidx ori stack))))

(define (solve-game fname)
  (let* ([ g         (parse-game fname)             ]
         [ positions (solve g (list (pos 0 0 0 0))) ])
    (struct-copy game g [ sol positions ])))

(define (roughness dim image vref)
  (define (count-hashes image) (for/sum ([ c (in-vector image) ]) (if (char=? c #\#) 1 0)))
  (define (num-monsters dim image vref)
    (for*/sum ([ row (in-range (- dim 2)) ][ col (in-range (- dim 18)) ])
      (if (is-monster? image vref row col) 1 0)))
  (define (is-monster? image vref row col)
    (andmap (λ (pair) (char=? #\# (vector-ref image (vref (+ row (car pair)) (+ col (cdr pair))))))
            '(( 0 . 18 ) ( 1 . 0 ) ( 1 . 5 ) ( 1 . 6 ) ( 1 . 11 ) ( 1 . 12 ) ( 1 . 17 ) ( 1 . 18 )
              ( 1 . 19 ) ( 2 . 1 ) ( 2 . 4 ) ( 2 . 7 ) ( 2 . 10 ) ( 2 . 13 ) ( 2 . 16 ))))
  (- (count-hashes image) (* (num-monsters dim image vref) 15)))

(define (create-image g)
  (define (get g g-row g-col t-row t-col)
    (match-define (pos tidx ori r c) (list-ref (game-sol g) ((vector-ref (game-vref g) 0) g-row g-col)))
    (let* ([ tile (vector-ref (game-tiles g) tidx)                ]
           [ vref (vector-ref vidx ori)                           ]
           [ val  (vector-ref (tile-vec tile) (vref t-row t-col)) ])
      val))
  (let* ([ width     (game-width g)                        ]
         [ dim       (game-dim g)                          ]
         [ image-dim (* width (- dim 2))                   ]
         [ vec       (make-vector (* image-dim image-dim)) ]
         [ idx       0                                     ])
    (for*/vector ([ g-row (in-range 0 width)      ]
                  [ t-row (in-range 1 (sub1 dim)) ]
                  [ g-col (in-range 0 width)      ]
                  [ t-col (in-range 1 (sub1 dim)) ])
      (get g g-row g-col t-row t-col))))

(define (part1 g)
  (let* ([ width (game-width g) ]
         [ vref  (vector-ref (game-vref g) 0) ]
         [ get   (λ (r c)
                   (let* ([ p    (list-ref (game-sol g) (vref r c)) ]
                          [ tile (vector-ref (game-tiles g) (pos-tidx p)) ])
                     (string->number (tile-id tile)))) ])
    (apply * (list (get 0 0) (get 0 (sub1 width))
                   (get (sub1 width) 0) (get (sub1 width) (sub1 width))))))

(define (part2 g)
  (let* ([ dim   (* (game-width g) (- (game-dim g) 2)) ]
         [ vidx  (get-vidx dim)                        ]
         [ image (create-image g)                      ])
    (let loop ([ n +inf.0 ][ lst '(0 1 2 3 4 5 6 7) ])
      (if (null? lst)
          n
          (let ([ r (roughness dim image (vector-ref vidx (car lst))) ])
            (loop (if (< r n) r n) (cdr lst)))))))

(module+ test (require rackunit)
  (define solution (solve-game "day20.txt"))
  (check-equal? (part1 solution) 17250897231301)
  (check-equal? (part2 solution) 1576))
