#lang racket
(require racket threading advent-of-code megaparsack megaparsack/text)

(define tile-map
  (~>
   ;; "esenee"
   "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew"
   (string-split "\n")))
   ;; (map (compose
   ;;       (curry filter (λ (s) ((compose not zero?) (string-length s))))
   ;;       (curryr string-split "")) _)))

#|

idea

first maybe we start by just parsing the directions

then we probably don't have a way to build the full board, we just have trails that lead to a given tile.

we need to find isomorphisms. Maybe we can do something like a distance
metric?

update: distance + angle makes sense to me

make a way to track distance + angle over the course of a given input string
then keep a hash table showing unique end-points, their colors, and whether they have been flipped

|#

;; parsing
(define (get-angle dir)
  (let ([angle-hsh (make-hash '(("ne" 60)
                                ("se" 300)
                                ("sw" 240)
                                ("nw" 120)
                                ("w" 180)
                                ("e" 0)))])
    (first (hash-ref angle-hsh dir))))

(define (parse-directions input)
  (regexp-match* #rx"(se|sw|ne|nw|e|w)" input))

(define (get-location str)
  (let* ([angles (map get-angle (parse-directions str))]
         [distance (length angles)]
         [final-angle (remainder (apply + angles) 360)])
    (list distance final-angle)))

(map get-location tile-map)
