#lang racket
(require racket advent-of-code)

(define (prep-input in)
  (map (compose (curry filter (λ (v) (not (zero? (string-length v)))))
                (curryr string-split ""))
       (string-split in "\n")))

(define platform (prep-input (fetch-aoc-input (find-session) 2023 14)))

(define platform
  (prep-input
   "..O....O....O..#O#OOOO...O..O...#.O.#.OO...O.##O#.#..#...OOO.#....O#....#...OO..O.OO..#.OOO.O..O.O.O
.OO.##....#.#..#O...........##O..#..O..O.....#.O.##.O.O............#......##........#.OO..#OO..#....
##........O.O.#O...##.#...#.#.O...#..#O...#....#O...OOOO..OO.OOO.O...O.#........OO.O.O....O...O##O..
...O..O#..O.O..O.O..OO..#.....O..OO....O#..O.##..O..#O.#.OO....####.OOO.OO..O.#..OO.O....O...#.#.O..
...#.O.OOO..#O...O#...O....O...#.O.OO.O.O...O...#...#.#O...#....#....#..O....#..O..#...#..O.....O...
.OO...#...#O#....O.O#.O#.O#.OO.OO.O.O#..........O#....O........O.O.....#...O.#....#........#........
.#.#.#.OOO....#.O.#....#.#..##O.........O.#OO.OO..OO#.#....##.#.##O#.#....O.#.........#...O.#O.O....
..O.#.#.O.....O....O.#..#..O#.#OO..OO........#O.#OO.O..OO.O.......#...#.#..##...........O...#.O.#.#.
...#....#...O....##O......O...#...#....O.#...OOO#..#..O....##.OO.......#O.#.....OO....#.###O#O#.....
O..#O..O..OO.OO.O........O.##O.O...O.O##O..#....#.#..O.OO.#.....O.#......O..O#..#.O.#O.#...#O....O..
OO...O.......#........O#.#.....#O.O...O.O........O.##O......OO.....#.....O#...O#.#..#...#.#O..#.....
O.....O..O..O...#..O..#O#.O..O...O....O#..O...#O.....O.OO...O....O..#...O....#O..#....O..O.##....OO.
.#.#..O........O...#.......O..##.O.###.O..O.....O.##O...OO....O..#..#O....O..O.O..........O...OO.O..
....O..O..O........#O.#.##...##....#.#.O...#O#.O....#.O#O#O....#.O#......O##..#.O.O....#O#..OO.#..#.
##....O...O###.O.....O#.OOO.O##.##....O.....O...O..O...O..#...O....##.....O........##...#.O...##O...
...O....###......#...O.OO........#.#.....OOO........O.#......O.O#....O.O.....#..##..O..#.#O#..O..O#O
.O..#.##O#...O.OO#...#OO.#..##.##..O.#.#O.O.#...OOOO...#.........O...........#..O.......#.##.O...##.
.OOO#.......OO....O..#..#O.O.O#.#O..#....O....#.O.O..O#..#...#.O.#O.#.OO.OO..#.#O.#.O#...#O.....#..#
...#O...O.O..#.O........O#.#....O.######O#...O.#.O.#..O.....#.....#...O..............#...O....##..#.
.O............O#......#O##O..OO......#....OO#..#..#..#OOO...#..#.....O#OO..O.O..#.O#..OO.##...O#.#..
.O...OOO.O.#........#..#..O.O.......O...#...#OO#O.....O...O......O...OO.O#.O.O#.OO.#......O.O....#O.
O.##O.....OO....O..##..O....#......O...O...O....####..#.....O.....O..O.#.O..#OO.#..#..O#O..O...#...O
..O...#O#...#.#..O...O#.O......O.#..#...#..#O..#...#..O.....O...OO.O#....OO.......#..###.OO..#..OOO.
.#..##O..O....O...O....O..O###.O.O......O#OO#O#O#O#...#...##.O.......#OOO#.##.#.O.....O.O.OO..#...O.
.O...#..O...O..O....#....O.....#O...#..O..#..#.......O...#..#....O.O#O....#....#O...OO...#.O.....OO#
.#.O..O............#...#...O..........O...O##......O.#....#O..O.O#...O##O.O..#....OOO..#...OOO....O.
O#O#.....O...OO##.....O#...O.....OO#.#...OO.O.O#.O.O.......#..##......O.#O.....#.O..#...#O...#...O..
..O##..#....O#.......OO#O.#......OO.##.#O.O.O....O.O..#O.#.....#..O......#O#....#O#.....#........#..
.O.##.#...O....#O........OOO.#O.....#.#....O..O..###....#.#..#.#O#.#.....#..#..#O##..#..OO.##.O....O
.O..O.O.....O.O...O#.#...##O....O.O.O...#....#O....#.O..OO...O.#..#O#........#OO....#...#.#..O..O...
...##O.O...O##....#..O...O#...#......#.OO##.....#..#.....O....#......O#.#O..O.#.O.O..O....O..O.O###O
...#....O.O...O..#.O....#...##O..O..O..#...O..#OO.#..O..#OO..O.........#O....#.#..O#O.O.#.O.....##O.
.......#.O.O.O..O##.............OO...O#O.O#.O..OO#.......O#...OOO.....#...O.......#.#..O#..O..OO#...
O...OO#.O..O.....OO......#.O#...#O#O.O......#.#O.....#..O.#.O#..#O.O....O.......O.....OO..O#....#.O.
..#...O#.O.O.O.#.O......O..#..O....#...#..O.O.#...#.....O..O....OO#OO.O.O.O..........#O..O#.O#..O...
......#O.....O.....OO.O..O#..#O###..O..#OO.#OO.....O.OOO..#.O...O....#O..#OO.#O...O.O....O#.....O.##
O.....O.O..#....O....##.....#.......OO#O.#...O..##...#....#.O#O..OO#.##.#.O.O.O.O#...#.OO.#O..OOOO#.
...OOO.#..#OO.O...#.....O#......#..O.O..OO#.O.O#.#.#........#.O.....#..O..O..#O..#O.#..#....#..#.O..
..O..O.....#.##.OO#.......O..OOO....OOO...#.O.#O...O...OO....O#OO...O.##....O.....#..O......#......O
.#..O..OO#O..#.##.O.O...##OOO.#...O......#O...O..#..#..#..O........#...O..#..#.OOO#.O.O......#.##O.#
#..#...O..#.O.#.....O..#.OO.#.....O.....#....O.OO.#O..#.#.#O..O....O.OO..O.......#O##..#O.#.OO...O..
O...O....OO..#OOOOO##.......##....#........O..O......O#O.#.......O.#.....OO........###..#........#.O
O.....O.......#OO.O.OO..O.O##.....O....O##......OO...O#O...O.....O.O....OOO#.O....#O.##...#.#..O##..
..O...#.....##.#O...#.OO.O#......O...O....OO..O.##O..OO#.O.#.O.O..#..#.O.O..O...O..#.OO.O.O....O....
O.OO.###.O..OO..##O....#.O......O.....O..OO......O.O.#OO...OO.#.....O#..O..#O.O........O.OO#O.OO....
O.....OO.....#O..O...O.#.OO#O.O##O...OO#..##O#.O.O#OO....#.#..#...##.#...#..O.O....O..O....#O.#.O...
...#.......OO.O..O......O.O.#....#..O.#.###.#......#.#O......#.#.O..O...O.#..#...#OO...O#.O....O##.O
....O.#...O..#.....O.#.OO..#...O.O....O....#..#...#..........O.O..#...O#...O#.#OOO.O.O.....O#O.....O
.....O...O.O...#.#....#....##.OO..#..O.O.OO...O#.O.....#..#...O...O......#.#....O..O......OO.O.O...#
.O.#..#.O..O...#.O.....OOO.O.#.OOO..O.........#.OO#....#.O.....#O.....#..#...O......#.#O.O...O.OOO.O
O#OO...O..#OO.OOO##..O........###..O....O#.O...O....O..#..........#...OO#O.....O.OO.O..#....O.#..#..
............O.O..O#...O.#......O....#.............O......#.O..O...O.#OO...#.....O....#.O#...#OO..O.#
.O.#.#O..#O...O.O..O#.O...##.#.#...O#...#..#.#OO#....#O....#......O.#O..O#O#.O....O.......#....#....
#.##...#.O.......#.....OO....#O#..O#.....O.#....O.#O#..O..##.#....O.......#....#.O#.#..#..O#.#..O.O.
..O..O#..O.O....O..#....O#.O##O..O..#.#OOO#O.O.##.O##O..........#.O.O#..#O.O.OO.#..OO...###O..#.O.##
.......O..........##..#.#.......#.O.O..O......O.O.OO.O............OO.O.O#..##.#......##O.O#O..O.....
....O......#..O..O..##O..O#.O....O..#.O.#...O.#O.O.O.O.#.#..O.....#..O.#..O#............O..#.O...#O#
..O...O..#O....O.#O..O.#.O.O..O..OO##.....##OO..#..#.O#O....O...#.....O#.O.#.......#.....O#..O..O.#.
.O..#...O..#.....O.#O#O....OOO..#..#.O..O.O.#......O.#OO#O..#.O#..#....O..#.O.O#......O#..##...OO..#
##.#...##.O...OOO....##O.O..OO#OO.O#...O.OO........#......OO...#.#..O#.#........O....OOO..O..O..O.#.
#.OOO....O......O....#.#.OO..#.....O...O.OO..O..OO.....OO........OO....O.O..#.O#.....O......#.#.#O..
...O........O..O.#O...#......O...O...O.....#.O....O.........#...OO....##...O.#.O.....O...O.......#O.
..O#.#O.OO#...O.O..O..OOO..........O.....#O....O......#..OO...O..O#O.#....#.O..........#O.OOO...##..
#..O#...OO#.O....O#......##.#O....O..OO.OO..O....#.......O.....O..O......O#O.....O..O...#....#....O.
.#....O............O.O.OO...O##O......#....O#.........OO.O#O............O##O.#....#O.O###.#O..#.....
O#.....O.O..O..#.....#.#....#......#...O.OO.#OO....#..##.O..O#O...OO..#...O.#.#.O..##.#....OO.O.....
#.....O......O#.#........#....O..#O.#O.OO..O.OO.O..O##.#.#.#OO....O.#....O.O..#.....#.....O.OO...OOO
..OO.##OO#O.............O.OO...O#.##...#.#O...#O.....#...#.......#OOO.#.O#OO.....#..O#..#...OO....#.
.#....OO.#.O##...#.O#.....OO#O...OOO..O##....OO.#.#.#......O#...O#OO....#......OOOOO#O...#..#.....O#
##O.O..O.##......#.OO...#...O..#O........##.O.......O#......#..#O....O....O#O.O..##.........O.......
..O.#...#OO#O....#O.....O...##.O....#..##O#........O..#..#..#...O..#.OO.#O#...O......#....#.......##
.O.###......#........##.....#...#...#O.O#O..........#..O.....O.OOO#O.....#...#.OO#O.O##O.OOO........
.#..O.......O....O...#.........#O##.OOO#O..#.#...O....O#O..O....#..O#..#....#O#......O#O#..#.....#.#
.###O.O..OO.O..O....O#.##O.O..O..O..#O#.#O#.O....O..O#..O..####.O...#..O..#OO....OO.......#..#.O..#.
O...O#...#.O....OO..##..O..O#O#.#OOO...OO...#..#....#O..#.#....#O...OO.O...O.#.O..#.#..O.O..O#.O#.#.
#.O..#..O##O..OOO.O...O.OO#.#...OO.OO.......O..#.O#OO....O.#O...........#...O..O...###O#..##..#.O.#.
.O.O.#.....O#...OO....#.....#.OOO.#..#O.#O.#O...#..O.....##..O#............O....O#...#.O#.....#OO.#.
##O.O...OO.O.#.#O..O....#.#.O............#O..O.....O..#..#.#.O..#.......#.O.........O.OO...#..#####.
.O##..####...#...........#.O..#..O#..OO........O..O...#O.##OO..#..O.O.O..#..##.O.OO#O#...#....#O.O.O
O.....O.O#....O#..#...O....OOO.OO..#..#......O...OOO.....O...O#..O..........####............#.O...OO
O#..O...#......#...#....#.#.O.........O...O....##O......#...O#.O.OO....#O.......#.O.O.OO.#O.#O...#OO
.O.....O...#.#..O#...O#.OO##O...#..#....#.O.O....O......#.O...##..O.##.....O#.O#..OO#.#.........#...
O....O.#O......O#O..OO#.#..#.OOO...#.#...#..#..OO..#........#.O..O..O##..OO#.#.O#OO.....#.O.##....O.
.OO............O....OO..#.#...O.#..O.......O.O......#....O..O..O#.......#.....OOO##...#.....O...O.#.
.#...O..OO.O....O...#.#...#OO#.O...#...##.O.......O.......#.....O..##O..O..OO..O#.OO..O.O...O.#...#.
..OO#.#O..#OO..O#O#O.O....O..O.......OO##.O..#.#O....O.#.....O..O.O....O##.#....#.#....O..#..#..OOO.
....#..O#..#...##.....#.#O.O......#....O##............#.....##O...#..O..##.#O..#O...O....O...##OO...
#O...O......O.O.#.OOO.....O##...#..O..OO...#..O#...O....O.O....#..O#...OOO..#.O..O....#..##.#.O.O...
##..#O#.O.#O...O#....OO...#..#.O##.O...#.O.#.O.O....O.#...O#.....####....OO.O..#.OO#....O...#..#....
O#..#...##.#.###.O##..#..##OO...#O#.........O.....O.OO#..OO..O....#.O#..........#...###.#......O..#.
O..#..##.O.O....OO.O#.##.O..O.#.#..#..O..OO..O...#.O.#....O#.....#OO...O.O#OO.O#.......OO#..#.O...O.
O..#..O#.#....#.O....O...#.OO.....O.O...O.#.###.#.O..O.O.#.....O.#......O#.....O...O......O.#..#...O
##O..#.##...#.......#.#..#.O....#.......O...O.##.O..#.O...O..##.O..#..O....O#....O.O#..O.......O.O.O
#..O...O....OO..O...O.OO....#..#..#.O#....#.#..O##.#..#.#.OOO#.#.O..#..O##......O.....OO....O#.#...O
#O.....O........##OO#..OO..O..O.#...O#......O..O..OO..OO....##.....#OOO.#OO.......O....O#.O...##..#.
.O..OO.O.....OOO...##O....##O....#.#.#O...OO.O...#..#......O.O.O..O.O..##....O....O.O.##....OO.#O.O.
..........O..O#...#...#..#..O..O...O#..O#OOO#......OOO#O..O.O.#O...O#..#O.OO..O##..O##O...O.O#.....#
#...#O..O.......O.O##.OO.#.#O..O.....O...#..O..O......O...#....#..O..O....##.....#.#...O.........#..
#....O.....O..O......#..#..........O..OO.#..##.....#..O...##....O.O..#.#O.#....OO.#....#.O...O...#O.
.......#..#.O..#....O......OOO.#....#O#.O..O..O#O#.##.#.#.#.............#O##OO.#.#O.#....O.O.#......"))

(define platform (prep-input
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."))


;; get column-above
(define (column-above lol x)
  (map (curryr list-ref x) lol))

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

#|

we have shift-rocks
this takes a row and pushes everything to the left

|#

;; shift east
;; (reverse (shift-rocks (reverse '("O" "." "." "." "O" "#" "." "." "O" "."))))


;; tilt south
;; (shift-rocks '("O" "." "." "." "O" "#" "." "." "O" "."))




(define (transpose lol)
  (apply map list lol))

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
;; (map string-join
(define 1bn 1,000,000,000)


;; we know there is a cycle every 78 iterations
;; and when we do 1bn mod 78 we get 64
(let ([cycle-num (modulo 1bn 78)])
  (let loop ([num-iters 78]
             [shifted-platform platform])
    (cond [(zero? num-iters) (north-beam-load shifted-platform)]
          [else
           (begin
             (let ([inum (- 78 num-iters)])
             (when (zero? (modulo inum 10))
               (displayln (format "iteration: ~a" inum))))
           (loop
            (sub1 num-iters)
            ((compose tilt-east tilt-south tilt-west tilt-north)
             shifted-platform)))])))





;; not brute force
;; we have n -> w -> s -> e
;; how can we reduce this?
;; maybe cycle finding algorithm and keep track of the iter

(map string-join
 (let loop ([num-iters 64]
            [shifted-platform platform]
            [history '()])
   (cond [(zero? num-iters)
          (north-beam-load shifted-platform)]
        [else
         (begin
           (let ([inum (- 1000 num-iters)])
             (when (zero? (modulo inum 10))
               (displayln (format "iteration: ~a" inum))))
           (let ([round ((compose tilt-east tilt-south tilt-west tilt-north)
                         shifted-platform)])
             (when (member round history)
               (begin
                 (displayln (format "found cycle! ~a ~a" num-iters (north-beam-load shifted-platform)))
                 (set! history '())))
             (loop
              (sub1 num-iters)
              round
              (cons round history))))])))

;; 96475
;; 827 749 671 593 515 437 359 281 203 125 47
(define test-res '(827 749 671 593 515 437 359 281 203 125 47))

(require (only-in srfi/1 map))

(map (λ (a b) (- a b)) test-res (rest test-res))
