#lang racket
(require racket advent-of-code)

(define mirrors (map (curryr string-split "\n") (string-split (fetch-aoc-input (find-session) 2023 13) "\n\n")))


(define ex1 (string-split "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#." "\n"))

(define ex2 (string-split "
#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#" "\n"))

#|

idea

horizontal

find indices of duplicates
then find mid-point?

|#

;; horizontal
;; get indices
;; (define inter (map (curry indexes-of mirrors) mirrors))

;; (takef inter (λ (v) ((compose not false? (curryr member mirrors)) v)))

(define (get-horizontal-symmetry-coords mirrors)
  (let ([mirror-indices (map (curry indexes-of mirrors) mirrors)])
    (let loop ([mirror-indices mirror-indices]
               [acc '()])
      (match mirror-indices
        ['() (filter (λ (v) (= 2 (length v))) acc)]
        [_ (let* ([f (first mirror-indices)]
                  [is-member ((compose not false? member) f (rest mirror-indices))])
             (if is-member
                 (loop (rest mirror-indices) (cons f acc))
                 (loop (rest mirror-indices) acc)))]))))

;; (get-horizontal-symmetry-coords mirrors)
;; (get-vertical-symmetry-coords mirrors)

;; (get-horizontal-symmetry-coords ex2)
;; (get-vertical-symmetry-coords ex2)

;; (get-horizontal-symmetry-coords ex2)
;; (get-vertical-symmetry-coords (map (compose list->string reverse string->list) (mirror-transpose ex2)))


;; (let loop ([inter inter]
;;            [acc '()])
;;   (match inter
;;     ['() acc]
;;     [_ (let* ([f (first inter)]
;;               [is-member ((compose not false? member) f (rest inter))])
;;          (displayln is-member)
;;          (if is-member
;;              (loop (rest inter) (cons f acc))
;;              (loop (rest inter) acc)))]))


;; vertical
(define (mirror-transpose mirror)
  (define (transpose lst)
    (apply map list lst))
  (map list->string (transpose (map string->list mirror))))

;; (mirror-transpose ex2)

;; '("##.##.#"
;;   "...##.."
;;   "..####."
;;   "..####."
;;   "#..##.."
;;   "##....#"
;;   "..####."
;;   "..####."
;;   "###..##")


(define (get-vertical-symmetry-coords mirror)
  (get-horizontal-symmetry-coords (mirror-transpose mirror)))


;; (mirror-transpose mirrors)

;; '("#.##..#"
;;   "..##..."
;;   "##..###"
;;   "#....#."
;;   ".#..#.#"
;;   ".#..#.#"
;;   "#....#."
;;   "##..###"
;;   "..##...")

;; we can figure out which one is 'correct' by choosing the one with the maximal points of symmetry

(define (config-selector mirror vfn hfn)
  (let ([vert (get-vertical-symmetry-coords mirror)]
        [hori (get-horizontal-symmetry-coords mirror)])
    (if (> (length vert) (length hori))
        (vfn vert)
        (hfn hori))))

(map (curryr config-selector (compose add1 length) (λ (vals) (* 100 ((compose add1 length) vals)))) (list ex1 ex2))

(apply +
       (map
        (curryr config-selector (compose add1 length) (λ (vals) (* 100 ((compose add1 length) vals))))
        (list ex1 ex2)))


("##....##.####"
 ".#...##.###.."
 ".#.#.....#.#."
 ".##..#.#.#.##"
 ".....#.##.#.#"
 ".....#.##.#.#"
 ".##..#.#.#.##"
 ".#.##....#.#."
 ".#...##.###.."
 "##....##.####"
 ".############"
 "..####.###.##"
 "..####.###.##"
 ".############"
 "##....##.####")

(config-selector (last mirrors) identity identity)
(config-selector ex1 identity identity)

;; we need a way to batch things up as a group to signify a reflection
;; then for each group, we need to find a middle and then find the rows above

;; a mirror point is a pair in which (a b) are within 1 of each other
(let ([reflects (config-selector (last mirrors) identity identity)])
  (map (compose add1 first) (filter (λ (p) (equal? (add1 (first p)) (second p))) reflects)))


(define (get-n-distances reflection-values)
  (map (compose add1 first) (curry filter (λ (p) (equal? (add1 (first p)) (second p))) reflection-values)))

(apply +
       (map
        (curryr config-selector
                (λ (vals)
                  (apply + (get-n-distances vals)))
                (λ (vals) (* 100 (apply + (get-n-distances vals)))))
        (list ex1 ex2)))

(apply +
       (map
        (curryr config-selector
                (λ (vals)
                  (apply + (get-n-distances vals)))
                (λ (vals) (* 100 (apply + (get-n-distances vals)))))
        mirrors))


;; idea
;; we want the best line of symmetry
;; AND we want to ignore any extraneous lines
;; maybe instead of taking

(config-selector (last mirrors) identity identity)

("##....##.####"
 ".#...##.###.."
 ".#.#.....#.#."
 ".##..#.#.#.##"
 ".....#.##.#.#"
 ".....#.##.#.#"
 ".##..#.#.#.##"
 ".#.##....#.#."
 ".#...##.###.."
 "##....##.####"
 ".############"
 "..####.###.##"
 "..####.###.##"
 ".############"
 "##....##.####")
