#lang racket

;; the game engine for 2048, suitable for analysis

;; for now, provide everything:
(provide (all-defined-out))


(define *side* 3)              ; Side-length of the grid

(define (set-side! n)
  (set! *side* n))


;;--------------------------------------------------------------------
;; Rows may be represented as lists, with 0s representing empty spots.
;;

(define (nonzero? x) (not (zero? x)))

;; Append padding to lst to make it n items long
;;
(define (pad-right lst padding n)
  (append lst (make-list (- n (length lst)) padding)))

;; Slide items towards the head of the list, doubling adjacent pairs
;; when no item is a 0.
;;
;; E.g. (combine '(2 2 2 4 4)) -> '(4 2 8)
;;
(define (combine lst)
  (cond [(<= (length lst) 1) lst]
        [(= (first lst) (second lst))
         (cons (* 2 (first lst)) (combine (drop lst 2)))]
        [else (cons (first lst) (combine (rest lst)))]))

;; Total of new elements introduced by combining.
;;
;; E.g. (combine-total '(2 2 2 4 4)) -> 4 + 8 = 12
;;
(define (combine-total lst)
  (cond [(<= (length lst) 1) 0]
        [(= (first lst) (second lst))
         (+ (* 2 (first lst)) (combine-total (drop lst 2)))]
        [else (combine-total (rest lst))]))

;; Slide towards the head of the list, doubling pairs, 0 are
;; allowed (and slid through), and length is preserved by
;; padding with 0s.
;;
;; E.g. (slide-left '(2 2 2 0 4 4)) -> '(4 2 8 0 0 0)
;;
(define (slide-left row)
  (pad-right (combine (filter nonzero? row)) 0 (length row)))

;; Slide towards the tail of the list:
;;
;; E.g. (slide-right '(2 2 0 0 4 4)) -> '(0 0 0 0 0 4 8)
;;
(define (slide-right row) (reverse (slide-left (reverse row))))


;;--------------------------------------------------------------------
;; We use a sparse representation for transitions in a row.
;;
;; Moves take the form '(value initial-position final-position) 
;;
(define (moves-row-left row [last #f] [i 0] [j -1])
  (if (null? row) 
      null
      (let ([head (first row)])
        (cond [(zero? head) (moves-row-left (rest row) last (add1 i) j)]
              [(equal? last head) 
               (cons (list head i j)
                     (moves-row-left (rest row) #f (add1 i) j))]
              [else (cons (list head i (add1 j))
                          (moves-row-left (rest row) head (add1 i) (add1 j)))]))))

;; Convert a row into the sparse representaiton without any sliding.
;;
;; E.g. (moves-row-none '(0 2 0 4)) -> '((2 1 1) (4 3 3))
;;
(define (moves-row-none row)
  (for/list ([value row]
             [i (in-naturals)]
             #:when (nonzero? value))
    (list value i i)))

;; Reverse all moves so that:
;;
;; '(value initial final) -> '(value (- n initial 1) (- n final 1)
;;
(define (reverse-moves moves n)
  (define (flip i) (- n i 1))
  (map (λ (m)
         (match-define (list a b c) m)
         (list a (flip b) (flip c)))
       moves))



(define (transpose-moves moves)
  (for/list ([m moves])
    (match-define (list v (list a b) (list c d)) m)
    (list v (list b a) (list d c))))

(define (moves-row-right row [n *side*])
  (reverse-moves (moves-row-left (reverse row)) n))

;;--------------------------------------------------------------------
;; Lift the sparse representation for transitions
;; up to two dimensions...
;;
;; '(value initial final) -> '(value (x initial) (x final))
;;
(define (add-row-coord i rows)
  (for/list ([r rows])
    (match-define (list a b c) r)
    (list a (list i b) (list i c))))

(define (transpose lsts)
  (apply map list lsts))

;; Slide the entire grid in the specified direction
;;
(define (left grid)
  (map slide-left grid))

(define (right grid)
  (map slide-right grid))

(define (up grid)
  ((compose transpose left transpose) grid))

(define (down grid)
  ((compose transpose right transpose) grid))

;; Calculate the change to score from sliding the grid left or right.
;;
(define (score-increment grid)
  (apply + (map (λ (row) 
                  (combine-total (filter nonzero? row)))
                grid)))
