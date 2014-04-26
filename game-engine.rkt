#lang racket

;; the game engine for 2048, suitable for analysis

;; for now, provide everything:
(provide (all-defined-out))


(define *side* 4)              ; Side-length of the grid

(define (set-side! n)
  (set! *side* n))

;;--------------------------------------------------------------------
;; Rows are represented as lists, with 0s representing empty spots.

;; is a number nonzero?
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

;; The next few functions are used to determine where to place a new
;; number in the grid...
;;

;; How many zeros in the current state?
;;
(define (count-zeros state)
  (length (filter zero? state)))

;; What is the absolute index of the nth zero in lst?
;;
;; E.g. (index-of-nth-zero '(0 2 0 4) 1 2)) 1) -> 2
;;
(define (index-of-nth-zero lst n)
  (cond [(null? lst) #f]
        [(zero? (first lst)) 
         (if (zero? n)
             0
             (add1 (index-of-nth-zero (rest lst) (sub1 n))))]
        [else (add1 (index-of-nth-zero (rest lst) n))]))

;; Place the nth zero in the lst with val.
;;
;; E.g. (replace-nth-zero '(0 2 0 4) 1 2)) -> '(0 2 2 4)
;;
(define (replace-nth-zero lst n val)
  (let ([i (index-of-nth-zero lst n)])
    (append (take lst i) (cons val (drop lst (add1 i))))))

;; There's a 90% chance that a new tile will be a two; 10% a four.
;;
(define (new-tile)
  (if (> (random) 0.9) 4 2))

;; Chop a list into a list of sub-lists of length n. Used to move from
;; a flat representation of the grid into a list of rows.
;; 
;;
(define (chop lst [n *side*])
  (if (<= (length lst) n) 
      (list lst)
      (cons (take lst n) (chop (drop lst n) n)))) 

;; given the flattened representation of a grid,
;; add a random tile to an empty square, return
;; the state and the index and value of the new tile (for use
;; in animation)
(define (add-random-tile state)
  (let* ([replace (random (count-zeros state))]
         [index (index-of-nth-zero state replace)]
         [value (new-tile)]
         [new-state (replace-nth-zero state replace value)])
    (list new-state index value)))


;; Create a random initial game-board with two non-zeros (2 or 4) 
;; and the rest 0s.
;;
;; E.g. '(0 0 0 0  
;;        0 2 0 0  
;;        2 0 0 0  
;;        0 0 0 0)
;;
(define (initial-state [side *side*])
  (shuffle (append (list (new-tile) (new-tile))
                   (make-list (- (sqr side) 2) 0))))



;; The game finishes when no matter which way you slide, the board doesn't
;; change.
;;
(define (finished? state [n *side*])
  (let ([grid (chop state n)])
    (for/and ([op (list left right up down)])
      (equal? grid (op grid)))))

(module+ test
  (require rackunit)
  (set-side! 4)
  
  (check-equal? (slide-left '(0 0 0 0)) '(0 0 0 0))
  (check-equal? (slide-left '(1 2 3 4)) '(1 2 3 4))
  (check-equal? (slide-left '(2 0 4 0)) '(2 4 0 0))
  (check-equal? (slide-left '(0 0 2 4)) '(2 4 0 0))
  (check-equal? (slide-left '(2 0 2 0)) '(4 0 0 0))
  (check-equal? (slide-left '(0 8 8 0)) '(16 0 0 0))
  (check-equal? (slide-left '(4 4 8 8)) '(8 16 0 0))
  (check-equal? (slide-right '(4 4 8 8)) '(0 0 8 16))
  (check-equal? (slide-right '(4 4 4 0)) '(0 0 4 8))
  
  (check-equal? (left '(( 0 8 8 0)
                        (16 0 0 0)
                        ( 2 2 4 4)
                        ( 0 2 2 2)))
                '((16 0 0 0)
                  (16 0 0 0)
                  ( 4 8 0 0)
                  ( 4 2 0 0)))
  (check-equal? (right '(( 0 8 8 0)
                         (16 0 0 0)
                         ( 2 2 4 4)
                         ( 0 2 2 2)))
                '((0 0 0 16)
                  (0 0 0 16)
                  (0 0 4  8)
                  (0 0 2  4)))
  (check-equal? (up '((0 16 2 0) 
                      (8  0 2 2) 
                      (8  0 4 2) 
                      (0  0 4 2)))
                '((16 16 4 4) 
                  (0  0  8 2) 
                  (0  0  0 0) 
                  (0  0  0 0)))
  (check-equal? (down '((0 16 2 0) 
                        (8  0 2 2) 
                        (8  0 4 2) 
                        (0  0 4 2)))
                '((0  0  0 0) 
                  (0  0  0 0) 
                  (0  0  4 2) 
                  (16 16 8 4)))
  
  (check-equal? (left '(( 0 8 8 0)
                        (16 0 0 0)
                        ( 2 2 4 4)
                        ( 0 2 2 2)))
                '((16 0 0 0)
                  (16 0 0 0)
                  ( 4 8 0 0)
                  ( 4 2 0 0)))
  
    (check-equal? (chop '(1 2 3 4 5 6 7 8) 4)
                '((1 2 3 4) (5 6 7 8)))
  
  (check-equal? (length (initial-state 5)) 25)
  
    (let* ([initial (initial-state)]
         [initial-sum (apply + initial)]
         [largest-3 (take (sort initial >) 3)])
    (check-equal? (length initial) 16)
    (check-true (or (= initial-sum 4)
                    (= initial-sum 6)
                    (= initial-sum 8)))
    (check-true (or (equal? largest-3  '(2 2 0))
                    (equal? largest-3  '(4 2 0))
                    (equal? largest-3  '(4 4 0)))))
  
  (check-equal? (count-zeros '(1 0 1 0 0 0 1)) 4)
  (check-equal? (count-zeros '(1 1)) 0)
  (check-equal? (replace-nth-zero '(0 0 0 1 2 0) 2 5)
                '(0 0 5 1 2 0))
  
  (check-true (finished? '(1 2 3 4) 2))
  (check-false (finished? '(2 2 3 4) 2))
  
  (check-equal? (score-increment '((0 2 0 0)
                                   (0 4 4 8)
                                   (8 2 16 16)
                                   (0 0 0 0)))
                (+ 8 32))
  )