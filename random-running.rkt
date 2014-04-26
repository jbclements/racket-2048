#lang racket

;; random running of game engine

(require "game-engine.rkt"
         plot)

;; transpose if the motion is not left or right
(define (maybe-transpose motion grid)
  (cond [(or (equal? motion left)
             (equal? motion right))
         grid]
        [else (transpose grid)]))

;; run a random trial, return the board and number of moves and score
(define (trial)
  (define i (chop (initial-state)))
  
  (let outer-loop ([board i] [moves-taken 0] [running-score 0])
    (define to-try (shuffle (list up down left right)))
    (let loop ([to-try to-try])
      (cond [(empty? to-try)
             (list 'game-over board moves-taken running-score)]
            [else (define movement (first to-try))
                  (define maybe-moved ((first to-try) board))
                  (cond [(equal? maybe-moved board)
                         (loop (rest to-try))]
                        [else 
                         (define added 
                           (chop (first (add-random-tile 
                                         (apply append maybe-moved)))))
                         (outer-loop
                          added
                          (add1 moves-taken)
                          (+ running-score
                             (score-increment 
                              (maybe-transpose movement board))))])]))))

(define trials 10000)

(define data 
  (for/list ([i trials])
    (trial)))

;; overall score density
(plot (density (map third data)))

(define (max-value board)
  (foldl max 0 (apply append board)))

(define max-vals 
  (sort (remove-duplicates (map max-value (map second data))) <))

(plot
 (map density
      (for/list ([mv max-vals])
        (map third
             (filter (lambda (point) (= (max-value (second point)) mv)) data))))
 #:y-max 0.2)

(plot (density (map fourth data)))