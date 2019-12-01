#lang racket
(define data (map string->number (file->lines "data/input1.txt")))

(define (fuel x) (- (floor (/ x 3)) 2))

(define (rfuel x)
  (let ((bf (fuel x)))
    (if (> bf 0)
        (+ bf (rfuel bf))
        0)))

(define (solve ffn xs)
  (foldl + 0 (map ffn xs)))

(displayln (solve fuel data))
(displayln (solve rfuel data))