#lang racket

(define data (string-trim (file->string "data/input8.txt")))

(define (cleave str cols)
  (map (λ (n) (substring str n (+ n cols)))
       (range 0 (string-length str) cols)))

(define layers (cleave data 150))

(define (census str)
  (define counts (for/fold ((counts (hash)))
                           ((i (string->list str)))
                   (hash-update counts i (λ (n) (+ n 1)) 0)))
  (list (hash-ref counts #\0)
        (* (hash-ref counts #\1) (hash-ref counts #\2))))

(argmin car (map census layers))

(define (solve2)
  (define (check-layer layer i)
    (case (string-ref (list-ref layers layer) i)
           ((#\2) (check-layer (+ layer 1) i))
           ((#\1) #\#)
           (else #\space)))
  (define output
    (list->string (for/list ((i (in-range 150))) (check-layer 0 i))))
  (for-each displayln (cleave output 25)))

(solve2)
