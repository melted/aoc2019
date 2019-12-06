#lang racket

(define (get-data)
  (define data (make-hash))
  (define orbits (make-hash))
  (define lines (file->lines "data/input6.txt"))
  (define (insert str)
    (define parts (string-split str ")"))
    (hash-set! data (car parts) (cons (cadr parts) (hash-ref data (car parts) '())))
    (hash-set! orbits (cadr parts) (car parts)))
  (for-each insert lines)
  (list data orbits))

(define (solve1)
  (define data (car (get-data)))
  (let loop ((acc 0) (level 0) (next '("COM")))
    (if (null? next)
        acc
        (loop (+ acc (* level (length next)))
              (+ level 1)
              (flatten (map (λ (v) (hash-ref data v '())) next))))))

(solve1)

(define (solve2)
  (define orbits (cadr (get-data)))
  (define (all-orbits str)
    (let loop ((next str) (acc '()))
      (let ((p (hash-ref orbits next #f)))
        (if p
            (loop p (cons next acc))
            (cons next acc)))))
  (define mine (all-orbits "YOU"))
  (define santas (all-orbits "SAN"))
  (let-values (((m s) (drop-common-prefix mine santas)))
    (- (+ (length m) (length s)) 2)))

(solve2)
  