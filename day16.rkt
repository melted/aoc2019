#lang racket
(define (digits n)
  (let loop ((k n) (acc '()))
    (if (= 0 k)
        acc
        (let-values (((q r) (quotient/remainder k 10)))
          (loop q (cons r acc))))))

(define start
  (digits
   (string->number
    (string-trim (file->string "data/input16.txt")))))

(define (pattern m)
  (define k (+ m 1))
  (define v (make-vector (* 4 k) 0))
  (for ((i (range m (+ m k))))
    (vector-set! v i 1)
    (vector-set! v (+ i (* 2 k)) -1))
  v)

(define (process xs)
  (for/list ((i (range (length xs))))
    (define p (pattern i))
    (remainder
     (abs (for/sum ((d xs) (j (in-naturals)))
            (* d (vector-ref p (modulo j (vector-length p))))))
     10)))

(define (solve1)
  (let loop ((n 0) (l start))
    (if (= n 100)
        (take l 8)
        (loop (+ n 1) (process l)))))

(define (mega xs) (flatten (for/list ((i (in-range 10000)))
                          xs)))

(define offset 5979673)

(define start2 (drop (mega start) offset))

(define (proc2 xs)
  (let loop ((l (reverse xs)) (sum 0) (out '()))
    (if (null? l)
        out
        (let ((new-sum (+ sum (car l))))
          (loop (cdr l) new-sum (cons (modulo new-sum 10) out))))))

(define (solve2 xs)
  (let loop ((n 0) (l xs))
    (if (= n 100)
        (take l 8)
        (loop (+ n 1) (proc2 l)))))

(define test (drop (mega (list 0 3 0 3 6 7 3 2 5 7 7 2 1 2 9 4 4 0 6 3 4 9 1 5 6 5 4 7 4 6 6 4)) 303673))

(solve1)
(solve2 start2)

