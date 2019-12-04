#lang racket

(define (digits n)
  (let loop ((k n) (acc '()))
    (if (= 0 k)
        acc
        (let-values (((q r) (quotient/remainder k 10)))
          (loop q (cons r acc))))))

(define (runs xs)
  (if (null? xs)
      xs
      (let loop ((acc '()) (k 1) (last (car xs)) (rem (cdr xs)))
        (cond
          ((null? rem) (reverse (cons k acc)))
          ((= (car rem) last) (loop acc (+ k 1) last (cdr rem)))
          (else (loop (cons k acc) 1 (car rem) (cdr rem)))))))

(define (valid n)
  (define d (digits n))
  (and (apply <= d) (not (null? (filter (λ (n) (<= 2 n)) (runs d))))))

(define (valid2 n)
  (define d (digits n))
  (and (apply <= d) (not (null? (filter (λ (n) (= 2 n)) (runs d))))))

(displayln (for/sum ((i (in-range 246540 787420))
                     #:when (valid i))
             1))

(displayln (for/sum ((i (in-range 246540 787420))
                     #:when (valid2 i))
             1))

(module+ test
  (require rackunit)
  (check-true (valid 111111))
  (check-false (valid 223450))
  (check-false (valid 123789))
  (check-true (valid2 112233)))