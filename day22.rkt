#lang racket

(struct cut (n) #:transparent)

(struct deal (n) #:transparent)

(struct stack () #:transparent)

(define (parse str)
  (match str
    ((pregexp #px"deal with increment (\\d+)" (list _ n)) (deal (string->number n)))
    ((pregexp #px"cut (-?\\d+)" (list _ n)) (cut (string->number n)))
    ("deal into new stack" (stack))))

(define steps (map parse (file->lines "data/input22.txt")))

(define deck (for/list ((i (in-range 10007))) i))

(define (perform step deck)
  (define (do-deal incr deck)
    (define v (make-vector (length deck)))
    (for ((i (in-range (length deck)))
          (c deck))
      (vector-set! v (modulo (* incr i) (length deck)) c))
    (vector->list v))
  (match step
    ((stack) (reverse deck))
    ((cut n) (let ((m (if (< n 0) (+ n (length deck)) n)))
               (let-values (((a b) (split-at deck m)))
                 (append b a))))
    ((deal n) (do-deal n deck))))

(define (solve1)
  (let loop ((d deck) (s steps))
    (displayln (index-of d 2019))
    (if (null? s)
        (index-of d 2019)
        (loop (perform (car s) d) (cdr s)))))

(define (exptmod b e m)
  (if (= e 0)
      1
      (let* ((h (exptmod b (quotient e 2) m))
             (r (modulo (* h h) m)))
        (if (odd? e)
            (modulo (* r b) m)
            r))))

(define (solve2)
  (define c 119315717514047)
  (define n 101741582076661)
  (define target 2020)
  (define (inv x)
    (exptmod x (- c 2) c))
  (let loop ((o 0) (i 1) (s steps))
    (if (null? s)
        (let ((o (* o (inv (- 1 i))))
              (i (exptmod i n c)))
          (modulo (+ (* target i) (* o (- 1 i))) c))
        (match (car s)
          ((stack) (loop (- o i) (* i -1) (cdr s)))
          ((deal n) (loop o (* i (inv n)) (cdr s)))
          ((cut n) (loop (+ o (* i n)) i (cdr s)))))))
