#lang racket

(struct vec3 (x y z) #:transparent)

(struct body (pos vel) #:transparent)

(define (get-init)
  (vector
   (body (vec3 1 2 -9) (vec3 0 0 0))
   (body (vec3 -1 -9 -4) (vec3 0 0 0))
   (body (vec3 17 6 8) (vec3 0 0 0))
   (body (vec3 12 4 2) (vec3 0 0 0))))

(define (vec3+ v1 v2)
  (vec3 (+ (vec3-x v1) (vec3-x v2))
        (+ (vec3-y v1) (vec3-y v2))
        (+ (vec3-z v1) (vec3-z v2))))

(define (gravitate v1 v2)
  (vec3 (sgn (- (vec3-x v2) (vec3-x v1)))
        (sgn (- (vec3-y v2) (vec3-y v1)))
        (sgn (- (vec3-z v2) (vec3-z v1)))))

(define (energy v)
  (+ (abs (vec3-x v)) (abs (vec3-y v)) (abs (vec3-z v))))

(define (total-energy b)
  (* (energy (body-pos b)) (energy (body-vel b))))

(define (step system)
  (define vels (for/vector ((i (in-range (vector-length system))))
                 (body-vel (vector-ref system i))))
  (for* ((i (in-range (vector-length system)))
         (j (in-range (vector-length system)))
         #:unless (= i j))
    (define b (vector-ref system i))
    (vector-set! vels i (vec3+ (vector-ref vels i) (gravitate (body-pos b) (body-pos (vector-ref system j))))))
  (for/vector ((i (in-range (vector-length system))))
    (define b (vector-ref system i))
    (body (vec3+ (body-pos b) (vector-ref vels i)) (vector-ref vels i))))

(define (solve1)
  (define init (get-init))
  (define sys
    (for/fold ((sys init))
              ((i (in-range 1000)))
      (step sys)))
  (for/sum ((b sys))
    (total-energy b)))

(solve1)

(define (period pv)
  (define count (/ (vector-length pv) 2))
  (define seen (make-hash))
  (let loop ((state pv) (t 0))
    (define new-state (vector-copy state))
    (define prev (hash-ref seen state #f))
    (if prev
        (begin
          (displayln (format "~a: ~a -> ~a (~a)" state prev t (- t prev)))
          (- t prev))
        (begin
          (for* ((i (in-range count))
                (j (in-range count))
                #:unless (= i j))
            (define old (vector-ref new-state (+ i count)))
            (vector-set! new-state (+ i count) (+  old (sgn (- (vector-ref state  j) (vector-ref state i))))))
          (for ((i (in-range count)))
            (vector-set! new-state i (+ (vector-ref state i) (vector-ref new-state (+ i count)))))
          (hash-set! seen state t)
          (loop new-state (+ t 1))))))

(lcm
  (period (vector 1 -1 17 12 0 0 0 0))
  (period (vector 2 -9 6 4 0 0 0 0))
  (period (vector -9 -4 8 2 0 0 0 0)))