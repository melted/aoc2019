#lang racket

(define data (list->vector (file->lines "data/input10.txt")))

(define size-x (string-length (vector-ref data 0)))
(define size-y (vector-length data))

(define (at? x y)
  (char=? (string-ref (vector-ref data y) x) #\#))

(define (pos x y) (+ (* size-x y) x))

(define asteroids
  (for*/set ((j (range size-y))
             (i (range size-x))
             #:when (at? i j))
    (pos i j)))

(define (out-of-bounds? x y)
  (not (and (<= 0 x (- size-x 1)) (<= 0 y (- size-y 1)))))

(define (occluded xy)
  (define-values (y x) (quotient/remainder xy size-x))
  (define (do-one bx by)
    (define dx (- bx x))
    (define dy (- by y))
    (define common
      (cond
        ((= dx 0) (abs dy))
        ((= dy 0) (abs dx))
        (else (gcd dx dy))))
    (define sx (/ dx common))
    (define sy (/ dy common))
    (for/set ((i (in-naturals 1))
              #:break (out-of-bounds? (+ bx (* i sx)) (+ by (* i sy))))
      (pos (+ bx (* i sx)) (+ by (* i sy)))))
  (let loop ((remaining (set->list (set-remove asteroids xy))) (hidden (set)))
    (cond
      ((null? remaining) (set-intersect asteroids hidden))
      ((= (car remaining) xy) (loop (cdr remaining) hidden))
      (else
       (let-values (((by bx) (quotient/remainder (car remaining) size-x)))
         (loop (cdr remaining) (set-union hidden (do-one bx by))))))))

(define station
  (argmax cadr (set->list (set-map asteroids
                                   (λ (s) (list s (- (set-count asteroids)
                                                     (set-count (occluded s))
                                                     1)))))))
(displayln (cadr station))

(define (shooting xy)
  (define targets (set-subtract (set-remove asteroids xy) (occluded xy)))
  (define-values (y x) (quotient/remainder xy size-x))
  (define (angle bxy)
    (define-values (by bx) (quotient/remainder bxy size-x))
    (define dx (- bx x))
    (define dy (- by y))
    (if (or (= 0 dy) (= 0 dx))
        (if (= dx 0)
            (if (> dy 0) pi 0)
            (if (> dx 0) (* 0.5 pi) (* 1.5 pi)))
        (cond
          ((and (> dx 0) (< dy 0)) (atan (/ dx (- dy))))
          ((and (> dx 0) (> dy 0)) (+ (atan (/ dy dx )) (* pi 0.5)))
          ((and (< dx 0) (> dy 0)) (+ (atan (/ (- dx) dy)) pi))
          (else (+ (atan (/ (- dy) (- dx))) (* pi 1.5 ))))))
  (map (λ (x) (list (angle x) x (+ (* 100 (remainder x size-x)) (quotient x size-x)))) (set->list targets)))

(define targets (sort (shooting (car station)) < #:key car))

(displayln (caddr (list-ref targets 199)))

