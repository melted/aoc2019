#lang racket/base
(require racket/file)
(require racket/string)
(struct lines (hors verts) #:transparent)

(struct vertical (x y1 y2 steps dir) #:transparent)

(struct horizontal (x1 x2 y steps dir) #:transparent)

(define (parse-data s)
  (define legs (string-split s ","))
  (let loop ((x 0) (y 0) (steps 0) (rem legs) (h '()) (v '()))
    (if (null? rem)
        (lines h v)
        (let ((dir (substring (car rem) 0 1))
              (dist (string->number (substring (car rem) 1))))
          (case dir
            (("U") (loop x (+ y dist) (+ steps dist) (cdr rem) h (cons (vertical x y (+ y dist) steps 1) v)))
            (("D") (loop x (- y dist) (+ steps dist) (cdr rem) h (cons (vertical x (- y dist) y (+ steps dist) -1) v)))
            (("R") (loop (+ x dist) y (+ steps dist) (cdr rem) (cons (horizontal x (+ x dist) y steps 1) h) v))
            (("L") (loop (- x dist) y (+ steps dist) (cdr rem) (cons (horizontal (- x dist) x y (+ steps dist) -1) h) v)))))))

(define data (map parse-data (file->lines "data/input3.txt")))

(define (crossing h v)
  (if (and (<= (vertical-y1 v) (horizontal-y h) (vertical-y2 v))
           (<= (horizontal-x1 h) (vertical-x v) (horizontal-x2 h)))
      (list (+ (abs (vertical-x v)) (abs (horizontal-y h)))
            (+ (+ (vertical-steps v) (* (vertical-dir v) (- (horizontal-y h) (vertical-y1 v))))
               (+ (horizontal-steps h) (* (horizontal-dir h) (- (vertical-x v) (horizontal-x1 h))))))
      #f))

(define (find-crossings hs vs)
  (for*/list ((h hs)
              (v vs)
              #:when (crossing h v))
    (crossing h v)))

(define (solve)
  (define a (car data))
  (define b (cadr data))
  (define crossings (append
         (find-crossings (lines-hors a) (lines-verts b))
         (find-crossings (lines-hors b) (lines-verts a))))
  (displayln (car (sort crossings < #:key car)))
  (displayln (car (sort crossings < #:key cadr))))

(solve) 

