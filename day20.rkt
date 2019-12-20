#lang racket

(define data (list->vector (file->lines "data/input20-fixed.txt")))

(define size-x (string-length (vector-ref data 0)))
(define size-y (vector-length data))

(struct vec2 (x y lv) #:transparent)

(define (flat pos)
  (vec2 (vec2-x pos) (vec2-y pos) 0))

(define (at pos)
  (string-ref (vector-ref data (vec2-y pos)) (vec2-x pos)))

(define (vec2+ v1 v2)
  (vec2 (+ (vec2-x v1) (vec2-x v2))
        (+ (vec2-y v1) (vec2-y v2))
        (vec2-lv v1)))

(define (outer-wall? p)
  (or (= (vec2-y p) 106) (= (vec2-y p) 2)
      (= (vec2-x p) 106) (= (vec2-x p) 2)))

(define (valid-move? pos)
  (define ch (at pos))
  (or (char=? ch #\.) (char-lower-case? ch)
      (and (= 0 (vec2-lv pos)) (char=? ch #\1))))

(define directions (vector (vec2 0 -1 0) (vec2 1 0 0) (vec2 0 1 0) (vec2 -1 0 0)))

(define (init)
  (define start #f)
  (define goal #f)
  (define points (make-hash))
  (define portals (make-hash))
  (for* ((x (in-range size-x))
         (y (in-range size-y)))
    (define ch (at (vec2 x y 0)))
    (cond
      ((char-lower-case? ch)
       (let ((pos (hash-ref points ch #f))
             (here (vec2 x y 0)))
         (if pos
             (begin
               (hash-set! portals pos here)
               (hash-set! portals here pos))
             (hash-set! points ch here))))
      ((char=? ch #\0) (set! start (vec2 x y 0)))
      ((char=? ch #\1) (set! goal (vec2 x y 0)))))
  (values start goal portals))

(define-values (start goal portals) (init))

(define (moves pos)
  (define ds (filter valid-move?
                     (for/list ((d directions))
                       (vec2+ pos d))))
  (define ps (let ((port (hash-ref portals (flat pos) #f)))
               (if (and port (or (< 0 (vec2-lv pos)) (not (outer-wall? pos))))
                   (list (vec2 (vec2-x port)
                               (vec2-y port)
                               (+ (vec2-lv pos) (if (outer-wall? pos) -1 1))))
                   '())))
  (append ds ps))

(define (search)
  (define seen (mutable-set))
  (let loop ((next (list start)) (n 0))
    (cond
      ((null? next) (error "stuck"))
      ((ormap (λ (p) (equal? goal p)) next) n)
      (else (let ((new-next (filter (λ (x) (not (set-member? seen x))) (flatten (map moves next)))))
              (for-each (λ (x) (set-add! seen x)) next)
              (loop new-next (+ n 1)))))))

