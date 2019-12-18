#lang racket

(define data (list->vector (file->lines "data/input18-2.txt")))

(define size-x (string-length (vector-ref data 0)))
(define size-y (vector-length data))

(define (at pos)
  (string-ref (vector-ref data (vec2-y pos)) (vec2-x pos)))
  
(struct vec2 (x y) #:transparent)

(struct state (keys pos steps) #:transparent)

(define directions (vector (vec2 0 -1) (vec2 1 0) (vec2 0 1) (vec2 -1 0)))

(define (vec2+ v1 v2)
  (vec2 (+ (vec2-x v1) (vec2-x v2))
        (+ (vec2-y v1) (vec2-y v2))))

(define (init)
  (define pos
    (for*/vector ((y (in-range size-y))
                  (x (in-range size-x))
                  #:when (char=? (at (vec2 x y)) #\@))
      (vec2 x y)))
  (state (mutable-set) pos 0))

(define (valid-move? st pos)
  (define ch (at pos))
  (or (char=? ch #\.)
      (char-lower-case? ch)
      (set-member? (state-keys st) (char-downcase ch))))

(define (new-key? st)
  (for/or ((i (in-range (vector-length (state-pos st))))) 
    (define ch (at (vector-ref (state-pos st) i)))
    (if (and (char-lower-case? ch) (not (set-member? (state-keys st) ch)))
        (begin
          (set-add! (state-keys st) ch)
          #t)
        #f)))

(define (search st)
  (define seen (mutable-set))
  (define robots (vector-length (state-pos st)))
  (let loop ((next (list st)) (done '()))
    (define new-next
      (for*/list ((s next)
                  (d directions)
                  (i (in-range robots))
                  #:when (let ((pos (vec2+ (vector-ref (state-pos s) i) d)))
                           (and (not (set-member? seen pos))
                                (valid-move? s pos))))
        (let ((pos (vec2+ (vector-ref (state-pos s) i) d)))
          (set-add! seen pos)
          (define new-pos (vector-copy (state-pos s)))
          (vector-set! new-pos i pos)
          (state (set-copy (state-keys s)) new-pos (+ (state-steps s) 1)))))
    (define-values (new-done rest) (partition new-key? new-next))
    (cond
      ((null? rest) (append done new-done))
      (else (loop rest (append done new-done))))))

(define (solve)
  (define (all? st) (= (set-count (state-keys st)) 26))
  (let loop ((l (list (init))))
    (define result (set->list (list->set (flatten (map search l)))))
    (displayln (length result))
    (define-values (done next) (partition all? result))
    (if (not (null? done))
        (take (sort done < #:key state-steps) (min (length done) 10))
        (loop (take (sort next < #:key state-steps) (min (length next) 6000))))))