#lang racket

(define (make-code str)
  (list->vector (map string->number
                     (string-split (string-trim str) ","))))

(define get-code
  (let ((code (make-code (file->string "data/input5.txt"))))
    (lambda () (vector-copy code))))

(define (magn n k)
  (= 1 (remainder (quotient n k) 10)))

(define (run code input)
  (let exec ((pc 0) (output '()))
    (define bop (vector-ref code pc))
    (define op (remainder bop 100))
    (define (get n)
      (let ((val (vector-ref code (+ pc n))))
        (if (magn bop (expt 10 (+ n 1)))
            val
            (vector-ref code val))))
    (define (set n x)
      (vector-set! code (vector-ref code (+ pc n)) x))
    (define (jump cd)
      (if (cd (= (get 1) 0))
          (exec (get 2) output)
          (exec (+ pc 3) output)))
    (define (test op)
      (set 3 (if (op (get 1) (get 2)) 1 0))
      (exec (+ pc 4) output))
    (define (do-op f)
      (set 3 (f (get 1) (get 2)))
      (exec (+ pc 4) output))
    (case op
      ((1) (do-op +))
      ((2) (do-op *))
      ((3) (set 1 input)
           (exec (+ pc 2) output))
      ((4) (exec (+ pc 2) (cons (get 1) output)))
      ((5) (jump not))
      ((6) (jump (λ (x) x)))
      ((7) (test <))
      ((8) (test =))
      ((99) (reverse output))
      (else (error (format "unknown op ~a" bop))))))

(run (get-code) 1)
(run (get-code) 5)
