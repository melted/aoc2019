#lang racket

(define (make-code str)
  (define vals (map string->number
                    (string-split (string-trim str) ",")))
  (for/hash ((i (range (length vals)))
             (v vals))
    (values i v)))

(define get-code
  (let ((code (make-code (file->string "data/input25.txt"))))
    (lambda () (hash-copy code))))

(struct vec2 (x y) #:transparent)

(struct state (pc mem runlevel base n pos output input) #:mutable #:transparent)

(define directions (vector (vec2 0 -1) (vec2 0 1) (vec2 -1 0) (vec2 1 0)))

(define (vec2+ v1 v2)
  (vec2 (+ (vec2-x v1) (vec2-x v2))
        (+ (vec2-y v1) (vec2-y v2))))


(define (process-input input)
  (map char->integer (string->list input)))

(define (run st)
  (define code (state-mem st))
  (define (exec pc input)
    (define bop (hash-ref code pc))
    (define op (remainder bop 100))
    (define (mode n)
      (remainder (quotient bop (expt 10 (+ n 1))) 10))
    (define (get n)
      (let ((val (hash-ref code (+ pc n) 0)))
        (case (mode n)
          ((0) (hash-ref code val 0)) 
          ((1) val)
          ((2) (hash-ref code (+ val (state-base st)) 0)))))
    (define (set n x)
      (define addr (hash-ref code (+ pc n)))
      (case (mode n)
        ((0) (hash-set! code addr x))
        ((1) (error "can't set immediate"))
        ((2) (hash-set! code (+ addr (state-base st)) x))))
    (define (jump cd)
      (if (cd (= (get 1) 0))
          (exec (get 2) input)
          (exec (+ pc 3) input)))
    (define (test op)
      (set 3 (if (op (get 1) (get 2)) 1 0))
      (exec (+ pc 4) input))
    (define (do-op f)
      (set 3 (f (get 1) (get 2)))
      (exec (+ pc 4) input))
    (define (handle-input)
      (when (null? input)
        (set! input (append (process-input (read-line)) (list 10))))
      (set 1 (car input))
      (exec (+ pc 2) (cdr input)))
    (define (handle-output)
      (define op (get 1))
      (display (integer->char op))
      (exec (+ pc 2) input))
    (set-state-pc! st pc)
    (case op
      ((1) (do-op +))
      ((2) (do-op *))
      ((3) (handle-input))
      ((4) (handle-output))
      ((5) (jump not))
      ((6) (jump (λ (x) x)))
      ((7) (test <))
      ((8) (test =))
      ((9) (set-state-base! st (+ (state-base st) (get 1)))
           (exec (+ pc 2) input))
      ((99) (state pc code 'halted (state-base st) (state-n st)
                   (state-pos st) (state-output st) input))
      (else (error (format "unknown op ~a" bop)))))
  (exec (state-pc st) (state-input st)))

(define (new-state code)
  (state 0 code 'ready 0 0 (vec2 0 0) '() '()))
  
(define (render state)
  (define str (list->string (reverse (state-output state))))
  (display str))


;; (render (run (new-state (get-code) '())))


(define (solve1)
  (define st (new-state (get-code)))
  (let ((out (run st)))
    '()))
  