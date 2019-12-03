#lang racket
(define code
  (list->vector
   (map string->number
        (string-split (string-trim (file->string "data/input2.txt")) ","))))

(define (run code)
  (define (do-op f pc)
    (let ((res  (f (vector-ref code (vector-ref code (+ pc 1)))
                   (vector-ref code (vector-ref code (+ pc 2))))))
      (vector-set! code (vector-ref code (+ pc 3)) res)))
  (let exec ((pc 0))
    (let ((op (vector-ref code pc)))
      (if (= op 99)
          code
          (begin
            (case op
              ((1) (do-op + pc))
              ((2) (do-op * pc))
              (else (error "unknown op")))
            (exec (+ pc 4)))))))

(define (execute noun verb)
  (define v (vector-copy code))
  (vector-set! v 1 noun)
  (vector-set! v 2 verb)
  (run v)
  (vector-ref v 0))

(execute 12 2)

(for*/first ((n (in-range 0 100))
            (v (in-range 0 100))
            #:when (= (execute n v) 19690720))
  (+ (* 100 n) v))