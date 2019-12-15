#lang racket

(struct component (amount chemical) #:transparent)

(struct reaction (in out) #:transparent)

(define (parse-reactions str)
  (define (parse-component str)
    (let ((parts (string-split str)))
      (component (string->number (car parts)) (cadr parts))))
  (define in-out (string-split str "=>"))
  (reaction (map parse-component (map string-trim (string-split (car in-out) ",")))
            (parse-component (string-trim (cadr in-out)))))

(define reactions (map parse-reactions (file->lines "data/input14.txt")))

(define (order-reactions xs)
  (define (possible? on-hand rx)
    (define in-rx (list->set (map component-chemical (reaction-in rx))))
    (set-empty? (set-subtract in-rx on-hand)))
  (define (add-on-hand on-hand rxs)
    (set-union on-hand (list->set (map (λ (rx) (component-chemical (reaction-out rx))) rxs))))
  (let loop ((on-hand (set "ORE")) (selected '()) (left xs))
    (if (null? left)
        selected
        (let-values (((possible rest) (partition (λ (x) (possible? on-hand x)) left)))
          (loop (add-on-hand on-hand possible) (append selected possible) rest)))))

(define (calculate-needed reactions x)
  (define ordered (reverse (order-reactions reactions)))
  (define needed (make-hash))
  (hash-set! needed "FUEL" x)
  (for ((rx ordered))
    (define amt (hash-ref needed (component-chemical (reaction-out rx))))
    (define prod (component-amount (reaction-out rx)))
    (define n (ceiling (/ amt prod)))
    (for ((output (reaction-in rx)))
      (hash-update! needed (component-chemical output) (λ (x) (+ x (* n (component-amount output)))) 0))
    (hash-remove! needed (component-chemical (reaction-out rx))))
  needed)
    