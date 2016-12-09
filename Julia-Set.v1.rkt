#lang typed/racket
(require math/flonum
         images/flomap)

;; CONSTANTS
(define WIDTH 400)
(define HEIGHT WIDTH)
(define DEPTH 64)
(define SCALE (expt 0.5 (- (/ (log WIDTH) (log 0.5)))))

;; PROCEDURES
(: julia (-> Float-Complex Float-Complex Natural))
(define (julia z c)
  (: iter (-> Float-Complex Natural Natural))
  (define (iter zn count)
    (cond [(> (magnitude zn) 2) count]
          [(> count DEPTH) DEPTH]
          [else (iter (+ (sqr zn) c) (add1 count))]))
  (iter z 0))

(: julia-vector (-> Integer Integer Float-Complex (Vector Real Real Real Real)))
(define (julia-vector x y z)
  (let* ([r (fl (- (* 3 SCALE x) 1.5))]
         [c (fl (- (* 3 SCALE y) 1.5))]
         [j (julia (make-flrectangular r c) z)])
    (vector (if (= j DEPTH) 1.0 (fl (- 1 (/ j DEPTH))))
            (if (= j DEPTH) 0.0 (fl (/ j DEPTH)))
            0.0
            (if (= j DEPTH) 0.0 (fl (- 1 (/ j DEPTH)))))))

(: julia-img (-> Float-Complex flomap))
(define (julia-img z)
  (build-flomap* 4 WIDTH HEIGHT
                 (λ (x y)
                   (julia-vector x y z))))

(define julia-fm (julia-img -0.8+0.156i))
(flomap->bitmap julia-fm)
