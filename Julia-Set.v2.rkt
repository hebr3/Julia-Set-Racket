#lang typed/racket
(require math/flonum
         images/flomap)

;; CONSTANTS
(define WIDTH 1000)
(define HEIGHT WIDTH)
(define DEPTH 64)
(define SCALE (/ WIDTH))

;; PROCEDURES
(: julia (-> Float-Complex Float-Complex Natural))
(define (julia z c)
  (: iter (-> Float-Complex Natural Natural))
  (define (iter zn count)
    (cond [(> (magnitude zn) 2) count]
          [(> count DEPTH) DEPTH]
          [else (iter (+ (sqr zn) c) (add1 count))]))
  (iter z 0))

(: julia-vector (-> Integer Integer Float-Complex
                    (Vector Real Real Real Real)))
(define (julia-vector x y z)
  (: scale (-> Integer Float))
  (define (scale n)
    (- (* 3.0 SCALE (fl n)) 1.5))
  (let ([j (julia (make-flrectangular (- (scale x)) (scale y)) z)])
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
