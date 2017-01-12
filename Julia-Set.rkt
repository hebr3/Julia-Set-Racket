;;;;;;;;;1;;;;;;;;;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7;;
#lang typed/racket
(require math/flonum
         images/flomap
         typed/racket/draw)

;; CONSTANTS
(define WIDTH 500)
(define HEIGHT WIDTH)
(define DEPTH 64)
(define SCALE (fl (/ 1 WIDTH)))

;; PROCEDURES
(: julia (-> Float-Complex Float-Complex Natural))
(define (julia z c)
  (: iter (-> Float-Complex Natural Natural))
  (define (iter zn count)
    (cond [(> (magnitude zn) 2.0) count]
          [(> count DEPTH) DEPTH]
          [else (iter (+ (sqr zn) c) (add1 count))]))
  (iter z 0))

(: julia-vector (-> Integer Integer Float-Complex
                    (Vector Real Real Real Real)))
(define (julia-vector x y z)
  (: scale (-> Integer Float))
  (define (scale n)
    (- (* 3.0 SCALE (fl n)) 1.5))
  (let ([j (julia (make-flrectangular (scale x) (scale y)) z)])
    (vector (if (= j DEPTH) 1.0 (fl (- 1 (/ j DEPTH))))
            (if (= j DEPTH) 0.0 (fl (/ j DEPTH)))
            0.0
            (if (= j DEPTH) 0.0 (fl (- 1 (/ j DEPTH)))))))

(: julia-img (-> Float-Complex flomap))
(define (julia-img z)
  (build-flomap* 4 WIDTH HEIGHT
                 (λ (x y)
                   (julia-vector x y z))))

(: julia-fm flomap)
(define julia-fm (time (julia-img -0.8+0.156i)))
;(time (send (flomap->bitmap (flomap-scale julia-fm 1/4))
;             save-file "julia.png" 'png))
(time (flomap->bitmap julia-fm))



(: scene (Listof (List Integer Integer)))
(define scene
  (for*/list : (Listof (List Integer Integer))
    ([x (range 4)] [y (range 3)])
    (list x y)))

(: scale-scene (-> (List Integer Integer) (List Float Float)))
(define (scale-scene x)
  (list (fl (- (* SCALE (first x)) 2.0))
        (fl (- (* SCALE (second x)) 1.0))))

(: scaled-scene (Listof (List Float Float)))
(define scaled-scene
  (map scale-scene scene))
