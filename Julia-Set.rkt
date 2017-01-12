#lang typed/racket
(require math/flonum
         images/flomap
         typed/racket/draw)

(require/typed future-visualizer
               [visualize-futures-thunk (All (A) ((-> A) -> A))])
(define-syntax-rule (visualize-futures e ...)
  (visualize-futures-thunk (lambda () e ...)))

;; CONSTANTS
(define WIDTH 700)
(define HEIGHT WIDTH)
(define DEPTH 100)
(define SCALE (fl (/ 3 WIDTH)))

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

;(: julia-fm flomap)
;(define julia-fm (time (julia-img -0.8+0.156i)))
;(time (send (flomap->bitmap (flomap-scale julia-fm 1/4))
;             save-file "julia.png" 'png))
;(time (flomap->bitmap julia-fm))

(: scene (Listof (List Integer Integer)))
(define scene
  (for*/list : (Listof (List Integer Integer))
    ([y (range HEIGHT)] [x (range WIDTH)])
    (list x y)))

(: scale-scene (-> (List Integer Integer) (List Float Float)))
(define (scale-scene x)
  (list (fl (fl- (fl* (fl SCALE) (->fl (first x))) 1.5))
        (fl (fl+ (- (fl* (fl SCALE) (->fl (second x)))) 1.5))))

(: scaled-scene (Listof (List Float Float)))
(define scaled-scene
  (map scale-scene scene))

(: mandelbrot2 (-> (List Float Float) Integer))
(define (mandelbrot2 start)
  (let ([sr (first start)] [si (second start)])
    (: iter (-> (List Float Float) Integer Integer))
    (define (iter z count)
      (let ([zr (first z)] [zi (second z)])
        (cond
          [(fl> (fl+ (sqr zr) (sqr zi)) 4.0) count]
          [(> count DEPTH) DEPTH]
          [else (iter (list (fl+ (fl- (sqr zr) (sqr zi)) sr)
                            (fl+ (fl* 2.0 (fl* zr zi)) si))
                      (add1 count))])))
    (iter start 0)))

(: julia2 (-> (List Float Float) (List Float Float) Integer))
(define (julia2 start pt)
  (let ([pt-r (first pt)] [pt-i (second pt)])
    (: iter (-> (List Float Float) Integer Integer))
    (define (iter z count)
      (let ([zr (first z)] [zi (second z)])
        (cond
          [(fl> (fl+ (fl* zr zr) (fl* zi zi)) 4.0) count]
          [(> count DEPTH) DEPTH]
          [else (iter (list (fl+ (fl- (fl* zr zr) (fl* zi zi)) pt-r)
                            (fl+ (fl* 2.0 (fl* zr zi)) pt-i))
                      (add1 count))])))
    (iter start 0)))


(define value-scene
  (time (map mandelbrot2 scaled-scene)))
(define value-scene3
  (time (map (λ ([x : (List Float Float)])
               (julia2 x (list (fl -0.8) (fl 0.156))))
             scaled-scene)))

(: future-julia2 (-> (List Float Float) (Futureof Integer)))
(define (future-julia2 start)
  (future (λ () (julia2 start (list (fl -0.8) (fl 0.156))))))

(define future-value-scene
  (time (map future-julia2 scaled-scene)))

(define value-scene2
  (time (map (λ ([x : (Futureof Integer)]) (touch x))
             future-value-scene)))

(equal? value-scene3 value-scene2)

(define normalized-value-scene
  (map (λ ([x : Integer]) (fl (- 1 (/ x DEPTH)))) value-scene))
(define normalized-value-scene2
  (map (λ ([x : Integer]) (fl (- 1 (/ x DEPTH)))) value-scene3))

(define scene-fm
  (flomap (list->flvector normalized-value-scene) 1 WIDTH HEIGHT))
(define scene2-fm
  (flomap (list->flvector normalized-value-scene2) 1 WIDTH HEIGHT))

(flomap->bitmap scene-fm)
(flomap->bitmap scene2-fm)


;(send bt save-file "julia.png" 'png)