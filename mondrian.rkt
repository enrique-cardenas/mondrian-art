#lang slideshow

(define (boxedRect w h)
 (cc-superimpose (colorize (filled-rectangle w h) "white") (rectangle w h)))

(define (coloredBox w h r)
  (cond
    [(< r 0.0833) (cc-superimpose (colorize (filled-rectangle w h) "red") (rectangle w h))]
    [(< r 0.1667) (cc-superimpose (colorize (filled-rectangle w h) "blue") (rectangle w h))]
    [(< r 0.25) (cc-superimpose (colorize (filled-rectangle w h) "yellow") (rectangle w h))]
    [else (cc-superimpose (colorize (filled-rectangle w h) "white") (rectangle w h))]
    )
  )

(define (randoms seed len)
  (if (zero? len) empty
      (cons (/ seed 4294967296)
            (randoms (modulo (+ (* seed 1664525) 1013904223) 4294967296) (- len 1))))
)

(define (splitter seed)
  (cond
    [(and (> seed (/ 1 3)) (< seed (/ 2 3))) seed]
    [(< seed (/ 1 3)) (splitter (* seed 2))]
    [(> seed (/ 2 3)) (splitter (/ seed 2))]
    )
  )
      

(define (mondrian width height rands)
  (letrec (
           (wide-enough (lambda (w r) (and (> w 50) (> (/ w width) (* 0.5 r)))))
           (tall-enough (lambda (h r) (and (> h 50) (> (/ h height) (* 0.5 r )))))
           (big-enough (lambda (w h r) (and (wide-enough w r) (tall-enough h r))))
           (split-four (lambda (w h r)
                         (let* (
                                (w1 (* w (splitter (first r))))
                                (h1 (* h (splitter (second r))))
                                (result1 (mondrian-helper w1 h1 (rest (rest r))))
                                (result2 (mondrian-helper (- w w1) h1 (first (rest result1))))
                                (result3 (mondrian-helper w1 (- h h1) (first (rest result2))))
                                (result4 (mondrian-helper (- w w1) (- h h1) (first (rest result3))))
                                )
                          (list
                                 (vc-append
                           (hc-append (first result1) (first result2))
                           (hc-append (first result3) (first result4)))
                                 
                                (second result4))
                           )))

           (split-width (lambda (w h r)
                          (let* (
                                 (w1 (* w (splitter (first r))))
                                 (result1 (mondrian-helper w1 h (rest r)))
                                 (result2 (mondrian-helper (- w w1) h (first (rest result1))))
                                          )
                            (list (hc-append (first result1) (first result2)) (second result2)))))
           
           (split-height (lambda (w h r)
                          (let* (
                                 (h1 (* h (splitter (first r))))
                                 (result1 (mondrian-helper w h1 (rest r)))
                                 (result2 (mondrian-helper w (- h h1) (first (rest result1))))
                                 )
                            (list (vc-append (first result1) (first result2)) (second result2)))))

                                          
           (mondrian-helper (lambda (w h r)
                              (cond
                                [(and (big-enough w h (first r))) (split-four w h (rest r))]
                                [(wide-enough w (first r)) (split-width w h (rest r))]
                                [(tall-enough h (first r)) (split-height w h (rest r))]
                                [else (list (coloredBox w h (second r)) (rest r))]
                                ))))
   (mondrian-helper width height rands))
)



(mondrian 350 300 '(.2 0.8 0.6 0.24 1 0.5 1 0.24 0.16 0.2 0.24 0.6 1 0.5 1 .22 0.6 10 0.8 0.6 1 0.24 0.5 0.24 1 0.6 2 .02 .2 .01 2 0.16 .02 .05 0.24 0.8 0.6 0.164 1 0.5 1 .20 0.08 1 0.08 0.6 10 .20 0.8 0.6 0.21 1 0.5 1 0.08 0.2 0.6 0.16 0.8 0.21 0.6 1 0.5 1 0.08 1 0.08 0.164 0.6 10 0.8 0.6 0.164 1 0.5 1 .21 0.08 0.164 0.6 0.16 .24 .22 0.6 0.164 1 0.5 1 0.08))