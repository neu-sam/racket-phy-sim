;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname main) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;; Vector ;;;;;;;

(define-struct vector [x y])
; A Vector is a (make-vector Number Number)
; - x: The x value of the vector
; - y: The y value of the vector
; Interpretation: A 2D vector.
(define VEC-0 (make-vector 0 0))
(define VEC-EXAMPLE-1 (make-vector 10 20))
(define VEC-EXAMPLE-2 (make-vector 5 12))
(define VEC-EXAMPLE-3 (make-vector 10 -5))
(define VEC-EXAMPLE-4 (make-vector -5 4))
(define (vec-temp vec)
  (... (vector-x vec) ... (vector-y vec) ...))

; vector-add: Vector Vector -> Vector
; Add two vectors
(check-expect (vector-add VEC-EXAMPLE-1 VEC-EXAMPLE-2) (make-vector 15 32))
(check-expect (vector-add VEC-EXAMPLE-3 VEC-EXAMPLE-4) (make-vector 5 -1))
(define (vector-add vec-1 vec-2)
  (make-vector (+ (vector-x vec-1) (vector-x vec-2))
               (+ (vector-y vec-1) (vector-y vec-2))))

; vector-sub: Vector Vector -> Vector
; Subtract two vectors
(check-expect (vector-sub VEC-EXAMPLE-1 VEC-EXAMPLE-2) (make-vector 5 8))
(check-expect (vector-sub VEC-EXAMPLE-3 VEC-EXAMPLE-4) (make-vector 15 -9))
(define (vector-sub vec-1 vec-2)
  (make-vector (- (vector-x vec-1) (vector-x vec-2))
               (- (vector-y vec-1) (vector-y vec-2))))

(check-expect (vector-mul VEC-0 10) VEC-0)
(check-expect (vector-mul VEC-EXAMPLE-2 2) (make-vector 10 24))
(define (vector-mul vec scaler)
  (make-vector (* (vector-x vec) scaler) (* (vector-y vec) scaler)))

; vector-div: Vector Number -> Vector
; Divide the vector by a scaler.
(check-expect (vector-div VEC-0 3) VEC-0)
(check-expect (vector-div VEC-EXAMPLE-1 10) (make-vector 1 2))
(check-error (vector-div VEC-EXAMPLE-1 0))
(define (vector-div vec scaler)
  (make-vector (/ (vector-x vec) scaler) (/ (vector-y vec) scaler)))

; vector-dot: Vector Vector -> Number
; Dot product of two vectors
(check-expect (vector-dot VEC-EXAMPLE-1 VEC-EXAMPLE-2) 290)
(check-expect (vector-dot VEC-EXAMPLE-3 VEC-EXAMPLE-4) -70)
(define (vector-dot vec-1 vec-2)
  (+ (* (vector-x vec-1) (vector-x vec-2)) (* (vector-y vec-1) (vector-y vec-2))))

; vector-length: Vector -> Number
; The length of the vector
(check-expect (vector-length (make-vector 3 4)) 5)
(check-expect (vector-length (make-vector 1 0)) 1)
(check-expect (vector-length (make-vector 0 0)) 0)
(define (vector-length vec)
  (sqrt (vector-dot vec vec)))

; dist-to: Vector Vector -> Number
; The distance from one vector to the other
(check-expect (dist-to (make-vector 3 3) (make-vector 6 7)) 5)
(check-expect (dist-to (make-vector 1 0) (make-vector 0 0)) 1)
(define (dist-to vec1 vec2)
  (sqrt (+ (sqr (- (vector-x vec1) (vector-x vec2))) (sqr (- (vector-y vec1) (vector-y vec2))))))

; vector=?: Vector Vector -> Boolean
; Check if two vectors are equal to each other.
(check-expect (vector=? VEC-EXAMPLE-1 VEC-EXAMPLE-1) #t)
(check-expect (vector=? VEC-EXAMPLE-3 VEC-EXAMPLE-3) #t)
(check-expect (vector=? VEC-EXAMPLE-3 VEC-EXAMPLE-2) #f)
(define (vector=? vec1 vec2)
  (and (= (vector-x vec1) (vector-x vec2))
       (= (vector-y vec1) (vector-y vec2))))

; vector-normalize: Vector -> Vector
; Returns the normalized vector.
(check-expect (vector-normalize VEC-0) VEC-0)
(define (vector-normalize vec)
  (if (vector=? vec VEC-0) VEC-0 (vector-div vec (vector-length vec))))

;;;;;;; Circle ;;;;;;;
(define-struct po-circle (pos vel rad color))
; A Circle is a (make-circle Vector Vector Number Color)
; - pos: The position of the circle
; - vel The velocity of the circle
; - rad The radius of the circle
; - color: The color of the circle
(define CIRCLE-0 (make-po-circle (make-vector 0 0) (make-vector 0 0) 5 "red"))
(define CIRCLE-1 (make-po-circle (make-vector 320 600) (make-vector 1 1) 30 "green"))
(define CIRCLE-2 (make-po-circle (make-vector 10 100) (make-vector 3 10) 90 "blue"))
(define CIRCLE-3 (make-po-circle (make-vector 100 90) (make-vector 20 5) 90 "purple"))
(define CIRCLE-4 (make-po-circle (make-vector 1290 600) (make-vector 1 1) 30 "green"))
(define CIRCLE-5 (make-po-circle (make-vector 1290 700) (make-vector 1 1) 30 "green"))
(define (po-circle-temp circ)
  (... (vec-temp (circle-pos circ)) ... (circle-vel circ) ...
       (circle-rad circ) ... (circle-color circ) ...))

; draw-circle: Circle -> Image
; Draw a circle.
(check-expect (draw-circle CIRCLE-0)
              (circle 5 "solid" "red"))
(check-expect (draw-circle CIRCLE-1)
              (circle 30 "solid" "green"))
(check-expect (draw-circle CIRCLE-2)
              (circle 90 "solid" "blue"))
(define (draw-circle circ)
  (circle (po-circle-rad circ) "solid" (po-circle-color circ)))

; draw-circles: [ListOF Circle] -> Image
; draw function
(check-expect (draw-circles (list CIRCLE-0 CIRCLE-1) (rectangle 1270 720 "solid" "gray"))
              (put-image (draw-circle CIRCLE-0)
                         (vector-x (po-circle-pos CIRCLE-0))
                         (vector-y (po-circle-pos CIRCLE-0))
                         (put-image (draw-circle CIRCLE-1)
                                    (vector-x (po-circle-pos CIRCLE-1))
                                    (vector-y (po-circle-pos CIRCLE-1))
                                    (rectangle 1270 720 "solid" "gray"))))
(check-expect (draw-circles (list CIRCLE-1 CIRCLE-2 CIRCLE-3) (rectangle 1270 720 "solid" "gray"))
              (put-image (draw-circle CIRCLE-1)
                         (vector-x (po-circle-pos CIRCLE-1))
                         (vector-y (po-circle-pos CIRCLE-1))
                         (put-image (draw-circle CIRCLE-2)
                                    (vector-x (po-circle-pos CIRCLE-2))
                                    (vector-y (po-circle-pos CIRCLE-2))
                                    (put-image (draw-circle CIRCLE-3)
                                               (vector-x (po-circle-pos CIRCLE-3))
                                               (vector-y (po-circle-pos CIRCLE-3))
                                               (rectangle 1270 720 "solid" "gray")))))
(define (draw-circles loc bg)
  (foldr (lambda (c scene)
           (put-image (draw-circle c) (vector-x (po-circle-pos c)) (vector-y (po-circle-pos c)) scene))
         bg loc))

; get-overlap: Circle Circle -> Number
; Get the overlaped length of two circles
(check-within (get-overlap CIRCLE-0 CIRCLE-1) 0.0 0.001)
(check-expect (= (get-overlap CIRCLE-2 CIRCLE-3) 0) #f)
(define (get-overlap c1 c2)
  (max (- (+ (po-circle-rad c1) (po-circle-rad c2)) (dist-to (po-circle-pos c1) (po-circle-pos c2))) 0))

(define LOC-0 '())
(define LOC-1 (list CIRCLE-1))
(define LOC-2 (list CIRCLE-1 CIRCLE-2 CIRCLE-3))

(define WINDOW-WIDTH 1270)
(define WINDOW-HEIGHT 720)
(define BG (rectangle WINDOW-WIDTH WINDOW-HEIGHT "solid" "gray"))

(define GRAVITY (make-vector 0 -0.25))
(define RESIST 0.995)

; circle-collide: Circle Circle -> Vector
; The displacement of the two circles.
; The first value of the return is the updated velocity for the first circle.
; The second value of the return is the updated velocity for the second circle.
(check-expect (circle-collide CIRCLE-0 CIRCLE-0) VEC-0)
(check-expect (circle-collide CIRCLE-0 CIRCLE-1) VEC-0)
(define (circle-collide c1 c2)
  (local [(define displace (/ (get-overlap c1 c2) 2))]
    (if (> displace 0)
        (vector-mul (vector-normalize (vector-sub (po-circle-pos c1) (po-circle-pos c2))) displace)
        VEC-0)))

; limit-circle-x: Vector Number -> Number
; Return the clamped x value of the position of the circle
(check-expect (limit-circle-x (po-circle-pos CIRCLE-0) (po-circle-rad CIRCLE-0)) 5)
(check-expect (limit-circle-x (po-circle-pos CIRCLE-1) (po-circle-rad CIRCLE-1)) 320)
(check-expect (limit-circle-x (po-circle-pos CIRCLE-4) (po-circle-rad CIRCLE-4)) 1240)
(define (limit-circle-x pos rad)
  (if (> (+ (vector-x pos) rad) WINDOW-WIDTH)
      (- WINDOW-WIDTH rad)
      (if (< (- (vector-x pos) rad) 0)
          rad
          (vector-x pos))))

; limit-circle-y: Vector Number -> Number
; Return the clamped y value of the position of the circle
(check-expect (limit-circle-y (po-circle-pos CIRCLE-0) (po-circle-rad CIRCLE-0)) 5)
(check-expect (limit-circle-y (po-circle-pos CIRCLE-1) (po-circle-rad CIRCLE-1)) 600)
(check-expect (limit-circle-y (po-circle-pos CIRCLE-5) (po-circle-rad CIRCLE-5)) 690)
(define (limit-circle-y pos rad)
  (if (> (+ (vector-y pos) rad) WINDOW-HEIGHT)
      (- WINDOW-HEIGHT rad)
      (if (< (- (vector-y pos) rad) 0)
          rad
          (vector-y pos))))

; limit-circle-pos: Circle -> Vector
; Return the clamped position of the circle
(check-expect (limit-circle-pos (po-circle-pos CIRCLE-0) (po-circle-rad CIRCLE-0)) (make-vector 5 5))
(check-expect (limit-circle-pos (po-circle-pos CIRCLE-1) (po-circle-rad CIRCLE-1)) (make-vector 320 600))
(define (limit-circle-pos pos rad)
  (make-vector (limit-circle-x pos rad) (limit-circle-y pos rad)))

; update-circle: Circle [ListOf Circle] -> [ListOf Circle]
; Update each circle's state
(define (update-circle c loc)
  (local [(define F-VEL (vector-add (po-circle-vel c) GRAVITY)) 
          (define F-POS (vector-add (limit-circle-pos (vector-add (po-circle-pos c)
                                                       F-VEL)
                                           (po-circle-rad c))
                                    (foldr (lambda (c2 v) (vector-add v (circle-collide c c2))) VEC-0 loc)))]
    (make-po-circle F-POS
                    (vector-mul (vector-sub F-POS (po-circle-pos c)) RESIST)
                    (po-circle-rad c)
                    (po-circle-color c))))

(define (main init-state)
  (big-bang init-state
    [on-draw draw-func]
    [on-tick tick-func]
    [on-mouse mouse-func]))

; draw-func: [ListOf Circles] -> Image
; the draw function for big bang
(define (draw-func loc)
  (draw-circles loc BG))

; tick-func: [ListOf Circles] -> [ListOf Circles]
; the tick function for big bang
(define (tick-func loc)
  (local [(define FIRST-ITER (map (lambda (c) (update-circle c loc)) loc))
          (define SECOND-ITER (map (lambda (c) (update-circle c FIRST-ITER)) FIRST-ITER))]
  SECOND-ITER))

; mouse-func [ListOf Circles] Number Number MouseEvent -> [ListOf Circles]
; FIRE!!!!!!!!!!!!!!!!!!
(define (mouse-func loc x y event)
  (local [(define FIRE-POINT (make-vector (/ WINDOW-WIDTH 2) (- WINDOW-HEIGHT 50)))]
    (if (mouse=? event "button-down")
        (cons (make-po-circle
               FIRE-POINT
               (vector-mul (vector-normalize (vector-sub (make-vector x (- WINDOW-HEIGHT y)) FIRE-POINT)) 30)
               (+ 10 (random 10)) (make-color (random 255) (random 255) (random 255)))
              loc)
        loc)))

(define CIRCLE-6 (make-po-circle (make-vector 0 0) (make-vector 0 0) 5 "red"))
(define CIRCLE-7 (make-po-circle (make-vector 320 600) (make-vector 1 1) 10 "green"))
(define CIRCLE-8 (make-po-circle (make-vector 10 100) (make-vector 3 10) 9 "blue"))
(define CIRCLE-9 (make-po-circle (make-vector 100 90) (make-vector 20 5) 2 "purple"))
(define CIRCLE-10 (make-po-circle (make-vector 1290 600) (make-vector 1 1) 3 "green"))
(define CIRCLE-11 (make-po-circle (make-vector 1290 700) (make-vector 1 1) 6 "green"))
(define INIT-STATE (list CIRCLE-6 CIRCLE-7 CIRCLE-8 CIRCLE-9 CIRCLE-10 CIRCLE-11))

(main INIT-STATE)



